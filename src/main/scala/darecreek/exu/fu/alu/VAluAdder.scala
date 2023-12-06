/**
  * Perform below instructions:
  *     11.1  vadd, ...
  *     11.2  vwadd, ...
  *     11.4  vadc, vmadc, ...
  *     11.8  vmseq, vmsltu, ...
  *     11.9  vminu, ...
  *     Part of 12.1
  *     Part of 12.2
  */
package darecreek.exu.fu.alu

import chisel3._
import chisel3.util._
import darecreek.DarecreekParam._
import darecreek.SewOH

class VAluAdder extends Module {
  val io = IO(new Bundle {
    val in = Input(new VAluInput)
    val out = Output(new Bundle {
      val vd = UInt(LaneWidth.W)
      val cmp = UInt((LaneWidth/8).W)  // compare result OR carry-out of vmadc...
    })
    val toFixP = Output(new AdderToFixP)
  })

  val uop = io.in.uop
  val funct6 = uop.ctrl.funct6
  val sew = io.in.sew
  val (vs1, vs2, vmask) = (io.in.vs1_rs1_imm, io.in.vs2, io.in.vmask)
  val (widen, widen2) = (uop.ctrl.widen, uop.ctrl.widen2)
  val vm = uop.ctrl.vm
  val rsub = funct6 === "b000011".U
  val sub = io.in.ctrl.sub || rsub
  val addWithCarry = funct6(5,2) === "b0100".U
  val signed = funct6(0)

  /**
    * Input adjust: widen & subtract
    */
  // Widen vs1 & vs2
  def widenPad(x: UInt) = {
    val len = x.getWidth
    Cat(Fill(len, x(len-1) && signed), x)
  }
  val vs = Seq(vs1, vs2)
  val vs_widen = Wire(Vec(2, UInt(64.W)))
  val widenCase = Seq({widen || widen2}, widen)
  for (i <- 0 until 2) {
    val vs_32b = Mux(uop.expdIdx(0), vs(i)(63, 32), vs(i)(31, 0))
    when (widenCase(i)) {
      vs_widen(i) := Mux1H(Seq(
        sew.is8  -> Cat(widenPad(vs_32b(31, 24)), widenPad(vs_32b(23, 16)), widenPad(vs_32b(15, 8)), widenPad(vs_32b(7, 0))),
        sew.is16 -> Cat(widenPad(vs_32b(31, 16)), widenPad(vs_32b(15, 0))),
        sew.is32 -> widenPad(vs_32b(31, 0))
      ))
    }.otherwise {
      vs_widen(i) := vs(i)
    }
  }
  // Subtract: bit negate
  val vs1_adjust = vs_widen(0) ^ Fill(64, io.in.ctrl.sub)
  val vs2_adjust = vs_widen(1) ^ Fill(64, rsub)

  /**
    * Chain all eight 8bit-adders
    */
  class Adder_8b(in1: UInt, in2: UInt, cin: UInt) {
    private val bits = Cat(0.U(1.W), in1, cin) +
                       Cat(0.U(1.W), in2, cin)
    val (cout, out) = (bits(9), bits(8, 1))
  }
  val cin  = Wire(Vec(8, Bool()))
  val cout = Wire(Vec(8, Bool()))
  val vd   = Wire(Vec(8, UInt(8.W)))
  // val destEew_oneHot = Seq.tabulate(4)(i => uop.info.destEew === i.U)
  val carryIn = Wire(Vec(8, Bool()))
  // Adjust vmask. sew==32: 00000011 -> 00010001   sew==16: 00001111 -> 01010101
  // val vmask_adjust = MuxCase(vmask, Seq(
  //   sew.is16 -> Cat(false.B, vmask(3), false.B, vmask(2), false.B, vmask(1), false.B, vmask(0)),
  //   sew.is32 -> Cat(0.U(3.W), vmask(1), 0.U(3.W), vmask(0))
  // ))

  for (i <- 0 until 8) {
    val adder_8b = new Adder_8b(vs1_adjust(8*i+7, 8*i), vs2_adjust(8*i+7, 8*i), cin(i))
    // Generate carry-in from sub and vmask(11.4 Add-with-Carry/Sub-with_Borrow)
    // carryIn(i) := Mux(addWithCarry, Mux(vm, sub, vmask_adjust(i) ^ sub), sub)
    carryIn(i) := Mux(addWithCarry, Mux(vm, sub, vmask(i) ^ sub), sub)
    // Generate final carry-in: cin
    val eewCin = SewOH(uop.info.destEew)
    if (i == 0) {
      cin(i) := carryIn(i)
    } else if (i == 4) {
      cin(i) := Mux(eewCin.is64, cout(i-1), carryIn(i))
    } else if (i % 2 == 0) {
      cin(i) := Mux(eewCin.is64 || eewCin.is32, cout(i-1), carryIn(i))
    } else {
      cin(i) := Mux(eewCin.is8, carryIn(i), cout(i-1))
    }
    // cin(i) := Mux1H(destEew_oneHot, Seq(1, 2, 4, 8).map(n => 
    //   if ((i % n) == 0) carryIn(i) else cout(i-1))
    // )
    cout(i) := adder_8b.cout
    vd(i) := adder_8b.out
  }

  /**
    * Integer Compare & Min/Max instructions
    */
  val lessThan_vec = Wire(Vec(8, Bool()))
  val equal_vec = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    lessThan_vec(i) := Mux(signed, (vs2(8*i+7) ^ vs1_adjust(8*i+7)) ^ cout(i), !cout(i))
    equal_vec(i) := vs2(8*i+7, 8*i) === vs1(8*i+7, 8*i)
  }
  val equal = Cat(equal_vec.reverse)
  val cmpEq = Mux1H(Seq(
    sew.is8  -> equal,
    sew.is16 -> Cat(Fill(2, equal(7, 6).andR), Fill(2, equal(5, 4).andR), Fill(2, equal(3, 2).andR), Fill(2, equal(1, 0).andR)),
    sew.is32 -> Cat(Fill(4, equal(7, 4).andR), Fill(4, equal(3, 0).andR)),
    sew.is64 -> Fill(8, equal.andR)
  ))
  val cmpNe = ~cmpEq
  val lessThan = Cat(lessThan_vec.reverse)
  val cmpResult = Mux1H(Seq(
    (funct6 === "b011000".U) -> cmpEq,
    (funct6 === "b011001".U) -> cmpNe,
    (funct6(5,1) === "b01101".U) -> lessThan,
    (funct6(5,1) === "b01110".U) -> (lessThan | cmpEq),
    (funct6(5,1) === "b01111".U) -> ~(lessThan | cmpEq)
  ))

  //-------- Min/Max --------
  val minMaxResult = Wire(Vec(8, UInt(8.W)))
  val selectVs1 = lessThan_vec.map(_ === funct6(1))
  for (i <- 0 until 8) {
    val sel = Mux1H(Seq(
      sew.is8  -> selectVs1(i),
      sew.is16 -> selectVs1((i/2)*2+1),
      sew.is32 -> selectVs1((i/4)*4+3),
      sew.is64 -> selectVs1(7),
    ))
    minMaxResult(i) := Mux(sel, vs1(8*i+7, 8*i), vs2(8*i+7, 8*i))
  }

  io.out.vd := Mux(funct6(5, 2) === "b0001".U, Cat(minMaxResult.reverse), Cat(vd.reverse))

  val cmpOut = Mux(addWithCarry, Mux(io.in.ctrl.sub, ~(cout.asUInt), cout.asUInt), cmpResult)
  io.out.cmp := Mux1H(Seq(
    sew.is8  -> cmpOut,
    sew.is16 -> Cat(~(0.U(4.W)), cmpOut(7), cmpOut(5), cmpOut(3), cmpOut(1)),
    sew.is32 -> Cat(~(0.U(6.W)), cmpOut(7), cmpOut(3)),
    sew.is64 -> Cat(~(0.U(7.W)), cmpOut(7))
  ))

  //---- To Fixed-Point unit ----
  for (i <- 0 until 8) {
    io.toFixP.vs2H(i) := vs2(8*i+7)
    io.toFixP.vs1H(i) := vs1(8*i+7)
    io.toFixP.vd(i) := vd(i)
    io.toFixP.cout(i) := cout(i)
  }
}
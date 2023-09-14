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
package darecreek.exu.vfu.alu

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._

class VIntAdder64b extends Module {
  val io = IO(new Bundle {
    val funct6 = Input(UInt(6.W))
    val vm = Input(Bool())
    val ma = Input(Bool())
    val sew = Input(new SewOH)
    val eewVd = Input(new SewOH) // Exclude narrow_to_1
    val uopIdx = Input(UInt(3.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val oldVd = Input(UInt(8.W))
    val vmask = Input(UInt(8.W))
    val isSub = Input(Bool())  // subtract (exclude vrsub)
    val widen = Input(Bool())  // SEW op SEW
    val widen2 = Input(Bool()) // 2*SEW op SEW
    val narrow_to_1 = Input(Bool())

    val vd = Output(UInt(64.W))
    val cmpOut = Output(UInt(8.W)) // compare or add-with-carry carry output
    val toFixP = Output(new AdderToFixP)
  })

  val funct6 = io.funct6
  val sew = io.sew
  val uopIdx = io.uopIdx
  val vs1 = io.vs1
  val vs2 = io.vs2
  val vmask = io.vmask
  val rsub = funct6 === "b000011".U
  val sub = io.isSub || rsub
  val signed = funct6(0)
  val addWithCarry = funct6(5,2) === "b0100".U


  // Widen vs1 & vs2
  val vs2_32b = Mux(uopIdx(0), vs2(63, 32), vs2(31, 0))
  val vs2Widen = Mux1H(sew.oneHot(2, 0), Seq(8, 16, 32).map(sew =>
                           Cat(UIntSplit(vs2_32b, sew).map(BitsExtend(_, 2*sew, signed)).reverse)))
  val vs1_32b = Mux(uopIdx(0), vs1(63, 32), vs1(31, 0))
  val vs1Widen = Mux1H(sew.oneHot(2, 0), Seq(8, 16, 32).map(sew =>
                           Cat(UIntSplit(vs1_32b, sew).map(BitsExtend(_, 2*sew, signed)).reverse)))
  // Subtract: bit negate
  val vs1_adjust = Mux(io.widen || io.widen2, vs1Widen, vs1) ^ Fill(64, io.isSub)
  val vs2_adjust = Mux(io.widen, vs2Widen, vs2 ^ Fill(64, rsub))

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
  val vmask_adjust = MuxCase(vmask, Seq(
    sew.is16 -> Cat(false.B, vmask(3), false.B, vmask(2), false.B, vmask(1), false.B, vmask(0)),
    sew.is32 -> Cat(0.U(3.W), vmask(1), 0.U(3.W), vmask(0))
  ))

  for (i <- 0 until 8) {
    val adder_8b = new Adder_8b(vs1_adjust(8*i+7, 8*i), vs2_adjust(8*i+7, 8*i), cin(i))
    // Generate carry-in from sub and vmask(11.4 Add-with-Carry/Sub-with_Borrow)
    carryIn(i) := Mux(addWithCarry, Mux(io.vm, sub, vmask_adjust(i) ^ sub), sub)
    // Generate final carry-in: cin
    val eewCin = Wire(new SewOH)
    eewCin.oneHot := Mux(io.narrow_to_1, sew.oneHot, io.eewVd.oneHot)

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

  io.vd := Mux(funct6(5, 2) === "b0001".U, Cat(minMaxResult.reverse), Cat(vd.reverse))

  val cmpOut = Mux(addWithCarry, Mux(io.isSub, ~(cout.asUInt), cout.asUInt), cmpResult)
  val cmpOutAdjust = Mux1H(Seq(
    sew.is8  -> cmpOut,
    sew.is16 -> Cat(~(0.U(4.W)), cmpOut(7), cmpOut(5), cmpOut(3), cmpOut(1)),
    sew.is32 -> Cat(~(0.U(6.W)), cmpOut(7), cmpOut(3)),
    sew.is64 -> Cat(~(0.U(7.W)), cmpOut(7))
  ))
  io.cmpOut := Mux(addWithCarry, cmpOutAdjust,
    Cat(Seq.tabulate(8)(i => Mux(!io.vm && !vmask(i), Mux(io.ma, true.B, io.oldVd(i)), cmpOutAdjust(i))).reverse))

  //---- To Fixed-Point unit ----
  for (i <- 0 until 8) {
    io.toFixP.vs2H(i) := vs2(8*i+7)
    io.toFixP.vs1H(i) := vs1(8*i+7)
    io.toFixP.vd(i) := vd(i)
    io.toFixP.cout(i) := cout(i)
  }
}

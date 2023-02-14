/**
  * Fixed-Point instructions:
  *     12.1  vsadd, ...
  *     12.2  vaadd, ...
  *     12.4  vssrl, ...
  *     12.5  vnclip, ...
  */
package vfu.alu

import chisel3._
import chisel3.util._
import vfu.{VIFuInfo, SewOH, UIntSplit}
import vfu.alu.VAluOpcode._

class AdderToFixP extends Bundle {
  val cout = Vec(8, Bool())
  val vd = Vec(8, UInt(8.W))
  val vs2H = Vec(8, Bool()) // Highest bit of one 8-bit element of vs2
  val vs1H = Vec(8, Bool()) // Highest bit of one 8-bit element of vs1_adjust
}
class MiscToFixP extends Bundle {
  val shiftOut = UInt(64.W)
  val rnd_high = UInt(8.W) // 8 * (v[d-1])
  val rnd_tail = UInt(8.W) // 8 * (v[d-2:0] == 0)
}

class VFixPoint64b extends Module {
  val io = IO(new Bundle {
    // val uop = Input(new VExpdUOp)
    // val ctrl = Input(new Bundle {
    //   val sew = new SewOH
    //   val sub = Bool()
    // })
    val opcode = Input(UInt(6.W))
    val info = Input(new VIFuInfo)
    val sew = Input(new SewOH)
    val isSub = Input(Bool())
    val isSigned = Input(Bool())
    val isNClip = Input(Bool())
    val fromAdder = Input(new AdderToFixP)
    val fromMisc = Input(new MiscToFixP)
    val out = Output(new Bundle {
      val vd = UInt(64.W)
      val narrow = UInt(32.W)
      val vxsat = Bool()
    })
  })

  val opcode = io.opcode
  val vxrm = io.info.vxrm
  val sew = io.sew
  val sub = io.isSub
  val signed = io.isSigned
  val rnd_high = io.fromMisc.rnd_high
  val rnd_tail = io.fromMisc.rnd_tail

  /**
    *   12.1 Saturating add/sub
    */
  val sat = Wire(Vec(8, Bool())) // Saturate
  val vs2H = io.fromAdder.vs2H
  val vs1H = io.fromAdder.vs1H
  val adderOut = io.fromAdder.vd
  val adderCout = io.fromAdder.cout
  val vdH = adderOut.map(_(7))
  for (i <- 0 until 8) {
    when (signed) {
      sat(i) := Mux(sub, vs2H(i) =/= vs1H(i), vs2H(i) === vs1H(i)) && vs2H(i) =/= vdH(i)
    }.otherwise {
      sat(i) := adderCout(i) ^ sub
    }
  }
  // Whether a 8-bit portion is under saturating state
  val saturate = Mux1H(Seq(
    sew.is8  -> Cat(sat.reverse),
    sew.is16 -> Cat(Fill(2, sat(7)), Fill(2, sat(5)), Fill(2, sat(3)), Fill(2, sat(1))),
    sew.is32 -> Cat(Fill(4, sat(7)), Fill(4, sat(3))),
    sew.is64 -> Fill(8, sat(7))
  ))

  val vdSat = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    // If this 8-bit portion is in up-overflow case (unsigned) when overflow happens
    val upOverflowU = Mux1H(Seq(
      sew.is8  -> adderCout(i),
      sew.is16 -> adderCout((i/2)*2+1),
      sew.is32 -> adderCout((i/4)*4+3),
      sew.is64 -> adderCout(7),
    ))
    // If this 8-bit portion is in down-overflow case (signed) when overflow happens
    val downOverflowS = Mux1H(Seq(
      sew.is8  -> vs2H(i),
      sew.is16 -> vs2H((i/2)*2+1),
      sew.is32 -> vs2H((i/4)*4+3),
      sew.is64 -> vs2H(7),
    ))
    // If this 8-bit portion is the highest bits of a SEW-bit element
    val highestBits = Mux1H(Seq(
      sew.is8  -> true.B,
      sew.is16 -> {if ((i % 2) == 1) true.B else false.B},
      sew.is32 -> {if ((i % 4) == 3) true.B else false.B},
      sew.is64 -> {if (i == 7) true.B else false.B},
    ))
    when (saturate(i)) {
      when (signed) {
        vdSat(i) := Mux(downOverflowS, Cat(highestBits, 0.U(7.W)), Cat(!highestBits, "h7F".U(7.W)))
      }.otherwise {
        vdSat(i) := Mux(upOverflowU, "hFF".U, "h00".U)
      }
    }.otherwise {
      vdSat(i) := adderOut(i)
    }
  }

  /**
    * Rounding methods
    */
  // Rounding Increment
  def roundingInc(v: UInt, d: Int): Bool = {
    Mux1H(Seq(
      (vxrm === 0.U) -> v(d-1),
      (vxrm === 1.U) -> (v(d-1) && ((if (d == 1) false.B else {v(d-2, 0) =/= 0.U}) || v(d))),
      (vxrm === 2.U) -> false.B,
      (vxrm === 3.U) -> (!v(d) && v(d-1, 0) =/= 0.U),
    ))
  }
  // cin is 1 bit carry-in
  class Adder_8b_rnd(in1: UInt, cin: Bool) {
    private val bits = in1 + cin.asUInt
    val (cout, out) = (in1 === "b1111_1111".U && cin, bits)
  }
  // Chain up eight 8-bit adders
  def Adder_chain_rnd(data: Seq[UInt], rndInc: Seq[Bool]): Seq[UInt] = {
    val cin = Wire(Vec(8, Bool()))
    val cout = Wire(Vec(8, Bool()))
    val out = Wire(Vec(8, UInt(8.W)))
    for (i <- 0 until 8) {
      val adder_8b_rnd = new Adder_8b_rnd(data(i), cin(i))
      cin(i) := Mux1H(sew.oneHot, Seq(1, 2, 4, 8).map(n => 
        if ((i % n) == 0) rndInc(i) else cout(i-1))
      )
      cout(i) := adder_8b_rnd.cout
      out(i) := adder_8b_rnd.out
    }
    out
  }

  /**
    *   12.2 Averaging add/sub
    */
  // Highest bit of vs1(i) +& vs2(i)  (8b + 8b = 9b)
  val highBitAvg = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    highBitAvg(i) := adderCout(i) ^ sub ^ Mux(signed, vs1H(i) ^ vs2H(i), false.B)
  }
  val avgRndInc = adderOut.map(x => roundingInc(x, 1))
  val avgBeforeRnd = Wire(Vec(8, UInt(8.W)))
  val high_avgBeforeRnd = Wire(Vec(8, Bool()))
  // ------------------------------------------------------
  //  sew=8:    high 7 6 5 4 3 2 1     high 7 6 5 4 3 2 1
  //  sew=16:   high 7 6 5 4 3 2 1        0 7 6 5 4 3 2 1
  //                                   ----
  //                     Select according to sew: (1) highest bit (2) b0 of higher segment
  for (i <- 0 until 8) {
    high_avgBeforeRnd(i) := Mux1H(sew.oneHot, Seq(1, 2, 4, 8).map(n => 
      if ((i % n) == (n - 1)) highBitAvg(i) else adderOut(i+1)(0)
    ))
  }
  avgBeforeRnd := high_avgBeforeRnd zip adderOut map {case (h, a) => Cat(h, a(7, 1))}

  /**
    *   12.4  12.5
    */
  val dataFromMisc = UIntSplit(io.fromMisc.shiftOut, 8)
  val shiftRndInc = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    shiftRndInc(i) := Mux1H(Seq(
      (vxrm === 0.U) -> rnd_high(i),
      (vxrm === 1.U) -> (rnd_high(i) && (!rnd_tail(i) || dataFromMisc(i)(0))),
      (vxrm === 2.U) -> false.B,
      (vxrm === 3.U) -> (!dataFromMisc(i)(0) && (rnd_high(i) || !rnd_tail(i))),
    ))
  }

  val isScalingShift = opcode === vssrl || opcode === vssra
  val beforeRnd = dataFromMisc zip avgBeforeRnd map {case (d, a) => Mux(isScalingShift, d, a)}
  val rndInc = shiftRndInc zip avgRndInc map {case (s, a) => Mux(isScalingShift, s, a)}
  val afterRnd = Adder_chain_rnd(beforeRnd, rndInc)

  /**
    *   12.5 Narrowing Fixed-Point Clip
    */
  // 2*sew -> sew                       (result, saturate)
  def narrowClip(data: UInt, sew: Int): (UInt, Bool) = {
    val w = data.getWidth
    require(w == sew * 2)
    val result = Wire(UInt(sew.W))
    val sat = Wire(Bool())
    when (signed) {
      when (data(w-1, sew-1) === 0.U || data(w-1, sew-1) === ~0.U((sew+1).W)) {
        result := data(sew-1, 0)
        sat := false.B
      }.otherwise {
        result := Cat(data(w-1), Fill(sew-1, !data(w-1)))
        sat := true.B
      }
    }.otherwise {
      when (data(w-1, sew) === 0.U) {
        result := data(sew-1, 0)
        sat := false.B
      }.otherwise {
        result := ~0.U(sew.W)
        sat := true.B
      }
    }
    (result, sat)
  }

  val afterRndUInt = Cat(afterRnd.reverse)
  val nclipResult32 = narrowClip(afterRndUInt, 32)
  val nclipResult16 = UIntSplit(afterRndUInt, 32).map(x => narrowClip(x, 16))
  val nclipResult8 = UIntSplit(afterRndUInt, 16).map(x => narrowClip(x, 8))
  io.out.narrow := Mux1H(Seq(
    sew.is32 -> nclipResult32._1,
    sew.is16 -> Cat(nclipResult16.map(_._1).reverse),
    sew.is8  -> Cat(nclipResult8.map(_._1).reverse),
  ))
  val nclipSat = Mux1H(Seq(
    sew.is32 -> nclipResult32._2,
    sew.is16 -> nclipResult16.map(_._2).reduce(_ || _),
    sew.is8  -> nclipResult8.map(_._2).reduce(_ || _)
  ))
  
  // io.out.vxsat := Mux(uop.ctrl.fixP && funct6(3) === funct6(2), 
                      // Mux(funct6(3), nclipSat, sat.reduce(_ || _)), false.B)
  io.out.vxsat := Mux(io.isNClip, nclipSat, 
                      Mux(opcode === vsadd || opcode === vssub, sat.reduce(_ || _), false.B))
  io.out.vd := Mux(opcode === vsadd || opcode === vssub, Cat(vdSat.reverse), afterRndUInt)
}
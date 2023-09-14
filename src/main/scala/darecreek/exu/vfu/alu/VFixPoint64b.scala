/**
  * Fixed-Point instructions:
  *     12.1  vsadd, ...
  *     12.2  vaadd, ...
  *     12.4  vssrl, ...
  *     12.5  vnclip, ...
  */
package darecreek.exu.vfu.alu

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._

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
    val funct6 = Input(UInt(6.W))
    val sew = Input(new SewOH)
    val vxrm = Input(UInt(2.W))
    val isFixp = Input(Bool())
    val isSub = Input(Bool())
    val fromAdder = Input(new AdderToFixP)
    val fromMisc = Input(new MiscToFixP)

    val vd = Output(UInt(64.W))
    val narrowVd = Output(UInt(32.W))
    val vxsat = Output(UInt(8.W))
  })

  val funct6 = io.funct6
  val vxrm = io.vxrm
  val sew = io.sew
  val sub = io.isSub
  val signed = funct6(0)
  val isNClip = io.isFixp && funct6(2)
  val isSatAdd = io.isFixp && !funct6(3)
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
  // // cin is 1 bit carry-in
  // class Adder_8b_rnd(in1: UInt, cin: Bool) {
  //   private val bits = in1 + cin.asUInt
  //   val (cout, out) = (in1 === "b1111_1111".U && cin, bits)
  // }
  // // Chain up eight 8-bit adders
  // def Adder_chain_rnd(data: Seq[UInt], rndInc: Seq[Bool]): Seq[UInt] = {
  //   val cin = Wire(Vec(8, Bool()))
  //   val cout = Wire(Vec(8, Bool()))
  //   val out = Wire(Vec(8, UInt(8.W)))
  //   for (i <- 0 until 8) {
  //     val adder_8b_rnd = new Adder_8b_rnd(data(i), cin(i))
  //     val eewCin = Wire(new SewOH)
  //     eewCin.oneHot := Mux(io.isNClip, (sew.oneHot << 1)(3, 0), sew.oneHot)
  //     if (i == 0) {
  //       cin(i) := rndInc(i)
  //     } else if (i == 4) {
  //       cin(i) := Mux(eewCin.is64, cout(i-1), rndInc(i))
  //     } else if (i % 2 == 0) {
  //       cin(i) := Mux(eewCin.is64 || eewCin.is32, cout(i-1), rndInc(i))
  //     } else {
  //       cin(i) := Mux(eewCin.is8, rndInc(i), cout(i-1))
  //     }
  //     // cin(i) := Mux1H(Mux(io.isNClip, (sew.oneHot.asUInt << 1)(3, 0), sew.oneHot.asUInt), Seq(1, 2, 4, 8).map(n => 
  //     //   if ((i % n) == 0) rndInc(i) else cout(i-1))
  //     // )
  //     cout(i) := adder_8b_rnd.cout
  //     out(i) := adder_8b_rnd.out
  //   }
  //   out
  // }

  //---- Rewrite to shorten critical path of vnclip(u) ----
  class Adder_8b_rnd(in1: UInt, cin: Bool) {
    private val bits = in1 + cin.asUInt
    val all_1s = in1 === "b1111_1111".U
    val cout = all_1s && cin
    val out = bits
  }
  // Chain up eight 8-bit adders
  class Adder_chain_rnd extends Module {
    val io = IO(new Bundle {
      val din = Input(Vec(8, UInt(8.W)))
      val rndInc = Input(Vec(8, Bool()))
      val isNClip = Input(Bool())
      val sew = Input(new SewOH)
      val dout = Output(Vec(8, UInt(8.W)))
      val cout = Output(Vec(8, Bool()))
      val all_1s = Output(Vec(8, Bool()))
      val all_0s = Output(Vec(8, Bool()))
    })
    val cin = Wire(Vec(8, Bool()))
    for (i <- 0 until 8) {
      val adder_8b_rnd = new Adder_8b_rnd(io.din(i), cin(i))
      val eewCin = Wire(new SewOH)
      eewCin.oneHot := Mux(io.isNClip, (io.sew.oneHot << 1)(3, 0), io.sew.oneHot)
      if (i == 0) {
        cin(i) := io.rndInc(i)
      } else if (i == 4) {
        cin(i) := Mux(eewCin.is64, io.cout(i-1), io.rndInc(i))
      } else if (i % 2 == 0) {
        cin(i) := Mux(eewCin.is64 || eewCin.is32, io.cout(i-1), io.rndInc(i))
      } else {
        cin(i) := Mux(eewCin.is8, io.rndInc(i), io.cout(i-1))
      }
      io.cout(i) := adder_8b_rnd.cout
      io.dout(i) := adder_8b_rnd.out
      io.all_1s(i) := adder_8b_rnd.all_1s
      io.all_0s(i) := io.din(i) === 0.U
    }
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

  val beforeRnd = dataFromMisc zip avgBeforeRnd map {case (d, a) => Mux(funct6(5), d, a)}
  val rndInc = shiftRndInc zip avgRndInc map {case (s, a) => Mux(funct6(5), s, a)}
  // val afterRnd = Adder_chain_rnd(beforeRnd, rndInc)
  //---- Rewrite to shorten critical path of vnclip(u) ----
  val adder_chain_rnd = Module(new Adder_chain_rnd)
  adder_chain_rnd.io.din := beforeRnd
  adder_chain_rnd.io.rndInc := rndInc
  adder_chain_rnd.io.isNClip := isNClip
  adder_chain_rnd.io.sew := sew
  val afterRnd = adder_chain_rnd.io.dout
  val cout = adder_chain_rnd.io.cout
  val all_1s = adder_chain_rnd.io.all_1s
  val all_0s = adder_chain_rnd.io.all_0s

  /**
    *   12.5 Narrowing Fixed-Point Clip
    */
  // // 2*sew -> sew                       (result, saturate)
  // def narrowClip(data: UInt, sew: Int): (UInt, Bool) = {
  //   val w = data.getWidth
  //   require(w == sew * 2)
  //   val result = Wire(UInt(sew.W))
  //   val sat = Wire(Bool())
  //   when (signed) {
  //     when (data(w-1, sew-1) === 0.U || data(w-1, sew-1) === ~0.U((sew+1).W)) {
  //       result := data(sew-1, 0)
  //       sat := false.B
  //     }.otherwise {
  //       result := Cat(data(w-1), Fill(sew-1, !data(w-1)))
  //       sat := true.B
  //     }
  //   }.otherwise {
  //     when (data(w-1, sew) === 0.U) {
  //       result := data(sew-1, 0)
  //       sat := false.B
  //     }.otherwise {
  //       result := ~0.U(sew.W)
  //       sat := true.B
  //     }
  //   }
  //   (result, sat)
  // }

  //---- Rewrite to shorten critical path of vnclip(u) ----
  // 2*sew -> sew                       (result, saturate)
  def narrow_clip(data: UInt, sew: Int, hi_all_1s: Bool, hi_all_0s: Bool, cout: Bool): (UInt, Bool) = {
    val w = data.getWidth
    require(w == sew * 2)
    val result = Wire(UInt(sew.W))
    val sat = Wire(Bool())
    when (signed) {
      when (hi_all_0s && !cout && !data(sew-1) || hi_all_1s && (cout || !cout && data(sew-1))) {
        result := data(sew-1, 0)
        sat := false.B
      }.otherwise {
        result := Cat(data(w-1), Fill(sew-1, !data(w-1)))
        sat := true.B
      }
    }.otherwise {
      when (hi_all_0s && !cout) {
        result := data(sew-1, 0)
        sat := false.B
      }.otherwise {
        result := ~0.U(sew.W)
        sat := true.B
      }
    }
    (result, sat)
  }

  //---- Rewrite to shorten critical path of vnclip(u) ----
  val afterRndUInt = Cat(afterRnd.reverse)
  // val nclipResult32 = narrowClip(afterRndUInt, 32)
  val nclipResult32 = narrow_clip(afterRndUInt, 32, all_1s.drop(4).reduce(_ && _), 
                                  all_0s.drop(4).reduce(_ && _), cout(3))
  // val nclipResult16 = UIntSplit(afterRndUInt, 32).map(x => narrowClip(x, 16))
  val nclipResult16 = UIntSplit(afterRndUInt, 32).zipWithIndex.map({case (data, i) => 
                      narrow_clip(data, 16, all_1s(i*4+2) && all_1s(i*4+3), 
                                  all_0s(i*4+2) && all_0s(i*4+3), cout(i*4+1))})
  // val nclipResult8 = UIntSplit(afterRndUInt, 16).map(x => narrowClip(x, 8))
  val nclipResult8 = UIntSplit(afterRndUInt, 16).zipWithIndex.map({case (data, i) => 
                     narrow_clip(data, 8, all_1s(i*2+1), 
                                  all_0s(i*2+1), cout(i*2))})
  io.narrowVd := Mux1H(Seq(
    sew.is32 -> nclipResult32._1,
    sew.is16 -> Cat(nclipResult16.map(_._1).reverse),
    sew.is8  -> Cat(nclipResult8.map(_._1).reverse),
  ))
  val nclipSat = Mux1H(Seq(
    sew.is32 -> Fill(4, nclipResult32._2),
    sew.is16 -> Cat(Fill(2, nclipResult16(1)._2), Fill(2, nclipResult16(0)._2)),
    sew.is8  -> Cat(nclipResult8.map(_._2).reverse)
  ))
  
  io.vxsat := Mux(isNClip, nclipSat, 
                  Mux(isSatAdd, saturate, 0.U))
  io.vd := Mux(isSatAdd, Cat(vdSat.reverse), afterRndUInt)
}

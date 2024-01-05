// https://github.com/OpenXiangShan/fudian
package darecreek.exu.vfu.fp.fudian

import chisel3._
import chisel3.util._
import utils.ShiftRightJam

/**
  *  op: 00 => f -> wu
  *      01 => f -> w
  *      10 => f -> lu
  *      11 => f -> l
  */

class FPToInt16_s1_to_s2(val expWidth: Int, val precision: Int) extends Bundle {
  val lpath_max_shamt = 63 - (precision - 1)
  val lpath_max_shmat_width = lpath_max_shamt.U.getWidth

  val raw_a = new RawFloat(expWidth, precision)
  val rm = UInt(3.W)
  val op = UInt(2.W)
  val sel_lpath = Bool()
  val exp_of = Bool()
  val iv_sel_max = Bool()
  // lpath
  val lpath_iv, lpath_of = Bool()
  val lpath_shamt = UInt(lpath_max_shmat_width.W)
  // rpath
  val rpath_shamt = UInt(expWidth.W)
}

class FPToInt16_s1(val expWidth: Int, val precision: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt((expWidth + precision).W))
    val rm = Input(UInt(3.W))
    val op = Input(UInt(2.W))
    val to_s2 = Output(new FPToInt16_s1_to_s2(expWidth, precision))
  })

  val is_signed_int = io.op(0)
  val is_long_int = io.op(1)

  val fp_a = FloatPoint.fromUInt(io.a, expWidth, precision)
  val decode_a = fp_a.decode
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero))

  val max_int_exp =
    FloatPoint.expBias(expWidth).U +& Mux(is_long_int, 63.U, 15.U)
  val exp_of = raw_a.exp > max_int_exp

  io.to_s2.raw_a := raw_a
  io.to_s2.rm := io.rm
  io.to_s2.op := io.op
  io.to_s2.exp_of := exp_of
  io.to_s2.iv_sel_max := decode_a.isNaN || !raw_a.sign
  io.to_s2.sel_lpath


  /*
      Left Shift Path
   */
//  val lpath_shamt = raw_a.exp - (FloatPoint.expBias(expWidth) + precision - 1).U
//  val lpath_iv = !is_signed_int && raw_a.sign
//  val lpath_may_of = is_signed_int && (raw_a.exp === max_int_exp)
//  val lpath_pos_of = lpath_may_of && !raw_a.sign
//  val lpath_neg_of = lpath_may_of && raw_a.sign && raw_a.sig.tail(1).orR()
//  // val lpath_of = lpath_pos_of || lpath_neg_of
//  val lpath_of = false.B

 // lpath shift invalid for int16
  val lpath_shamt = 0.U
  val lpath_iv = false.B
  val lpath_may_of = false.B
  val lpath_pos_of = false.B
  val lpath_neg_of = false.B
  // val lpath_of = lpath_pos_of || lpath_neg_of
  val lpath_of = false.B

  io.to_s2.lpath_iv := lpath_iv
  io.to_s2.lpath_of := lpath_of
  io.to_s2.lpath_shamt := lpath_shamt

  /*
      Right Shift Path
   */
  val rpath_shamt = (FloatPoint.expBias(expWidth) + 15).U - raw_a.exp

  io.to_s2.rpath_shamt := rpath_shamt

  /*
      Select Result
   */
  val sel_lpath = raw_a.exp >= (FloatPoint.expBias(expWidth) + precision - 1).U

  io.to_s2.sel_lpath := sel_lpath

}


class FPToInt16_s2(val expWidth: Int, val precision: Int) extends Module {
  val io = IO(new Bundle() {
    val s1 = Input(new FPToInt16_s1_to_s2(expWidth, precision))
    val result = Output(UInt(64.W))
    val fflags = Output(UInt(5.W))
  })

  val is_signed_int = io.s1.op(0)
  val is_long_int = io.s1.op(1)
  val raw_a = io.s1.raw_a
  val exp_of = io.s1.exp_of

  // lpath
  val lpath_iv = io.s1.lpath_iv
  val lpath_of = io.s1.lpath_of
  val lpath_shamt = io.s1.lpath_shamt
  val lpath_sig_shifted =
    (raw_a.sig << lpath_shamt(io.s1.lpath_max_shmat_width - 1, 0))(63, 0)


  // rpath
  val rpath_shamt = io.s1.rpath_shamt
  val (rpath_sig_shifted, rpath_sitcky) = ShiftRightJam(
    Cat(raw_a.sig, 0.U(1.W)),
    rpath_shamt
  )
  val rpath_rounder = Module(new RoundingUnit(16))
  rpath_rounder.io.in := rpath_sig_shifted.head(16)
  rpath_rounder.io.roundIn := rpath_sig_shifted.tail(16).head(1)
  rpath_rounder.io.stickyIn := rpath_sitcky || rpath_sig_shifted.tail(17).orR
  rpath_rounder.io.signIn := raw_a.sign
  rpath_rounder.io.rm := io.s1.rm
  val rpath_sig = Cat(
    0.U((64 - 16 - 1).W),
    rpath_rounder.io.cout,
    rpath_rounder.io.out
  )
  val rpath_ix = rpath_rounder.io.inexact
  val rpath_iv = !is_signed_int && raw_a.sign && rpath_sig.orR()
  val rpath_exp_inc =
    rpath_rounder.io.r_up && rpath_rounder.io.in(14, 0).andR()
  val rpath_exp_eq_15 = raw_a.exp === (FloatPoint.expBias(expWidth) + 15).U
  val rpath_exp_eq_14 = raw_a.exp === (FloatPoint.expBias(expWidth) + 14).U
  val rpath_pos_of = !raw_a.sign && Mux(
    is_signed_int,
    rpath_exp_eq_15 || (rpath_exp_eq_14 && rpath_exp_inc),
    rpath_exp_eq_15 && rpath_exp_inc
  )
  val rpath_neg_of = raw_a.sign && rpath_exp_eq_15 &&
    (rpath_rounder.io.in(14, 0).orR() || rpath_rounder.io.r_up)

  val rpath_of = !is_long_int && (rpath_pos_of || rpath_neg_of)

  /*
      Select Result
   */
  val sel_lpath = io.s1.sel_lpath
  val of = exp_of || (sel_lpath && lpath_of) || (!sel_lpath && rpath_of)
  val iv = of || (sel_lpath && lpath_iv) || (!sel_lpath && rpath_iv)
  val ix = !iv && !sel_lpath && rpath_ix

  val int_abs = Mux(sel_lpath, lpath_sig_shifted, rpath_sig)
  val int = Mux(raw_a.sign && is_signed_int, -int_abs.tail(48), int_abs.tail(48)
 // & Cat(
 //   Fill(32, is_long_int),
 //   ~0.U(32.W)
  )

  val max_int64 = Cat(!is_signed_int, ~0.U(63.W))
  val min_int64 = Cat(is_signed_int, 0.U(63.W))
  val max_int32 = Cat(0.U(32.W), max_int64.head(32))
  val min_int32 = Cat(0.U(32.W), min_int64.head(32))
  val max_int16 = Cat(0.U(48.W), max_int64.head(16))
  val min_int16 = Cat(0.U(48.W), min_int64.head(16))

  io.result := Mux(
    iv,
    Mux(
      io.s1.iv_sel_max,
      Mux(is_long_int, max_int64, max_int16),
      Mux(is_long_int, min_int64, min_int16)
    ),
    int
  )
  io.fflags := Cat(iv, false.B, false.B, false.B, ix)

}


class FPToInt16(val expWidth: Int, val precision: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt((expWidth + precision).W))
    val rm = Input(UInt(3.W))
    val op = Input(UInt(2.W))
    val result = Output(UInt(64.W))
    val fflags = Output(UInt(5.W))
  })

  val s1 = Module(new FPToInt16_s1(expWidth, precision))
  val s2 = Module(new FPToInt16_s2(expWidth, precision))

  s1.io.a := io.a
  s1.io.rm := io.rm
  s1.io.op := io.op
  s2.io.s1 := s1.io.to_s2
  io.result := s2.io.result
  io.fflags := s2.io.fflags
}

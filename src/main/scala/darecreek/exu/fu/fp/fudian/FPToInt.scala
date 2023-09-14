// https://github.com/OpenXiangShan/fudian
package darecreek.exu.fp.fudian

import chisel3._
import chisel3.util._
import utils.ShiftRightJam

/**
  *  op: 00 => f -> wu
  *      01 => f -> w
  *      10 => f -> lu
  *      11 => f -> l
  */
class FPToInt(val expWidth: Int, val precision: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt((expWidth + precision).W))
    val rm = Input(UInt(3.W))
    val op = Input(UInt(2.W))
    val result = Output(UInt(64.W))
    val fflags = Output(UInt(5.W))
  })

  val is_signed_int = io.op(0)
  val is_long_int = io.op(1)

  val fp_a = FloatPoint.fromUInt(io.a, expWidth, precision)
  val decode_a = fp_a.decode
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero))

  val max_int_exp =
    FloatPoint.expBias(expWidth).U +& Mux(is_long_int, 63.U, 31.U)
  val exp_of = raw_a.exp > max_int_exp

  /*
      Left Shift Path
   */
  val lpath_shamt = raw_a.exp - (FloatPoint.expBias(expWidth) + precision - 1).U
  val lpath_max_shamt = 63 - (precision - 1)
  val lpath_max_shmat_width = lpath_max_shamt.U.getWidth
  val lpath_sig_shifted =
    (raw_a.sig << lpath_shamt(lpath_max_shmat_width - 1, 0))(63, 0)
  val lpath_iv = !is_signed_int && raw_a.sign
  val lpath_may_of = is_signed_int && (raw_a.exp === max_int_exp)
  val lpath_pos_of = lpath_may_of && !raw_a.sign
  val lpath_neg_of = lpath_may_of && raw_a.sign && raw_a.sig.tail(1).orR()
  val lpath_of = lpath_pos_of || lpath_neg_of

  /*
      Right Shift Path
   */
  val rpath_shamt = (FloatPoint.expBias(expWidth) + precision - 1).U - raw_a.exp
  val (rpath_sig_shifted, rpath_sitcky) = ShiftRightJam(
    Cat(raw_a.sig, 0.U(1.W)),
    rpath_shamt
  )
  val rpath_rounder = Module(new RoundingUnit(precision))
  rpath_rounder.io.in := rpath_sig_shifted.head(precision)
  rpath_rounder.io.roundIn := rpath_sig_shifted(0)
  rpath_rounder.io.stickyIn := rpath_sitcky
  rpath_rounder.io.signIn := raw_a.sign
  rpath_rounder.io.rm := io.rm
  val rpath_sig = Cat(
    0.U((64 - precision - 1).W),
    rpath_rounder.io.cout,
    rpath_rounder.io.out
  )
  val rpath_ix = rpath_rounder.io.inexact
  val rpath_iv = !is_signed_int && raw_a.sign && rpath_sig.orR()
  val rpath_of = if (precision >= 32) {
    val rpath_exp_inc =
      rpath_rounder.io.r_up && rpath_rounder.io.in(30, 0).andR()
    val rpath_exp_eq_31 = raw_a.exp === (FloatPoint.expBias(expWidth) + 31).U
    val rpath_exp_eq_30 = raw_a.exp === (FloatPoint.expBias(expWidth) + 30).U
    val rpath_pos_of = !raw_a.sign && Mux(
      is_signed_int,
      rpath_exp_eq_31 || (rpath_exp_eq_30 && rpath_exp_inc),
      rpath_exp_eq_31 && rpath_exp_inc
    )
    val rpath_neg_of = raw_a.sign && rpath_exp_eq_31 &&
      (rpath_rounder.io.in(30, 0).orR() || rpath_rounder.io.r_up)
    // only for int32
    !is_long_int && (rpath_pos_of || rpath_neg_of)
  } else false.B

  /*
      Select Result
   */
  val sel_lpath = raw_a.exp >= (FloatPoint.expBias(expWidth) + precision - 1).U
  val of = exp_of || (sel_lpath && lpath_of) || (!sel_lpath && rpath_of)
  val iv = of || (sel_lpath && lpath_iv) || (!sel_lpath && rpath_iv)
  val ix = !iv && !sel_lpath && rpath_ix

  val int_abs = Mux(sel_lpath, lpath_sig_shifted, rpath_sig)
  val int = Mux(raw_a.sign && is_signed_int, -int_abs, int_abs) & Cat(
    Fill(32, is_long_int),
    ~0.U(32.W)
  )

  val max_int64 = Cat(!is_signed_int, ~0.U(63.W))
  val min_int64 = Cat(is_signed_int, 0.U(63.W))
  val max_int32 = Cat(0.U(32.W), max_int64.head(32))
  val min_int32 = Cat(0.U(32.W), min_int64.head(32))

  io.result := Mux(
    iv,
    Mux(
      decode_a.isNaN || !raw_a.sign,
      Mux(is_long_int, max_int64, max_int32),
      Mux(is_long_int, min_int64, min_int32)
    ),
    int
  )
  io.fflags := Cat(iv, false.B, false.B, false.B, ix)

}

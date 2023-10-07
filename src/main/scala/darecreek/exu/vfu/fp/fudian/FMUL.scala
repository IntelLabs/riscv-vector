// https://github.com/OpenXiangShan/fudian
package darecreek.exu.vfu.fp.fudian

import chisel3._
import chisel3.util._
import utils.{CLZ, Multiplier}

class FMULToFADD_fflags extends Bundle {
  val isNaN = Bool()
  val isInf = Bool()
  val isInv = Bool()
  val overflow = Bool()
}

class FMULToFADD(val expWidth: Int, val precision: Int) extends Bundle {
  val fp_prod = new FloatPoint(expWidth, 2 * precision)
  val inter_flags = new FMULToFADD_fflags
  val rm = UInt(3.W)
}

class FMUL_special_info extends Bundle {
  val nan = Bool()
  val inf = Bool()
  val inv = Bool()
  val hasZero = Bool()
}

class FMUL_s1_to_s2(val expWidth: Int, val precision: Int) extends Bundle {
  val special_case = ValidIO(new FMUL_special_info)
  val early_overflow = Output(Bool())
  val prod_sign = Output(Bool())
  val shift_amt = Output(UInt((expWidth + 1).W))
  val exp_shifted = Output(UInt((expWidth + 1).W))
  val may_be_subnormal = Output(Bool())
  val rm = Output(UInt(3.W))
}

class FMUL_s2_to_s3(val expWidth: Int, val precision: Int) extends Bundle {
  val paddingBits = precision + 2
  val special_case = ValidIO(new FMUL_special_info)
  val raw_out = Output(new RawFloat(expWidth + 1, paddingBits + 2 * precision))
  val early_overflow = Output(Bool())
  val rm = Output(UInt(3.W))
}

class FMUL_s1(val expWidth: Int, val precision: Int) extends Module {
  val io = IO(new Bundle() {
    val a, b = Input(UInt((expWidth + precision).W))
    val rm = Input(UInt(3.W))
    val out = new FMUL_s1_to_s2(expWidth, precision)
  })

  val fp_a = FloatPoint.fromUInt(io.a, expWidth, precision)
  val fp_b = FloatPoint.fromUInt(io.b, expWidth, precision)
  val (decode_a, decode_b) = (fp_a.decode, fp_b.decode)
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero))
  val raw_b = RawFloat.fromFP(fp_b, Some(decode_b.expNotZero))

  val prod_sign = fp_a.sign ^ fp_b.sign

  /*
      prod = xx.xxx...xxx
      sig_pre_shift = precision | g | s | prod
      padding = precision | g | s
      paddingBits = precision + 2
      if prod <- [2, 4):
        prod_exp = a.exp + b.exp - bias + paddingBits + 1
      if prod <- [1, 2):
        prod_exp = a.exp + b.exp - bias + paddingBits
      we assume product <- [2, 4) at first
   */
  val paddingBits = precision + 2
  val padding = 0.U(paddingBits.W)
  val biasInt = FloatPoint.expBias(expWidth)
  require(biasInt > paddingBits)
  val exp_sum = raw_a.exp +& raw_b.exp
  val prod_exp = exp_sum - (biasInt - (paddingBits + 1)).U

  val shift_lim_sub = Cat(0.U(1.W), exp_sum) - (biasInt - paddingBits).U
  val prod_exp_uf = shift_lim_sub.head(1).asBool
  val shift_lim = shift_lim_sub.tail(1)
  // ov <=> exp_a + exp_b - bias > max_exp
  val prod_exp_ov = exp_sum >
    (FloatPoint.maxNormExp(expWidth)+ FloatPoint.expBias(expWidth)).U

  val subnormal_sig = Mux(decode_a.expIsZero, raw_a.sig, raw_b.sig)
  val lzc = CLZ(Cat(padding, subnormal_sig))
  val exceed_lim = shift_lim <= lzc
  val shift_amt = Mux(prod_exp_uf, 0.U, Mux(exceed_lim, shift_lim, lzc))

  val exp_shifted = prod_exp - shift_amt

  io.out.early_overflow := prod_exp_ov
  io.out.prod_sign := prod_sign
  io.out.shift_amt := shift_amt
  io.out.exp_shifted := exp_shifted
  io.out.may_be_subnormal := exceed_lim || prod_exp_uf
  io.out.rm := io.rm

  /*
      Special cases
   */
  val hasZero = decode_a.isZero || decode_b.isZero
  val hasNaN = decode_a.isNaN || decode_b.isNaN
  val hasSNaN = decode_a.isSNaN || decode_b.isSNaN
  val hasInf = decode_a.isInf || decode_b.isInf
  val special_case_happen = hasZero || hasNaN || hasInf

  val zero_mul_inf = hasZero && hasInf
  val nan_result = hasNaN || zero_mul_inf
  val special_iv = hasSNaN || zero_mul_inf

  io.out.special_case.valid := special_case_happen
  io.out.special_case.bits.nan := nan_result
  io.out.special_case.bits.inf := hasInf
  io.out.special_case.bits.inv := special_iv
  io.out.special_case.bits.hasZero := hasZero

}

class FMUL_s2(val expWidth: Int, val precision: Int) extends Module {
  val paddingBits = precision + 2
  val io = IO(new Bundle() {
    val in = Flipped(new FMUL_s1_to_s2(expWidth, precision))
    val prod = Input(UInt((2 * precision).W))
    val out = new FMUL_s2_to_s3(expWidth, precision)
  })

  io.out.special_case := io.in.special_case
  io.out.early_overflow := io.in.early_overflow
  io.out.rm := io.in.rm

  /*
    prod = xx.xxx...xxx
    sig_pre_shift = precision | g | s | prod
    padding = precision | g | s
    paddingBits = precision + 2
    if prod <- [2, 4):
      prod_exp = a.exp + b.exp - bias + paddingBits + 1
    if prod <- [1, 2):
      prod_exp = a.exp + b.exp - bias + paddingBits
    we assume product <- [2, 4) at first
 */

  val padding = 0.U(paddingBits.W)

  val rm = io.in.rm
  val prod = io.prod
  val prod_sign = io.in.prod_sign
  val shift_amt = io.in.shift_amt
  val exp_shifted = io.in.exp_shifted

  val sig_shifter_in = Cat(padding, prod)
  val sig_shifted_raw = (sig_shifter_in << shift_amt)(paddingBits + 2 * precision - 1, 0)
  val exp_is_subnormal = io.in.may_be_subnormal && !sig_shifted_raw.head(1).asBool
  val no_extra_shift = sig_shifted_raw.head(1).asBool || exp_is_subnormal

  val exp_pre_round = Mux(exp_is_subnormal, 0.U, Mux(no_extra_shift, exp_shifted, exp_shifted - 1.U))
  val sig_shifted = Mux(no_extra_shift, sig_shifted_raw, Cat(sig_shifted_raw.tail(1), 0.U(1.W)))

  io.out.raw_out.sign := prod_sign
  io.out.raw_out.exp := exp_pre_round
  io.out.raw_out.sig := sig_shifted

}

class FMUL_s3(val expWidth: Int, val precision: Int) extends Module {
  val paddingBits = precision + 2
  val io = IO(new Bundle() {
    val in = Flipped(new FMUL_s2_to_s3(expWidth, precision))
    val result = Output(UInt((expWidth + precision).W))
    val fflags = Output(UInt(5.W))
    val to_fadd = Output(new FMULToFADD(expWidth, precision))
  })

    val rm = io.in.rm
    val prod_sign = io.in.raw_out.sign

  val exp_pre_round = io.in.raw_out.exp
  val sig_shifted = io.in.raw_out.sig

  val raw_in = Wire(new RawFloat(expWidth, precision + 3))
  raw_in.sign := prod_sign
  raw_in.exp := exp_pre_round
  raw_in.sig := Cat(sig_shifted.head(precision + 2), sig_shifted.tail(precision + 2).orR)

  val tininess_rounder = Module(new TininessRounder(expWidth, precision))
  tininess_rounder.io.in := raw_in
  tininess_rounder.io.rm := rm
  val tininess = tininess_rounder.io.tininess

  val rounder = RoundingUnit(
    raw_in.sig.tail(1), // hidden bit is not needed
    rm,
    raw_in.sign,
    precision - 1
  )

  val exp_rounded = rounder.io.cout + raw_in.exp
  val sig_rounded = rounder.io.out

  val common_of = Mux(
    rounder.io.cout,
    raw_in.exp === ((BigInt(1) << expWidth) - 2).U,
    raw_in.exp === ((BigInt(1) << expWidth) - 1).U
  ) || io.in.early_overflow
  val common_ix = rounder.io.inexact | common_of
  val common_uf = tininess & common_ix

  val rmin = RoundingUnit.is_rmin(rm, raw_in.sign)

  val of_exp = Mux(rmin,
    ((BigInt(1) << expWidth) - 2).U(expWidth.W),
    ((BigInt(1) << expWidth) - 1).U(expWidth.W)
  )
  val common_exp = Mux(
    common_of,
    of_exp,
    exp_rounded(expWidth - 1, 0)
  )
  val common_sig = Mux(
    common_of,
    Mux(rmin, Fill(precision - 1, 1.U(1.W)), 0.U((precision - 1).W)),
    sig_rounded
  )
  val common_result =
    Cat(raw_in.sign, common_exp, common_sig)

  val common_fflags = Cat(false.B, false.B, common_of, common_uf, common_ix)

  val special_case = io.in.special_case
  val special_result = Mux(special_case.bits.nan,
    FloatPoint.defaultNaNUInt(expWidth, precision), // default NaN
    Mux(special_case.bits.inf,
      Cat(
        raw_in.sign,
        ((BigInt(1) << expWidth) - 1).U(expWidth.W),
        0.U((precision - 1).W)), // inf
      Cat(raw_in.sign, 0.U((expWidth + precision - 1).W)) // zero
    )
  )
  val special_fflags = Cat(special_case.bits.inv, false.B, false.B, false.B, false.B)

  io.result := Mux(special_case.valid, special_result, common_result)
  io.fflags := Mux(special_case.valid, special_fflags, common_fflags)

  io.to_fadd.fp_prod.sign := prod_sign
  io.to_fadd.fp_prod.exp := Mux(special_case.bits.hasZero, 0.U, exp_pre_round)
  io.to_fadd.fp_prod.sig := Mux(special_case.bits.hasZero,
    0.U,
    sig_shifted.tail(1).head(2 * precision - 1) | sig_shifted.tail(2 * precision).orR
  )
  io.to_fadd.inter_flags.isInv := special_case.bits.inv
  io.to_fadd.inter_flags.isInf := special_case.bits.inf && !special_case.bits.nan
  io.to_fadd.inter_flags.isNaN := special_case.bits.nan
  io.to_fadd.inter_flags.overflow := exp_pre_round > Fill(expWidth, 1.U(1.W))
  io.to_fadd.rm := rm
}

class FMUL(val expWidth: Int, val precision: Int) extends Module {
  val io = IO(new Bundle() {
    val a, b = Input(UInt((expWidth + precision).W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt((expWidth + precision).W))
    val fflags = Output(UInt(5.W))
    val to_fadd = Output(new FMULToFADD(expWidth, precision))
  })

  val multiplier = Module(new Multiplier(precision + 1, pipeAt = Seq()))
  val fmul_s1 = Module(new FMUL_s1(expWidth, precision))
  val fmul_s2 = Module(new FMUL_s2(expWidth, precision))
  val fmul_s3 = Module(new FMUL_s3(expWidth, precision))


  val raw_a = RawFloat.fromUInt(io.a, expWidth, precision)
  val raw_b = RawFloat.fromUInt(io.b, expWidth, precision)

  multiplier.io.a := raw_a.sig
  multiplier.io.b := raw_b.sig
  multiplier.io.regEnables.foreach(_ := true.B)

  fmul_s1.io.a := io.a
  fmul_s1.io.b := io.b
  fmul_s1.io.rm := io.rm

  fmul_s2.io.in := fmul_s1.io.out
  fmul_s2.io.prod := multiplier.io.result

  fmul_s3.io.in := fmul_s2.io.out

  io.to_fadd := fmul_s3.io.to_fadd
  io.result := fmul_s3.io.result
  io.fflags := fmul_s3.io.fflags

}
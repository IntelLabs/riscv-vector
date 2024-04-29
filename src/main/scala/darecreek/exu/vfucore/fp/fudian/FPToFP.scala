// https://github.com/OpenXiangShan/fudian
package darecreek.exu.vfucore.fp.fudian

import chisel3._
import chisel3.util._
import utils.{CLZ, ShiftRightJam}

abstract class FPConverter(
  val inExpWidth:   Int,
  val inPrecision:  Int,
  val outExpWidth:  Int,
  val outPrecision: Int)
    extends Module {
  val io = IO(new Bundle() {
    val in = Input(UInt((inExpWidth + inPrecision).W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt((outExpWidth + outPrecision).W))
    val fflags = Output(UInt(5.W))
  })
}

class FPToFP(
  inExpWidth:   Int,
  inPrecision:  Int,
  outExpWidth:  Int,
  outPrecision: Int)
    extends FPConverter(inExpWidth, inPrecision, outExpWidth, outPrecision) {

  val down_cvt = (inExpWidth > outExpWidth) && (inPrecision > outPrecision)
  val up_cvt = (inExpWidth < outExpWidth) && (inPrecision < outPrecision)
  require(down_cvt || up_cvt)

  val converter = if (down_cvt) {
    Module(
      new FPDownConverter(
        inExpWidth,
        inPrecision,
        outExpWidth,
        outPrecision
      )
    )
  } else {
    Module(
      new FPUpConverter(
        inExpWidth,
        inPrecision,
        outExpWidth,
        outPrecision
      )
    )
  }

  io <> converter.io
}

class FPDownConverter(
  inExpWidth:   Int,
  inPrecision:  Int,
  outExpWidth:  Int,
  outPrecision: Int)
    extends FPConverter(inExpWidth, inPrecision, outExpWidth, outPrecision) {

  val down_cvt = (inExpWidth > outExpWidth) && (inPrecision > outPrecision)
  require(down_cvt)

  val exp_delta =
    FloatPoint.expBias(inExpWidth) - FloatPoint.expBias(outExpWidth)

  val fp_in = FloatPoint.fromUInt(io.in, inExpWidth, inPrecision)
  val decode = fp_in.decode
  val raw_in = RawFloat.fromFP(fp_in, Some(decode.expNotZero))
  val down_exp = fp_in.exp.zext() - exp_delta.S

  /*
      Normal path
   */
  val normal_sig = fp_in.sig.head(outPrecision - 1)
  val normal_roundBit = fp_in.sig.tail(outPrecision - 1).head(1).asBool()
  val normal_stickyBit = fp_in.sig.tail(outPrecision).orR()

  val normal_rounder = Module(new RoundingUnit(outPrecision - 1))
  normal_rounder.io.in := normal_sig
  normal_rounder.io.roundIn := normal_roundBit
  normal_rounder.io.stickyIn := normal_stickyBit
  normal_rounder.io.signIn := fp_in.sign
  normal_rounder.io.rm := io.rm

  val normal_sig_rounded = normal_rounder.io.out
  val normal_exp_rounded = Mux(normal_rounder.io.cout, down_exp + 1.S, down_exp)
  val normal_of = Mux(
    normal_rounder.io.cout,
    down_exp > (FloatPoint.maxNormExp(outExpWidth) - 1).S,
    down_exp > FloatPoint.maxNormExp(outExpWidth).S
  )
  val exp_uf = Mux(normal_rounder.io.cout, down_exp < 0.S, down_exp < 1.S)
  val normal_ix = normal_rounder.io.inexact || normal_of

  /*
      Subnormal path
   */

  /*
      if down_exp < 1, the input will be a subnormal in output's form,
      so we need to right shift the `sig`.
      shamt = 1 - down_exp
            = 1 - (fp_in.exp - exp_delta)
            = (1 + exp_delta) - fp_in.exp
   */
  val shamt = (exp_delta + 1).U(inExpWidth.W) - fp_in.exp
  val (subnormal_sig, shift_sticky) = ShiftRightJam(
    Cat(decode.expNotZero, fp_in.sig.head(outPrecision)),
    shamt
  )
  val subnormal_sitckyBit = shift_sticky | normal_stickyBit
  val subnormal_rounder = Module(new RoundingUnit(outPrecision - 1))
  subnormal_rounder.io.in := subnormal_sig.tail(1).head(outPrecision - 1)
  subnormal_rounder.io.roundIn := subnormal_sig(0)
  subnormal_rounder.io.stickyIn := subnormal_sitckyBit
  subnormal_rounder.io.signIn := fp_in.sign
  subnormal_rounder.io.rm := io.rm
  val subnormal_sig_rounded = subnormal_rounder.io.out
  val subnormal_exp_rounded = Mux(subnormal_rounder.io.cout, 1.U, 0.U)
  val subnormal_ix = subnormal_rounder.io.inexact

  val may_be_subnormal = down_exp < 1.S

  val rmin =
    io.rm === ROD || io.rm === RTZ || (io.rm === RDN && !fp_in.sign) || (io.rm === RUP && fp_in.sign)

  val normal_of_exp = Mux(
    rmin,
    ((BigInt(1) << outExpWidth) - 2).U(outExpWidth.W),
    ((BigInt(1) << outExpWidth) - 1).U(outExpWidth.W)
  )

  val normal_of_sig = Mux(
    rmin,
    ~0.U((outPrecision - 1).W),
    0.U((outPrecision - 1).W)
  )

  val common_exp = Mux1H(
    Seq(
      !may_be_subnormal && normal_of,
      !may_be_subnormal && !normal_of,
      may_be_subnormal
    ),
    Seq(
      normal_of_exp,
      normal_exp_rounded(outExpWidth - 1, 0),
      subnormal_exp_rounded
    )
  )

  val common_sig = Mux1H(
    Seq(
      !may_be_subnormal && normal_of,
      !may_be_subnormal && !normal_of,
      may_be_subnormal
    ),
    Seq(
      normal_of_sig,
      normal_sig_rounded,
      subnormal_sig_rounded
    )
  )

  val special_case = decode.expIsOnes // NaN or Inf

  val iv = decode.isSNaN
  val dz = false.B
  val of = !special_case && normal_of
  val uf = !special_case && may_be_subnormal && exp_uf && subnormal_ix
  val ix = !special_case && (
    (!may_be_subnormal && normal_ix) ||
      (may_be_subnormal && subnormal_ix)
  )

  val result = Cat(
    !decode.isNaN && fp_in.sign,
    Mux1H(
      Seq(special_case, !special_case),
      Seq(~0.U(outExpWidth.W), common_exp)
    ),
    Mux1H(
      Seq(special_case, !special_case),
      Seq(
        Cat(decode.sigNotZero, 0.U((outPrecision - 2).W)),
        common_sig
      )
    )
  )

  io.result := result
  io.fflags := Cat(iv, dz, of, uf, ix)

}

class FPUpConverter(
  inExpWidth:   Int,
  inPrecision:  Int,
  outExpWidth:  Int,
  outPrecision: Int)
    extends FPConverter(inExpWidth, inPrecision, outExpWidth, outPrecision) {

  val up_cvt = (inExpWidth < outExpWidth) && (inPrecision < outPrecision)
  require(up_cvt)

  val fp_in = FloatPoint.fromUInt(io.in, inExpWidth, inPrecision)
  val decode_in = fp_in.decode

  val exp_delta =
    FloatPoint.expBias(outExpWidth) - FloatPoint.expBias(inExpWidth)
  val normal_sig = fp_in.sig
  val normal_exp = exp_delta.U(outExpWidth.W) + fp_in.exp

  val subnormal_shamt = CLZ(fp_in.sig)
  val subnormal_sig =
    Cat((fp_in.sig << subnormal_shamt)(inPrecision - 3, 0), 0.U(1.W))
  val subnormal_exp = exp_delta.U(outExpWidth.W) - subnormal_shamt

  val result = Cat(
    !decode_in.isNaN && fp_in.sign,
    Mux1H(
      Seq(
        decode_in.expIsOnes,
        decode_in.isZero,
        decode_in.isSubnormal,
        !decode_in.expIsOnes && !decode_in.expIsZero
      ),
      Seq(
        ~0.U(outExpWidth.W),
        0.U(outExpWidth.W),
        subnormal_exp,
        normal_exp
      )
    ),
    Mux1H(
      Seq(
        decode_in.expIsOnes,
        decode_in.expIsZero,
        !decode_in.expIsOnes && !decode_in.expIsZero
      ),
      Seq(
        Cat(decode_in.sigNotZero, 0.U((outPrecision - 2).W)),
        Cat(subnormal_sig, 0.U((outPrecision - inPrecision).W)),
        Cat(normal_sig, 0.U((outPrecision - inPrecision).W))
      )
    )
  )

  val fflags = Cat(decode_in.isSNaN, 0.U(4.W))

  io.result := result
  io.fflags := fflags
}

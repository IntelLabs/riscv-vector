/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

// This file contains components originally written by Yifei He, see
// https://github.com/OpenXiangShan/XS-Verilog-Library/tree/main/int_div_radix_4_v1
// Email of original author: hyf_sysu@qq.com

package darecreek.exu.vfu.fp.fudian

import chisel3._
import chisel3.util._
import utils.CLZ
import chisel3.util.experimental.decode._

object mLookupTable {
  val minus_m = Seq(
    Seq( // -m[-1]
      (0.U(3.W)) -> ("b00_1101".U(6.W)),
      (1.U(3.W)) -> ("b00_1111".U(6.W)),
      (2.U(3.W)) -> ("b01_0000".U(6.W)),
      (3.U(3.W)) -> ("b01_0010".U(6.W)),
      (4.U(3.W)) -> ("b01_0011".U(6.W)),
      (5.U(3.W)) -> ("b01_0101".U(6.W)),
      (6.U(3.W)) -> ("b01_0110".U(6.W)),
      (7.U(3.W)) -> ("b01_1000".U(6.W))
    ),
    Seq( // -m[0]
      (0.U(3.W)) -> ("b00_0100".U(6.W)),
      (1.U(3.W)) -> ("b00_0110".U(6.W)),
      (2.U(3.W)) -> ("b00_0110".U(6.W)),
      (3.U(3.W)) -> ("b00_0110".U(6.W)),
      (4.U(3.W)) -> ("b00_1000".U(6.W)),
      (5.U(3.W)) -> ("b00_1000".U(6.W)),
      (6.U(3.W)) -> ("b00_1000".U(6.W)),
      (7.U(3.W)) -> ("b00_1000".U(6.W))
    ),
    Seq( //-m[1]
      (0.U(3.W)) -> ("b11_1101".U(6.W)),
      (1.U(3.W)) -> ("b11_1100".U(6.W)),
      (2.U(3.W)) -> ("b11_1100".U(6.W)),
      (3.U(3.W)) -> ("b11_1100".U(6.W)),
      (4.U(3.W)) -> ("b11_1011".U(6.W)),
      (5.U(3.W)) -> ("b11_1010".U(6.W)),
      (6.U(3.W)) -> ("b11_1010".U(6.W)),
      (7.U(3.W)) -> ("b11_1010".U(6.W))
    ),
    Seq( //-m[2]
      (0.U(3.W)) -> ("b11_0100".U(6.W)),
      (1.U(3.W)) -> ("b11_0010".U(6.W)),
      (2.U(3.W)) -> ("b11_0001".U(6.W)),
      (3.U(3.W)) -> ("b10_1111".U(6.W)),
      (4.U(3.W)) -> ("b10_1110".U(6.W)),
      (5.U(3.W)) -> ("b10_1100".U(6.W)),
      (6.U(3.W)) -> ("b10_1011".U(6.W)),
      (7.U(3.W)) -> ("b10_1001".U(6.W))
    ),
  )
}

object mLookupTable2 {
  val minus_m = Seq(
    Seq( // -m[-1]
      (0.U(3.W)) -> ("b00_1101".U(6.W)),
      (1.U(3.W)) -> ("b00_1110".U(6.W)),
      (2.U(3.W)) -> ("b01_0000".U(6.W)),
      (3.U(3.W)) -> ("b01_0001".U(6.W)),
      (4.U(3.W)) -> ("b01_0010".U(6.W)),
      (5.U(3.W)) -> ("b01_0100".U(6.W)),
      (6.U(3.W)) -> ("b01_0110".U(6.W)),
      (7.U(3.W)) -> ("b01_0111".U(6.W))
    ),
    Seq( // -m[0]
      (0.U(3.W)) -> ("b00_0100".U(6.W)),
      (1.U(3.W)) -> ("b00_0101".U(6.W)),
      (2.U(3.W)) -> ("b00_0110".U(6.W)),
      (3.U(3.W)) -> ("b00_0110".U(6.W)),
      (4.U(3.W)) -> ("b00_0110".U(6.W)),
      (5.U(3.W)) -> ("b00_1000".U(6.W)),
      (6.U(3.W)) -> ("b00_1000".U(6.W)),
      (7.U(3.W)) -> ("b00_1000".U(6.W))
    ),
    Seq( //-m[1]
      (0.U(3.W)) -> ("b11_1100".U(6.W)),
      (1.U(3.W)) -> ("b11_1100".U(6.W)),
      (2.U(3.W)) -> ("b11_1100".U(6.W)),
      (3.U(3.W)) -> ("b11_1100".U(6.W)),
      (4.U(3.W)) -> ("b11_1010".U(6.W)),
      (5.U(3.W)) -> ("b11_1010".U(6.W)),
      (6.U(3.W)) -> ("b11_1000".U(6.W)),
      (7.U(3.W)) -> ("b11_1000".U(6.W))
    ),
    Seq( //-m[2]
      (0.U(3.W)) -> ("b11_0100".U(6.W)),
      (1.U(3.W)) -> ("b11_0010".U(6.W)),
      (2.U(3.W)) -> ("b11_0000".U(6.W)),
      (3.U(3.W)) -> ("b11_0000".U(6.W)),
      (4.U(3.W)) -> ("b10_1110".U(6.W)),
      (5.U(3.W)) -> ("b10_1100".U(6.W)),
      (6.U(3.W)) -> ("b10_1100".U(6.W)),
      (7.U(3.W)) -> ("b10_1010".U(6.W))
    ),
  )
}

class FDIVSpecialIO extends Bundle {
  val in_valid, out_ready = Input(Bool())
  val in_ready, out_valid = Output(Bool())
  val isSqrt = Input(Bool())
  val kill = Input(Bool())
}



object SignExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    val signBit = a(aLen-1)
    if (aLen >= len) a(len-1,0) else Cat(Fill(len - aLen, signBit), a)
  }
}

class FDIV(val expWidth: Int, val precision: Int) extends Module {
  val io = IO(new Bundle() {
    val a, b = Input(UInt((expWidth + precision).W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt((expWidth + precision).W))
    val fflags = Output(UInt(5.W))
    val specialIO = new FDIVSpecialIO
  })


  val isSqrt = io.specialIO.isSqrt
  val in_valid = io.specialIO.in_valid
  val out_ready = io.specialIO.out_ready
  val out_valid = io.specialIO.out_valid
  val in_ready = io.specialIO.in_ready

  // add the implicit 1 at front
  val fp_a = FloatPoint.fromUInt(io.a, expWidth, precision)
  val fp_b = FloatPoint.fromUInt(io.b, expWidth, precision)
  val (decode_a, decode_b) = (fp_a.decode, fp_b.decode)
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero))
  val raw_b = RawFloat.fromFP(fp_b, Some(decode_b.expNotZero))

  // consts
  val len = precision
  val lzc_width = log2Up(precision+1)
  val itn_len = 1 + len + 2 + 1 // TODO
  val state_num = 6

  val s_idle :: s_pre_0 :: s_pre_1 :: s_iter :: s_post_0 :: s_finish :: Nil = Enum(state_num)

  // STATE
  // idle: change format
  // pre_0: calculate LZC for subnormals, can be skipped
  // pre_1: calculate exponent, and obatins 1st q/s for iter. find all special cases
  // iter: ...
  // post: increment/ decrement exponent
  val state = RegInit((1 << s_idle.litValue.toInt).U(state_num.W))
  val outValidReg = RegInit(false.B)

  val in_fire = in_ready && in_valid
  in_ready := state(s_idle)
  out_valid := outValidReg

  // reused wires
//  val aNormAbs = Wire(UInt((len + 1).W)) // Inputs of xNormAbs regs below
//  val dNormAbs = Wire(UInt((len + 1).W))
  val quotIter = Wire(UInt((len+2).W))
  val quotM1Iter = Wire(UInt((len+2).W))

  val finalIter = Wire(Bool())
  val skipIter = Wire(Bool())

  // reused regs
//  val aNormAbsReg = RegEnable(aNormAbs, newReq | state(s_pre_0) | state(s_post_0)) // reg for normalized a & d and rem & rem+d
//  val dNormAbsReg = RegEnable(dNormAbs, newReq | state(s_pre_0) | state(s_post_0))
  val sqrt = io.specialIO.isSqrt
  val aSign = raw_a.sign
  val dSign = raw_b.sign
  val aSub = decode_a.isSubnormal
  val dSub = decode_b.isSubnormal
  val aSubReg = RegEnable(aSub, state(s_idle))
  val dSubReg = RegEnable(dSub, state(s_idle))
  val hasSubnormal = aSub || (~sqrt && dSub) // todo use wire not reg
  val sqrtReg = RegEnable(isSqrt, state(s_idle))
  val rmReg = RegEnable(io.rm, state(s_idle))
  val resSignReg = RegEnable(Mux(sqrt, Mux(decode_a.isZero, aSign, false.B), aSign ^ dSign), state(s_idle))

  // TODO: sqrt always gets Normal results!
  when(io.specialIO.kill) {
    state := UIntToOH(s_idle, state_num)
    outValidReg := false.B
  } .elsewhen(state(s_idle) && in_fire) {
    state := Mux(hasSubnormal, UIntToOH(s_pre_0, state_num), UIntToOH(s_pre_1, state_num))
  } .elsewhen(state(s_pre_0)) {
    state := UIntToOH(s_pre_1, state_num)
  } .elsewhen(state(s_pre_1)) {
    state := Mux(skipIter, UIntToOH(s_post_0, state_num), UIntToOH(s_iter, state_num))
    outValidReg := skipIter
  } .elsewhen(finalIter && state(s_iter)) {
    state := UIntToOH(s_post_0, state_num)
    outValidReg := true.B
  } .elsewhen(state(s_post_0) && out_ready) {
    state := UIntToOH(s_finish, state_num)
    outValidReg := false.B
  } .elsewhen(state(s_finish)) {
    state := UIntToOH(s_idle, state_num)
  } .otherwise {
    state := state
  }

  assert(outValidReg === state(s_post_0))

  val aSigNorm = Wire(UInt(precision.W))
  val aSigReg = RegEnable(Mux(state(s_idle), raw_a.sig, aSigNorm), state(s_idle) || state(s_pre_0)) // 1.xxx
  val dSigNorm = Wire(UInt(precision.W))
  val dSigReg = RegEnable(Mux(state(s_idle), raw_b.sig, dSigNorm), state(s_idle) || state(s_pre_0))
  val aLZC = CLZ(aSigReg)

  val aExpFix = Wire(UInt((expWidth+1).W)) // these are exp including normalized Subnormals
  val aExp = RegEnable(Mux(state(s_idle), raw_a.exp, aExpFix), state(s_idle) || (state(s_pre_0) && aSubReg))
  val dExpFix = Wire(UInt((expWidth+1).W))
  val dExp = RegEnable(Mux(state(s_idle), raw_b.exp, dExpFix), state(s_idle) || (state(s_pre_0) && dSubReg))

  aSigNorm := aSigReg << aLZC
  val dLZC = CLZ(dSigReg)
  dSigNorm := dSigReg << dLZC
  aExpFix := aExp - aLZC
  dExpFix := dExp - dLZC
  // special cases
  // Detected at s_idle: divide by 0, invalid(0/0, sqrt minus)
  // detected at pre_1: overflow, underflow(except one case)
  // detected at rounding: inexact
  // detect after rounding: underflow
  val inv = RegEnable(Mux(isSqrt, (fp_a.sign && !decode_a.isZero) || decode_a.isNaN, (decode_a.isInf && decode_b.isInf) || (decode_b.isZero && decode_a.isZero) || decode_a.isNaN || decode_b.isNaN), state(s_idle))
  val inv_flag = RegEnable(Mux(isSqrt, (fp_a.sign && !decode_a.isQNaN&& !decode_a.isZero) || decode_a.isSNaN, (decode_a.isInf && decode_b.isInf) || (decode_b.isZero && decode_a.isZero) || (decode_a.isSNaN) || (decode_b.isSNaN)), state(s_idle))
  val dz = RegEnable(decode_b.isZero && !decode_a.isZero && !isSqrt, state(s_idle))
  val zero_div = RegEnable(decode_a.isZero && (!decode_b.isZero || isSqrt), state(s_idle)) // exact zero
  val div_inf = RegEnable(!isSqrt && decode_b.isInf && !decode_a.isInf, state(s_idle))
  val inf_div = RegEnable(Mux(isSqrt, decode_a.isInf, decode_a.isInf && !decode_b.isInf), state(s_idle))

  // s_pre_1
  // obtain final exp and 0st iter result
  // also obtain iterNum
  val sigCmp = aSigReg < dSigReg
//  val sigCmpReg = RegEnable(sigCmp, state(s_pre_1))
  val divFinalExp = (SignExt(aExp, expWidth+2) + ~SignExt(dExp, expWidth+2)) + (~sigCmp).asUInt
  val sqrtShift = !aExp(0)
  val sqrtFinalExp = SignExt((aExp - FloatPoint.expBias(expWidth).U) >> 1, expWidth + 2) // TODO minus expBias
  val finalExp = Mux(sqrtReg, sqrtFinalExp, divFinalExp) // todo finalExpReg

  val zeroRes = (finalExp + (FloatPoint.expBias(expWidth)+precision).U).head(1).asBool // inexact zero
  val infRes = (~finalExp + (FloatPoint.maxNormExp(expWidth) - FloatPoint.expBias(expWidth) + 1).U).head(1).asBool
  val subRes = (finalExp + (FloatPoint.expBias(expWidth)-1).U).head(1).asBool && ~zeroRes
  val normRes = (~(FloatPoint.expBias(expWidth)).U + 2.U + ~finalExp).head(1).asBool && ~infRes
  val subResReg = RegEnable(subRes, state(s_pre_1))

  val overflow = infRes && !inv && !dz && !inf_div
  val underflow_pre = zeroRes && !inv && !dz && !zero_div && !div_inf
  val inexact_pre = !zero_div && !inv && !dz && !div_inf && !inf_div

  val special_fflags = RegEnable(Cat(inv_flag, dz && !inv && !inf_div, overflow, underflow_pre, inexact_pre), state(s_pre_1))
  val special_sign = RegEnable(Mux(inv, false.B, resSignReg), state(s_pre_1))
  val special_exp = RegEnable(Mux(inv || overflow || dz || inf_div, Fill(expWidth, true.B), 0.U(expWidth.W)), state(s_pre_1)) // put inv first
  val special_sig = RegEnable(Mux(inv, 1.U << (precision-2), 0.U(expWidth.W)), state(s_pre_1))
  val special_result = Cat(special_sign, special_exp, special_sig)

  skipIter := overflow || underflow_pre || inv || dz || zeroRes || zero_div || div_inf || inf_div// TODO div_inf and inf_div
  val skipIterReg = RegEnable(skipIter, state(s_pre_1))

  val subResBits = finalExp + (FloatPoint.expBias(expWidth)+precision).U // TODO sig cmp
  val normResBits = (precision+1).U

  val resultSigBits = Mux(subRes, subResBits, normResBits) // with one round bit
  val resultSigBitsReg = RegEnable(resultSigBits, state(s_pre_1))  
  val needShift = Mux(sqrtReg, resultSigBits(0), !resultSigBits(0)) //
  val needShiftReg = RegEnable(needShift, state(s_pre_1)) //
  val oddIterReg = RegEnable(!resultSigBits(1), state(s_pre_1))
  val iterNum = Wire(UInt((lzc_width-1).W))
  val iterNumInit = Mux(sqrtReg, (resultSigBits -1.U) >> 1, (resultSigBits) >> 2) // TODO
  val iterNumReg = RegEnable(iterNum, state(s_pre_1) || state(s_iter))
  iterNum := Mux(state(s_pre_1), iterNumInit, iterNumReg - 1.U)
  finalIter := iterNumReg === 0.U

  // Here are the iter modules
  val divModule = Module(new DivIterModule(len, itn_len))
  divModule.io.a := aSigReg
  divModule.io.d := dSigReg
  divModule.io.state := Cat(state(s_iter), state(s_pre_1))
  divModule.io.lastIterDoHalf := oddIterReg && finalIter
  divModule.io.sigCmp := sigCmp

  val sqrtModule = Module(new SqrtIterModule(len, itn_len+1))
  sqrtModule.io.a := Mux(!sqrtShift, Cat(0.U(1.W), aSigReg), Cat(aSigReg, 0.U(1.W)))
  sqrtModule.io.state := Cat(state(s_iter), state(s_pre_1))
  
  // Recovery
  quotIter := Mux(sqrtReg, sqrtModule.io.res, divModule.io.quot)
  quotM1Iter := Mux(sqrtReg, sqrtModule.io.resM1, divModule.io.quotM1)

  // post_1
  val r = Mux(sqrtReg, SignExt(sqrtModule.io.rem, itn_len+1), SignExt(divModule.io.rem, itn_len+1)) // TODO fix this
  val qFinal = Mux(r.head(1).asBool, quotM1Iter, quotIter) //
  val sticky = (r.orR()) || (needShiftReg && qFinal(0)) // if non-zero remainder( which must be positive), we
  val round = Mux(needShiftReg, qFinal(1).asBool(), qFinal(0).asBool)
  val rounder = Module(new RoundingUnit(precision-1))
  rounder.io.in := Mux(needShiftReg, qFinal(precision, 2), qFinal(precision-1, 1))
  rounder.io.stickyIn := sticky
  rounder.io.roundIn := round
  rounder.io.signIn := resSignReg
  rounder.io.rm := rmReg

  val resExp = Mux(subResReg, 0.U, finalExp + FloatPoint.expBias(expWidth).U)(expWidth-1, 0)
  val inexact = rounder.io.inexact
  val uf = !resExp.orR && inexact // TODO
  val of = resExp.andR
  val normal_fflags = Cat(0.U(2.W), of, uf, inexact)
  val combinedFFlags = Mux(skipIterReg, special_fflags, normal_fflags)

  // in rmin rmax rminMag
  val noInf = (rmReg === RTZ || (rmReg === RDN && !resSignReg) || (rmReg === RUP && resSignReg)) && special_fflags(2)
  val noZero = ((rmReg === RDN && resSignReg) || (rmReg === RUP && !resSignReg)) && special_fflags(1)
  val specialRmSig = Mux(noInf, Fill(precision-1, true.B), 1.U((precision - 1).W))
  val specialRmExp = Mux(noInf, FloatPoint.maxNormExp(expWidth).U, 0.U(expWidth.W))

  val combinedExp = Mux(noInf || noZero, specialRmExp,
    Mux(skipIterReg, special_exp,
      Mux(rounder.io.cout && (resExp =/= FloatPoint.maxNormExp(expWidth).U), resExp + 1.U, resExp)))
  val combinedSig = Mux(noInf || noZero, specialRmSig,
    Mux(skipIterReg, special_sig,
      Mux(rounder.io.cout && (resExp === FloatPoint.maxNormExp(expWidth).U), rounder.io.in(precision - 2, 0) ,rounder.io.out(precision - 2, 0))))
  val combinedExpReg = RegEnable(combinedExp, state(s_post_0))
  val combinedSigReg = RegEnable(combinedSig, state(s_post_0))
  val combinedFFlagsReg = RegEnable(combinedFFlags, state(s_post_0))
  val combinedSignReg = RegEnable(Mux(inv, false.B, resSignReg), state(s_post_0))

//  val specialInf = noInf || noZero
//  val specialInfResult = Mux(noInf, Cat(resSignReg, FloatPoint.maxNormExp(expWidth).U, Fill(precision-1, true.B)), Cat(resSignReg, 0.U(expWidth.W), 1.U((precision - 1).W)))

  io.result := Cat(combinedSignReg, combinedExpReg, combinedSigReg)
  io.fflags := combinedFFlagsReg
}

class DivIterModule(len: Int, itn_len: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(len.W)) 
    val d = Input(UInt(len.W))
    val state = Input(UInt(2.W)) // 01 for pre and 10 for iter
    val lastIterDoHalf = Input(Bool()) // ~oddIter && finalIter
    val sigCmp = Input(Bool())
    val rem = Output(UInt((itn_len).W))
    val quot = Output(UInt((len + 2).W)) // additional bit in case odd bit result
    val quotM1 = Output(UInt((len + 2).W))
  })
  val quot_neg_2 :: quot_neg_1 :: quot_0 :: quot_pos_1 :: quot_pos_2 :: Nil = Enum(5)

  val state = io.state
  val (s_pre_1, s_iter) = (0, 1)
  val wsInit = Mux(io.sigCmp, Cat(0.U(2.W), io.a, 0.U(2.W)), Cat(0.U(3.W), io.a, 0.U(1.W)))
  val wcInit = 0.U(itn_len.W)
  val (a, d) = (io.a, io.d)
  val lookup = d(len-2, len-4)

  val smallerThanM1 = (wsInit.head(7)(4, 0) + MuxLookup(lookup(2,0), 0.U, mLookupTable.minus_m(2))).head(1).asBool
  val smallerThanM2 = (wsInit.head(7)(4, 0) + MuxLookup(lookup(2,0), 0.U, mLookupTable.minus_m(3))).head(1).asBool

  val qInit = Mux(smallerThanM1, UIntToOH(quot_0), Mux(smallerThanM2, UIntToOH(quot_pos_1), UIntToOH(quot_pos_2)))

  // in pre_1 we also obtain m_i + 16u * d for all u
  // udNeg -> (rud, r2ud) -> (rudPmNeg, r2udPmNeg)
  val dPos = Cat(0.U(1.W), d)                          // +d, 0.1xxx, (1, 64)
  val dNeg = -Cat(0.U(1.W), d) // -d, 1.xxxx, (1, 64)
  // val m = Wire(Vec(4, UInt(7.W)))     // we have to sigext them to calculate rqd-m_k

  // index 0 is for q=-2 and 4 is for q=2!!!
  val mNeg = Wire(Vec(4, UInt(12.W))) // selected m, extended to (6, 6) bits
  val rudNeg = Wire(Vec(5, UInt(10.W))) // (4, 6)
  val r2udNeg = Wire(Vec(5, UInt(12.W))) // (6, 6)

  // Selection Block with improved timing
  val rudPmNeg = Wire(Vec(5, Vec(4, UInt(10.W)))) // -(r*u*d+m_k), (5, 5) bits
  val r2ws = Wire(UInt(10.W)) // r^2*ws (5, 5) bits
  val r2wc = Wire(UInt(10.W))
  // calculating exact values of w
  val udNeg = Wire(Vec(5, UInt(itn_len.W))) // (3, 65), 1 signExt'ed Bit
  // val r3udNeg = Wire(Vec(5, UInt(13.W)))

  // Speculative Block
  val r2udPmNeg = Wire(Vec(5, Vec(4, UInt(13.W)))) // -(r^2*d*d+m_k), (7, 6) bits. 1st index for q 2nd for m
  val r3ws = Wire(UInt(13.W)) // r^3*ws, (7, 6) bits
  val r3wc = Wire(UInt(13.W))
  val qSpec = Wire(Vec(5, UInt(5.W))) // 5 speculative results of qNext2
  // output wires
  val qNext = Wire(UInt(5.W))
  val qNext2 = Wire(UInt(5.W))
  val wsIter = Wire(UInt(itn_len.W)) // (1, 67)
  val wcIter = Wire(UInt(itn_len.W))
  val quotIter = Wire(UInt((len+2).W))
  val quotM1Iter = Wire(UInt((len+2).W))

  // Input Regs of whole Spec + Sel + sum adder block
  val qPrevReg = RegEnable(Mux(state(s_pre_1), qInit, qNext2), state(s_pre_1) | state(s_iter))
  val wsReg = RegEnable(Mux(state(s_pre_1), wsInit, wsIter), state(s_pre_1) | state(s_iter)) // (1, 67)
  val wcReg = RegEnable(Mux(state(s_pre_1), wcInit, wcIter), state(s_pre_1) | state(s_iter))
  val quotIterReg = RegEnable(quotIter, state(s_pre_1) | state(s_iter))
  val quotM1IterReg = RegEnable(quotM1Iter, state(s_pre_1) | state(s_iter))

  // Give values to the regs and wires above...
  mNeg := VecInit(Seq.tabulate(4){i =>
    Cat(SignExt(MuxLookup(lookup(2,0), 0.U, mLookupTable.minus_m(i)), 10), 0.U(2.W))
  }) // (2, 4) -> (6, 6)

  udNeg := VecInit(
    Cat(SignExt(dPos, len+2), 0.U(2.W)),
    Cat(SignExt(dPos, len+3), 0.U(1.W)),
    0.U,
    Cat(SignExt(dNeg, len+3), 0.U(1.W)),
    Cat(SignExt(dNeg, len+2), 0.U(2.W))
  )

  rudNeg := VecInit(Seq.tabulate(5){i => udNeg(i)(itn_len-2, itn_len-11)})
  r2udNeg := VecInit(Seq.tabulate(5){i => udNeg(i)(itn_len-2, itn_len-13)})
  // r3udNeg := VecInit(Seq.tabulate(5){i => udNeg(i)(itn_len-2, itn_len-13)})
  rudPmNeg := VecInit(Seq.tabulate(5){i => VecInit(Seq.tabulate(4){ j => SignExt(rudNeg(i)(9, 1), 10) + mNeg(j)(10, 1)})})
  r2udPmNeg := VecInit(Seq.tabulate(5){i => VecInit(Seq.tabulate(4){ j => SignExt(r2udNeg(i), 13) + SignExt(mNeg(j), 13)})})
  r3ws := wsReg(itn_len-1, itn_len-13)
  r3wc := wcReg(itn_len-1, itn_len-13)

  r2ws := wsReg(itn_len-1, itn_len-10)
  r2wc := wcReg(itn_len-1, itn_len-10)

  val udNegReg = RegEnable(udNeg, state(s_pre_1))
  //  val rudNegReg = RegEnable(rudNeg, state(s_pre_1))
  val rudPmNegReg = RegEnable(rudPmNeg, state(s_pre_1))
  val r2udPmNegReg = RegEnable(r2udPmNeg, state(s_pre_1))

  // Selection block
  val signs = VecInit(Seq.tabulate(4){ i => {
    val csa = Module(new CSA3_2(10)).suggestName(s"csa_sel_${i}")
    csa.io.in(0) := r2ws
    csa.io.in(1) := r2wc
    csa.io.in(2) := Mux1H(qPrevReg, rudPmNegReg.toSeq)(i) // rudPmNeg(OHToUInt(qPrevReg))(i)

    (csa.io.out(0) + (csa.io.out(1)(8, 0) << 1))(9)
  }})
  qNext := DetectSign(signs.asUInt, s"sel_q")
  val csaWide1 = Module(new CSA3_2(itn_len)).suggestName("csa_sel_wide_1")
  val csaWide2 = Module(new CSA3_2(itn_len)).suggestName("csa_sel_wide_2")
  csaWide1.io.in(0) := wsReg << 2
  csaWide1.io.in(1) := wcReg << 2
  csaWide1.io.in(2) := Mux1H(qPrevReg, udNegReg.toSeq) << 2//udNeg(OHToUInt(qPrevReg)) << 2
  csaWide2.io.in(0) := csaWide1.io.out(0) << 2
  csaWide2.io.in(1) := (csaWide1.io.out(1) << 1)(itn_len-1, 0) << 2
  csaWide2.io.in(2) := Mux1H(qNext, udNegReg.toSeq) << 2 // udNeg(OHToUInt(qNext)) << 2
  wsIter := Mux(io.lastIterDoHalf, csaWide1.io.out(0), csaWide2.io.out(0))
  wcIter := Mux(io.lastIterDoHalf, (csaWide1.io.out(1) << 1)(itn_len-1, 0), (csaWide2.io.out(1) << 1)(itn_len-1, 0))
  // r3wsIter := r3udNeg(OHToUInt(qNext))
  // r3wcIter := (csaWide1.io.out(0)(itn_len-3, itn_len-16) + (csaWide1.io.out(1) << 1)(itn_len-3, itn_len-16))(13,1)
  // Speculative block
  qSpec := VecInit(Seq.tabulate(5){ q_spec => {
    val csa1 = Module(new CSA3_2(13)).suggestName(s"csa_spec_${q_spec}")
    csa1.io.in(0) := r3ws
    csa1.io.in(1) := r3wc
    csa1.io.in(2) := SignExt(udNegReg(q_spec)(itn_len-2, itn_len-11), 13) // (4, 6) -> (7, 6)
    val signs2 = VecInit(Seq.tabulate(4){ i => {
      val csa2 = Module(new CSA3_2(13)).suggestName(s"csa_spec_${q_spec}_${i}")
      csa2.io.in(0) := csa1.io.out(0)
      csa2.io.in(1) := (csa1.io.out(1) << 1)(12, 0)
      csa2.io.in(2) := Mux1H(qPrevReg, r2udPmNegReg.toSeq)(i) // r2udPmNeg(OHToUInt(qPrevReg))(i)
      (csa2.io.out(0) + (csa2.io.out(1)(11, 0) << 1))(12)
    }})
    val qVec2 = DetectSign(signs2.asUInt, s"spec_q_${q_spec}")
    qVec2
  }})

  qNext2 := Mux1H(qNext, qSpec.toSeq)

  // on the fly quotient conversion
  val quotHalfIter = Wire(UInt((len+2).W))
  val quotM1HalfIter = Wire(UInt((len+2).W))
  val quotIterNext = Wire(UInt((len+2).W))
  val quotM1IterNext = Wire(UInt((len+2).W))
  
  quotHalfIter := OTFC(qPrevReg, quotIterReg, quotM1IterReg, len+2)._1
  quotM1HalfIter := OTFC(qPrevReg, quotIterReg, quotM1IterReg, len+2)._2
  quotIterNext := Mux(io.lastIterDoHalf, quotHalfIter, OTFC(qNext, quotHalfIter, quotM1HalfIter, len+2)._1)
  quotM1IterNext := Mux(io.lastIterDoHalf, quotM1HalfIter, OTFC(qNext, quotHalfIter, quotM1HalfIter, len+2)._2)

  quotIter := Mux(state(s_iter), quotIterNext, 0.U(len.W))
  quotM1Iter := Mux(state(s_iter), quotM1IterNext, 0.U(len.W))

  io.rem := wsReg + wcReg // (2,
  io.quot := quotIterReg
  io.quotM1 := quotM1IterReg
}

class SqrtIterModule(len: Int, itn_len: Int) extends Module { // itn_len == len + 5
  val io = IO(new Bundle{
    val a = Input(UInt((len+1).W)) // because of odd exponents (.1xxx or .01xx)
    val state = Input(UInt(2.W)) // 01 for pre and 10 for iter
    val rem = Output(UInt((itn_len-2).W)) // this itn_len is diffferent
    val res = Output(UInt((len+2).W))
    val resM1 = Output(UInt((len+2).W))
  })

  // y is truncated wj
  val state = io.state
  val (s_pre_1, s_iter) = (0, 1)

  val aInit = 1.U
  val bInit = 0.U
  val wsInit = Cat(3.U(2.W), io.a) // 11.1xxx or 11.01xx
  val wcInit = 0.U

  val aIter = Wire(UInt((len+2).W))
  val bIter = Wire(UInt((len+2).W))
  val wsIter = Wire(UInt((itn_len-2).W))
  val wcIter = Wire(UInt((itn_len-2).W))
  val s = Wire(UInt(5.W))
  val f = Wire(UInt(itn_len.W)) // (3, itn_len-3) or (3, len+1)
  val j = Wire(UInt(32.W)) // TODO do not hardcode this

  val aReg = RegEnable(Mux(state(s_pre_1), aInit, aIter), state(s_pre_1) || state(s_iter))
  val bReg = RegEnable(Mux(state(s_pre_1), bInit, bIter), state(s_pre_1) || state(s_iter)) // (1, len+1)
  val aHeadReg = Reg(UInt(3.W))

  val wsReg = RegEnable(Mux(state(s_pre_1), wsInit, wsIter), state(s_pre_1) || state(s_iter)) // (2, len+1)
  val wcReg = RegEnable(Mux(state(s_pre_1), wcInit, wcIter), state(s_pre_1) || state(s_iter))

  val jReg = RegEnable(Mux(state(s_pre_1), 1.U, j+1.U), state(s_pre_1) || state(s_iter))
  j := jReg

  val lookup = Mux(j === 1.U, "b101".U, aHeadReg)(2, 0)

  val mNeg = VecInit(Seq.tabulate(4){i =>
    Cat(SignExt(MuxLookup(lookup(2,0), 0.U, mLookupTable2.minus_m(i)), 7), 0.U(1.W))
  }) // (2, 4) * 2 -> (4, 4)

  // Debug Utils
//  when(state === 1.U) {
//    printf(
//      p"\nINIT:\nio.a is ${Binary(io.a)}\nwsInit is ${Binary(wsInit.head(2))}.${Binary(wsInit.tail(2))}\n")
//  }
//  when(state === 2.U) {
//    printf(
//      p"***** ITER NUM J is ${jReg} *****\n" +
//        p"ws is ${Binary(wsReg.head(2))}.${Binary(wsReg.tail(2))}\n" +
//        p"wc is ${Binary(wcReg.head(2))}.${Binary(wcReg.tail(2))}\n" +
////        p"-m is ${Binary(csa.io.in(2).head(4))}.${Binary(csa.io.in(2).tail(4))}\n" +
//        p" A is ${Binary(aReg)}\n" +
//        p" B is ${Binary(bReg)}\n"
//    )
//  }

  val signs = VecInit(Seq.tabulate(4){ i => {
    val csa = Module(new CSA3_2(8)).suggestName(s"csa_sel_${i}")
    csa.io.in(0) := wsReg.head(8) // wsReg << 2 (4, 4)
    csa.io.in(1) := wcReg.head(8)
    csa.io.in(2) := mNeg(i)

//    // debug utils
//    when (jReg < 8.U && jReg > 4.U) {
//      when(state === 2.U) {
//        printf(s"=== csa_sel_${i} ===\n")
//        printf(
//          p"state is ${Binary(state)}\n" +
//            p"ws is ${Binary(csa.io.in(0).head(4))}.${Binary(csa.io.in(0).tail(4))}\n" +
//            p"wc is ${Binary(csa.io.in(1).head(4))}.${Binary(csa.io.in(1).tail(4))}\n" +
//            p"-m is ${Binary(csa.io.in(2).head(4))}.${Binary(csa.io.in(2).tail(4))}\n" +
//            p"rs is ${Binary(csa.io.out(0).head(4))}.${Binary(csa.io.out(0).tail(4))}\n" +
//            p"rc is ${Binary(csa.io.out(1).head(5).tail(1))}.${Binary(csa.io.out(1).tail(5))}0\n"
//        )
//      }
//    }
    (csa.io.out(0) + (csa.io.out(1)(6, 0) << 1))(7)
  }})
  s := DetectSign(signs.asUInt, s"sel_q")
  f := Mux1H(s, Seq(
    (bReg << 4 | "b1100".U),
    (bReg << 3 | "b111".U),
    0.U,
    (~aReg << 3 | "b111".U),
    (~aReg << 4 | "b1100".U)
  )) // (2, len + 1)

  val csaWide = Module(new CSA3_2(len+3)).suggestName("csa_iter")
  csaWide.io.in(0) := wsReg << 2
  csaWide.io.in(1) := wcReg << 2
  csaWide.io.in(2) := (f << (len+1).U) >> (j << 1) // workaround for f << ((len+1).U - (j << 1))
  wsIter := csaWide.io.out(0)(len+2, 0)
  wcIter := csaWide.io.out(1)(len+1, 0) << 1

//  when(state === 2.U) {
//    printf(
////      p"=== Wide CSAs ===\n" +
//        p"signs is ${Binary(signs.asUInt)}; s[j+1] is ${s}\n" +
//        p"lookup is ${Binary(lookup)}\n" //+
////        p"4ws is ${Binary(csaWide.io.in(0).head(2))}.${Binary(csaWide.io.in(0).tail(2))}\n" +
////        p"4wc is ${Binary(csaWide.io.in(1).head(2))}.${Binary(csaWide.io.in(1).tail(2))}\n" +
////        p"  F is ${Binary(csaWide.io.in(2).head(2))}.${Binary(csaWide.io.in(2).tail(2))}\n" +
////        p" rs is ${Binary(csaWide.io.out(0).head(2))}.${Binary(csaWide.io.out(0).tail(2))}\n" +
////        p" rc is ${Binary(csaWide.io.out(1).head(3).tail(1))}.${Binary(csaWide.io.out(1).tail(3))}0\n\n"
//    )
//  }

  // OTFC
  aIter := OTFC(s, aReg, bReg, len+2)._1
  bIter := OTFC(s, aReg, bReg, len+2)._2

  aHeadReg := Mux(aIter(j << 1),
    "b111".U(3.W),
    Mux(j === 1.U,
      Cat(aIter(0), 0.U(2.W)),
      aIter >> ((j - 2.U) << 1)))

  io.rem := wsReg + wcReg // (1, len+3)
  io.res := aReg
  io.resM1 := bReg
}

object DetectSign {
  def apply(signs: UInt, name: String): UInt = {
    val qVec = Wire (Vec(5, Bool())).suggestName (name)
    qVec (0) := signs (0) && signs (1) && signs (2)
    qVec (1) := ~ signs (0) && signs (1) && signs (2)
    qVec (2) := signs (2) && ~ signs (1)
    qVec (3) := signs (3) && ~ signs (2) && ~ signs (1)
    qVec (4) := ~ signs (3) && ~ signs (2) && ~ signs (1)
    qVec.asUInt
  }
}

object OTFC {
  def apply(q: UInt, quot: UInt, quotM1: UInt, len: Int): (UInt, UInt) = {
    val quotNext = Mux1H(Seq(
      q(4) -> (quot << 2 | "b10".U),
      q(3) -> (quot << 2 | "b01".U),
      q(2) -> (quot << 2 | "b00".U),
      q(1) -> (quotM1 << 2 | "b11".U),
      q(0) -> (quotM1 << 2 | "b10".U)
    ))
    val quotM1Next = Mux1H(Seq(
      q(4) -> (quot << 2 | "b01".U),
      q(3) -> (quot << 2 | "b00".U),
      q(2) -> (quotM1 << 2 | "b11".U),
      q(1) -> (quotM1 << 2 | "b10".U),
      q(0) -> (quotM1 << 2 | "b01".U)
    ))
    (quotNext(len - 1, 0), quotM1Next(len - 1, 0))
  }
}

abstract class CarrySaveAdderMToN(m: Int, n: Int)(len: Int) extends Module{
  val io = IO(new Bundle() {
    val in = Input(Vec(m, UInt(len.W)))
    val out = Output(Vec(n, UInt(len.W)))
  })
}

class CSA3_2(len: Int) extends CarrySaveAdderMToN(3, 2)(len){
  val temp = Wire(Vec(len, UInt(2.W)))
  for((t, i) <- temp.zipWithIndex){
    val (a, b, cin) = (io.in(0)(i), io.in(1)(i), io.in(2)(i))
    val a_xor_b = a ^ b
    val a_and_b = a & b
    val sum = a_xor_b ^ cin
    val cout = a_and_b | (a_xor_b & cin)
    t := Cat(cout, sum)
  }
  io.out.zipWithIndex.foreach({case(x, i) => x := Cat(temp.reverse map(_(i)))})
}


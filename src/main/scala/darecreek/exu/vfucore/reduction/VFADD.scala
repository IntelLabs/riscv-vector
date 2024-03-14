package darecreek.exu.vfucore.reduction

/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * ************************************************************************************* */

import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3._
import freechips.rocketchip.config._
import darecreek.exu.vfucore.fp._
import fudian._
import fudian.utils.Multiplier
import darecreek.exu.vfucore._
import darecreek.exu.vfucoreconfig.Redirect

/*
 * for widening insts and various fma ops
 * latency = 1
 */
class VFMASrcPreprocessPipe(implicit val p: Parameters) extends VFPUBaseModule {

  def invert(x: UInt, len: Int) = {
    Cat(!x(len - 1), x(len - 2, 0))
  }

  def invertWithTypeTag(x: UInt, typeTag: UInt) = {
    require(x.getWidth == 64)
    Mux(typeTag === VFPU.S, Cat(invert(x(63, 32), 32), invert(x(31, 0), 32)), invert(x, 64))
  }

  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LaneFloatFUIn))
    val redirect = Input(new Redirect)
    val out = Decoupled(new LaneFloatFUIn)
  })
  val uop = io.in.bits.uop
  val fpCtrl = uop.vfpCtrl
  val typeTag = uop.typeTag
  val validReg = Reg(chiselTypeOf(io.out.valid))

  // preprocessing for fma & sub insts, etc
  val vs1StageMid = Mux(fpCtrl.negVs1, invertWithTypeTag(io.in.bits.vs1, typeTag), io.in.bits.vs1)
  val vs2StageMid = Mux(fpCtrl.negVs2, invertWithTypeTag(io.in.bits.vs2, typeTag), io.in.bits.vs2)
  val vdStageMid = Mux(fpCtrl.negVd, Mux(uop.ctrl.widen, invertWithTypeTag(io.in.bits.old_vd, VFPU.D), invertWithTypeTag(io.in.bits.old_vd, typeTag)), io.in.bits.old_vd)
  val vs1StageFinal = vs1StageMid
  val vs2StageFinal = Mux(fpCtrl.switchVdVs2, vdStageMid, vs2StageMid)
  val vdStageFinal = Mux(fpCtrl.switchVdVs2, vs2StageMid, vdStageMid)


  //  Redirect handling
  def latency = 1

  val validVec = (io.in.valid) +: Array.fill(latency)(RegInit(false.B))
  val rdyVec = Array.fill(latency)(Wire(Bool())) :+ io.out.ready
  val uopVec = io.in.bits.uop +: Array.fill(latency)(Reg(new VFPUOp))
  // val flushVec = validVec.zip(uopVec).map(x => x._1 && x._2.sysUop.robIdx.needFlush(io.redirect))
  val flushVec = validVec.zip(uopVec).map(x => x._1 && io.redirect.needFlush(x._2.robIdx))

  def regEnable(i: Int): Bool = validVec(i - 1) && rdyVec(i - 1) && !flushVec(i - 1)

  for (i <- 0 until latency) {
    rdyVec(i) := !validVec(i + 1) || rdyVec(i + 1)
  }

  for (i <- 1 to latency) {
    when(regEnable(i)) {
      validVec(i) := validVec(i - 1)
      uopVec(i) := uopVec(i - 1)
    }.elsewhen(flushVec(i) || rdyVec(i)) {
      validVec(i) := false.B
    }
  }

  // io.out.valid := validVec.last && !io.out.bits.uop.sysUop.robIdx.needFlush(io.redirect)
  io.out.valid := validVec.last && !io.redirect.needFlush(io.out.bits.uop.robIdx)
  io.in.ready := rdyVec(0)


  // val transfer = io.in.valid && io.in.ready
  val transfer = regEnable(1)

  // fp32 to fp64 converter for widening insts
  val fcvt1 = Module(new FPToFP(
    VFPU.f32.expWidth, VFPU.f32.precision,
    VFPU.f64.expWidth, VFPU.f64.precision
  ))
  val fcvt2 = Module(new FPToFP(
    VFPU.f32.expWidth, VFPU.f32.precision,
    VFPU.f64.expWidth, VFPU.f64.precision
  ))
  // widen fma insts do not need vdvs2 switch
  fcvt1.io.in := Mux(
    uop.expdIdx(0),
    vs1StageMid.head(32),
    vs1StageMid.tail(32)
  )
  fcvt2.io.in := Mux(
    uop.expdIdx(0),
    vs2StageMid.head(32),
    vs2StageMid.tail(32)
  )
  fcvt1.io.rm := 0.U // up convert, thus no need to specify rounding mode
  fcvt2.io.rm := 0.U

  // Keep SNaN signalling
  val cvt1_fp_res = FloatPoint.fromUInt(fcvt1.io.result, VFPU.f64.expWidth, VFPU.f64.precision)
  val cvt1_decode = cvt1_fp_res.decode
  val cvt1_res = Mux(cvt1_decode.isNaN && fcvt1.io.fflags(4), Cat(fcvt1.io.result(63, 52), ~fcvt1.io.result(51, 0)), fcvt1.io.result)
  val cvt2_fp_res = FloatPoint.fromUInt(fcvt2.io.result, VFPU.f64.expWidth, VFPU.f64.precision)
  val cvt2_decode = cvt2_fp_res.decode
  val cvt2_res = Mux(cvt2_decode.isNaN && fcvt2.io.fflags(4), Cat(fcvt2.io.result(63, 52), ~fcvt2.io.result(51, 0)), fcvt2.io.result)

  io.out.bits := RegEnable(io.in.bits, transfer)
  io.out.bits.vs1 := RegEnable(
    Mux(uop.ctrl.widen || uop.ctrl.widen2, cvt1_res, vs1StageFinal),
    transfer
  )
  io.out.bits.vs2 := RegEnable(
    Mux(uop.ctrl.widen, cvt2_res, vs2StageFinal),
    transfer
  )
  io.out.bits.old_vd := RegEnable(vdStageFinal, transfer)
  // instruct fma to use f64 arithmetic blocks, override
  io.out.bits.uop.typeTag := RegEnable(
    Mux(uop.ctrl.widen || uop.ctrl.widen2, VFPU.D, uop.typeTag),
    transfer
  )
}

class VFADD_pipe(val addLat: Int = 2)(implicit val p: Parameters) extends VFPUPipelineModule {

  override def latency: Int = addLat

  val isFMA = IO(Input(Bool()))
  val src1 = S1Reg(io.in.bits.vs1)
  val src2 = S1Reg(io.in.bits.vs2)
  val eleActives = S2Reg(S1Reg(VecInit(Seq(0, 4).map(isActive))))

  val uopIn = S1Reg(io.in.bits.uop)
  val typeTagIn = uopIn.typeTag

  val fma = S1Reg(isFMA)

  // for element group 0
  val stagesElmt0 = VFPU.ftypes.zipWithIndex.map {
    case (t, i) => initPipeStages(t, src1, src2)
  }
  val (stage1Elmt0, stage2Elmt0) = stagesElmt0.unzip
  val (stage1Elmt1, stage2Elmt1) = initPipeStages(
    VFPU.f32, src1.head(32), src2.head(32)
  )

  val outSel = S2Reg(VecInit(VFPU.ftypes.zipWithIndex.map(_._2.U === typeTagIn)))
  io.out.bits.vd := Mux1H(outSel, Seq(
    Cat(stage2Elmt1.io.result, stage2Elmt0(0).io.result),
    stage2Elmt0(1).io.result
  ))

  io.out.bits.fflags := Mux1H(outSel, Seq(
    Mux(eleActives(1), stage2Elmt1.io.fflags, empty_fflags)
      | Mux(eleActives(0), stage2Elmt0(0).io.fflags, empty_fflags), // 32bit x 2
    Mux(
      Mux(S2Reg(uopIn.expdIdx(0)), eleActives(1), eleActives(0)),
      stage2Elmt0(1).io.fflags,
      empty_fflags
    ))
  )

  def initPipeStages(ftype: VFPU.FType, src1: UInt, src2: UInt) = {
    val s1 = Module(new FCMA_ADD_s1(ftype.expWidth, 2 * ftype.precision, ftype.precision))
    val s2 = Module(new FCMA_ADD_s2(ftype.expWidth, 2 * ftype.precision, ftype.precision))
    val in1 = Cat(src1(ftype.len - 1, 0), 0.U(ftype.precision.W))
    val in2 = Cat(src2(ftype.len - 1, 0), 0.U(ftype.precision.W))
    s1.io.a := in2
    s1.io.b := in1
    s1.io.b_inter_valid := fma
    s1.io.b_inter_flags := 0.U.asTypeOf(s1.io.b_inter_flags)
    s1.io.rm := S1Reg(io.in.bits.uop.info.frm)
    s2.io.in := S2Reg(s1.io.out)
    (s1, s2)
  }
}

class VFADD(implicit val p: Parameters) extends VFPUSubModule {

  val sourcePrepare = Module(new VFMASrcPreprocessPipe)
  sourcePrepare.io.in <> io.in
  sourcePrepare.io.redirect := io.redirect
  val add_pipe = Module(new VFADD_pipe())

  val fpCtrl = sourcePrepare.io.out.bits.uop.vfpCtrl

  // Note: current version does not support pipeline flush
  val mulOutIsFMA = false.B
  val mulOutIsFMAReg = RegNext(mulOutIsFMA)

  // 0 indicates mul, 1 indicates add/sub
  val isAddSub = fpCtrl.fmaCmd(1) && !fpCtrl.fmaCmd(0)
  // For FADD, it accepts instructions from io.in and FMUL.
  // When FMUL gives an FMA, FADD accepts this instead of io.in.
  // Since FADD gets FMUL data from add_pipe.mulToAdd, only uop needs Mux.
  add_pipe.io.in.valid := sourcePrepare.io.out.valid && isAddSub || mulOutIsFMA // isAddSub or FMAfromMulPipe
  add_pipe.io.in.bits := sourcePrepare.io.out.bits
  add_pipe.io.in.bits.prestart := sourcePrepare.io.out.bits.prestart
  add_pipe.io.in.bits.mask := sourcePrepare.io.out.bits.mask
  add_pipe.io.in.bits.tail := sourcePrepare.io.out.bits.tail

  add_pipe.io.redirect := io.redirect
  add_pipe.io.in.bits.uop := sourcePrepare.io.out.bits.uop
  add_pipe.isFMA := mulOutIsFMA

  // When the in uop is Add/Sub, we check FADD, otherwise fmul is checked.
  sourcePrepare.io.out.ready := add_pipe.io.in.ready

  // For FMUL:
  // (1) It always accept FMA from FADD (if an FMA wants FMUL, it's never blocked).
  // (2) It has lower writeback arbitration priority than FADD (and may be blocked when FMUL.out.valid).
  // mul_pipe.io.out.ready := mulOutIsFMA || (io.out.ready && !add_pipe.io.out.valid)
  add_pipe.io.out.ready := io.out.ready

  io.out.bits.uop := add_pipe.io.out.bits.uop
  io.out.bits.vd := add_pipe.io.out.bits.vd
  io.out.bits.fflags := add_pipe.io.out.bits.fflags

  io.out.valid := add_pipe.io.out.valid

}

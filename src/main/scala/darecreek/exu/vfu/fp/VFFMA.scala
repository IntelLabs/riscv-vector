package darecreek.exu.vfu.fp

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
import fudian._
import fudian.utils.Multiplier
import darecreek.exu.vfu._
import xiangshan.Redirect

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

  object WidenState extends ChiselEnum {
    val sEmpty, sWiden1, sWiden2 = Value
  }

  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LaneFloatFUIn))
    val redirect = Input(ValidIO(new Redirect))
    val out = Decoupled(new LaneFloatFUIn)
  })
  val uop = io.in.bits.uop
  val fpCtrl = uop.vfpCtrl
  val typeTag = uop.typeTag
  val widenState = RegInit(WidenState.sEmpty)
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
  val flushVec = validVec.zip(uopVec).map(x => x._1 && x._2.sysUop.robIdx.needFlush(io.redirect))

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

  io.out.valid := validVec.last && !io.out.bits.uop.sysUop.robIdx.needFlush(io.redirect)
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
  io.out.bits.uop.fWidenEnd := RegEnable(
    widenState.isOneOf(WidenState.sWiden1, WidenState.sWiden2),
    transfer
  )

  // FSM & ready/valid interface
  // always accept new input, no matter if it is a widening
  // io.in.ready := io.out.ready || !io.out.valid
  // io.out.valid := validReg


  when(transfer) {
    // validReg := io.in.valid
    switch(widenState) {
      is(WidenState.sEmpty) {
        when(uop.ctrl.widen) {
          widenState := WidenState.sWiden1
        }.elsewhen(uop.ctrl.widen2) { // NOTE!!! It is assumed only .wv insts has widen2 == true
          widenState := WidenState.sWiden2
        }
      }
      is(WidenState.sWiden1) {
        when(uop.ctrl.widen) {
          widenState := WidenState.sEmpty
        }
      }
      is(WidenState.sWiden2) {
        when(uop.ctrl.widen2) {
          widenState := WidenState.sEmpty
        }
      }
    }
    //  } .elsewhen(io.out.ready) {
    //    validReg := false.B
  }



  // switch(widenState) {
  //   is(WidenState.sEmpty) {
  //     when(transfer) {
  //       validReg := true.B
  //       when(uop.ctrl.widen) {
  //         widenState := WidenState.sWiden1
  //       }.elsewhen(uop.ctrl.widen2) {  // NOTE!!! It is assumed only .wv insts has widen2 == true
  //         widenState := WidenState.sWiden2
  //       }
  //     }.elsewhen(io.out.ready) {
  //       validReg := false.B
  //     }
  //   }
  //   is(WidenState.sWiden1) {
  //     when(io.out.ready) {
  //       widenState := WidenState.sEmpty
  //     }
  //   }
  //   is(WidenState.sWiden2) {
  //     when(io.out.ready) {
  //       widenState := WidenState.sEmpty
  //     }
  //   }
  // }
}


class MulToAddIO(val ftypes: Seq[VFPU.FType])(implicit val p: Parameters) extends Bundle {
  val mulOut = MixedVec(ftypes.map(t => new FMULToFADD(t.expWidth, t.precision)))
  val addend = UInt(ftypes.map(_.len).max.W)
  val uop = new VFPUOp

  def getFloat = mulOut.head

  def getDouble = mulOut.last
}

class MulToAddIOVec(val ftypes: Seq[VFPU.FType])(implicit val p: Parameters) extends Bundle {
  val mulOutElmt0Vec = MixedVec(ftypes.map(t => new FMULToFADD(t.expWidth, t.precision))) // 32 bit + 64 bit
  val mulOutElmt1 = new FMULToFADD(VFPU.f32.expWidth, VFPU.f32.precision) // 32 bit
  val addend = UInt(ftypes.map(_.len).max.W)
  val prestart = UInt(8.W)
  val mask = UInt(8.W)
  val tail = UInt(8.W)
  val uop = new VFPUOp

  //  def getFloat0 = mul_out_0.head
  //  def getDouble0 = mul_out_0.last
}

//
//class FMAMidResult extends FMULToFADD(FPU.ftypes.last.expWidth, FPU.ftypes.last.precision) {
//
//  def toFloat: FMULToFADD = {
//    val floatMidResult = Wire(new FMULToFADD(FPU.ftypes.head.expWidth, FPU.ftypes.head.precision))
//    floatMidResult.fp_prod.sign := fp_prod.sign
//    floatMidResult.fp_prod.exp := fp_prod.exp
//    floatMidResult.fp_prod.sig := fp_prod.sig
//    floatMidResult.inter_flags := inter_flags
//    floatMidResult
//  }
//
//  def fromFloat(float: FMULToFADD): FMULToFADD = {
//    fp_prod.sign := float.fp_prod.sign
//    fp_prod.exp := float.fp_prod.exp
//    fp_prod.sig := float.fp_prod.sig
//    inter_flags := float.inter_flags
//    this
//  }
//}
//
//class FMAMidResultIO extends Bundle {
//  val in = Flipped(ValidIO(new FMAMidResult))
//  val out = ValidIO(new FMAMidResult)
//  val waitForAdd = Input(Bool())
//}


class VFMUL_pipe(val mulLat: Int = 2)(implicit val p: Parameters)
  extends VFPUPipelineModule {
  override def latency: Int = mulLat

  val toAdd = IO(Output(new MulToAddIOVec(VFPU.ftypes)))

  val uopIn = uopVec(0)
  val typeTagIn = uopIn.typeTag

  val typeSel = VecInit(VFPU.ftypes.zipWithIndex.map(_._2.U === typeTagIn))
  val outSel = S2Reg(S1Reg(typeSel))
  val eleActives = S2Reg(S1Reg(VecInit(Seq(0, 4).map(isActive))))

  val src1 = io.in.bits.vs1
  val src2 = io.in.bits.vs2

  // the second elements (32 bit)
  val src1Elmt1 = Mux(typeTagIn === VFPU.D, 0.U(32.W), src1.head(32))
  val src2Elmt1 = Mux(typeTagIn === VFPU.D, 0.U(32.W), src2.head(32))

  // for element 0
  val multiplierElmt0 = Module(new Multiplier(VFPU.ftypes.last.precision + 1, pipeAt = Seq(1)))
  val (stage1Elmt0, stage2Elmt0, stage3Elmt0) = VFPU.ftypes.map {
    initPipeStages(_, src1, src2)
  }.unzip3
  stage2Elmt0.foreach(_.io.prod := multiplierElmt0.io.result)
  val (mul_a_sel0, mul_b_sel0) = stage1Elmt0.zipWithIndex.map {
    case (s, i) =>
      val raw_a = RawFloat.fromUInt(s.io.a, s.expWidth, s.precision)
      val raw_b = RawFloat.fromUInt(s.io.b, s.expWidth, s.precision)
      (
        (typeTagIn === i.U) -> raw_a.sig,
        (typeTagIn === i.U) -> raw_b.sig
      )
  }.unzip
  multiplierElmt0.io.a := Mux1H(mul_a_sel0)
  multiplierElmt0.io.b := Mux1H(mul_b_sel0)
  multiplierElmt0.io.regEnables(0) := regEnable(1)

  // for element 1
  val typeS = VFPU.ftypes(0)
  val multiplierElmt1 = Module(new Multiplier(typeS.precision + 1, pipeAt = Seq(1)))
  val (stage1Elmt1, stage2Elmt1, stage3Elmt1) = initPipeStages(typeS, src1Elmt1, src2Elmt1)
  stage2Elmt1.io.prod := multiplierElmt1.io.result
  multiplierElmt1.io.a := RawFloat.fromUInt(stage1Elmt1.io.a, stage1Elmt1.expWidth, stage1Elmt1.precision).sig
  multiplierElmt1.io.b := RawFloat.fromUInt(stage1Elmt1.io.b, stage1Elmt1.expWidth, stage1Elmt1.precision).sig
  // only enable the multiplier when ops are of 32 bit
  multiplierElmt1.io.regEnables(0) := regEnable(1) && typeSel(0) // enable when fp32

  // output logic
  // to adder
  toAdd.addend := S2Reg(S1Reg(io.in.bits.old_vd))
  toAdd.prestart := S2Reg(S1Reg(io.in.bits.prestart))
  toAdd.mask := S2Reg(S1Reg(io.in.bits.mask))
  toAdd.tail := S2Reg(S1Reg(io.in.bits.tail))
  toAdd.mulOutElmt0Vec.zip(stage3Elmt0.map(_.io.to_fadd)).foreach(x => x._1 := x._2)
  toAdd.mulOutElmt1 := stage3Elmt1.io.to_fadd
  toAdd.uop := uopVec.last
  // to output

  io.out.bits.vd := Mux1H(outSel, Seq(
    Cat(stage3Elmt1.io.result, stage3Elmt0(0).io.result), // 32 bit x 2
    stage3Elmt0(1).io.result // 64 bit
  ))

  io.out.bits.fflags := Mux1H(outSel, Seq(
    // 32bit x 2
    Mux(eleActives(1), stage3Elmt1.io.fflags, empty_fflags)
      | Mux(eleActives(0), stage3Elmt0(0).io.fflags, empty_fflags),
    // 64bit
    Mux(
      Mux(S2Reg(S1Reg(uopIn.fWidenEnd)), eleActives(1), eleActives(0)),
      stage3Elmt0(1).io.fflags,
      empty_fflags
    ))
  )

  def initPipeStages(ftype: VFPU.FType, src1: UInt, src2: UInt) = {
    require(src1.getWidth >= ftype.len && src2.getWidth >= ftype.len)
    // s1 -> s2 -> s3
    val s1 = Module(new FMUL_s1(ftype.expWidth, ftype.precision))
    val s2 = Module(new FMUL_s2(ftype.expWidth, ftype.precision))
    val s3 = Module(new FMUL_s3(ftype.expWidth, ftype.precision))
    s1.io.a := src1(ftype.len - 1, 0)
    s1.io.b := src2(ftype.len - 1, 0)
    s1.io.rm := io.in.bits.uop.info.frm
    s2.io.in := S1Reg(s1.io.out)
    s3.io.in := S2Reg(s2.io.out)
    (s1, s2, s3)
  }
}


class VFADD_pipe(val addLat: Int = 2)(implicit val p: Parameters) extends VFPUPipelineModule {

  override def latency: Int = addLat

  // isFMA, io.in and mulToAddVec should be sync sigals
  val mulToAddVec = IO(Input(new MulToAddIOVec(VFPU.ftypes)))
  val isFMA = IO(Input(Bool()))
  val src1 = S1Reg(io.in.bits.vs1)
  val src2 = S1Reg(Mux(isFMA, mulToAddVec.addend, io.in.bits.vs2))
  val eleActives = S2Reg(S1Reg(VecInit(Seq(0, 4).map(isActive))))

  val uopIn = S1Reg(Mux(isFMA, mulToAddVec.uop, io.in.bits.uop))
  val typeTagIn = uopIn.typeTag

  val fma = S1Reg(isFMA)
  val mulProdElmt0Vec = S1Reg(mulToAddVec.mulOutElmt0Vec)
  val mulProdElmt1 = S1Reg(mulToAddVec.mulOutElmt1)

  // for element group 0
  val stagesElmt0 = VFPU.ftypes.zipWithIndex.map {
    case (t, i) => initPipeStages(t, src1, src2, mulProdElmt0Vec(i))
  }
  val (stage1Elmt0, stage2Elmt0) = stagesElmt0.unzip
  val (stage1Elmt1, stage2Elmt1) = initPipeStages(
    VFPU.f32, src1.head(32), src2.head(32), mulProdElmt1
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
      Mux(S2Reg(uopIn.fWidenEnd), eleActives(1), eleActives(0)),
      stage2Elmt0(1).io.fflags,
      empty_fflags
    ))
  )

  def initPipeStages(ftype: VFPU.FType, src1: UInt, src2: UInt, mulProd: FMULToFADD) = {
    val s1 = Module(new FCMA_ADD_s1(ftype.expWidth, 2 * ftype.precision, ftype.precision))
    val s2 = Module(new FCMA_ADD_s2(ftype.expWidth, 2 * ftype.precision, ftype.precision))
    val in1 = Mux(fma,
      mulProd.fp_prod.asUInt,
      Cat(src1(ftype.len - 1, 0), 0.U(ftype.precision.W))
    )
    val in2 = Cat(src2(ftype.len - 1, 0), 0.U(ftype.precision.W))
    s1.io.a := in2
    s1.io.b := in1
    s1.io.b_inter_valid := fma
    s1.io.b_inter_flags := Mux(fma,
      mulProd.inter_flags,
      0.U.asTypeOf(s1.io.b_inter_flags)
    )
    s1.io.rm := S1Reg(io.in.bits.uop.info.frm)
    s2.io.in := S2Reg(s1.io.out)
    (s1, s2)
  }
}

class VFFMA(implicit val p: Parameters) extends VFPUSubModule {

  val sourcePrepare = Module(new VFMASrcPreprocessPipe)
  sourcePrepare.io.in <> io.in
  sourcePrepare.io.redirect := io.redirect
  val mul_pipe = Module(new VFMUL_pipe())
  val add_pipe = Module(new VFADD_pipe())

  val fpCtrl = sourcePrepare.io.out.bits.uop.vfpCtrl
  mul_pipe.io.in <> sourcePrepare.io.out
  mul_pipe.io.redirect <> io.redirect
  mul_pipe.io.in.valid := sourcePrepare.io.out.valid && fpCtrl.fmaCmd(0) // override

  // Note: current version does not support pipeline flush
  val mulOutIsFMA = mul_pipe.io.out.valid && mul_pipe.io.out.bits.uop.vfpCtrl.fmaCmd(1)
  val mulOutIsFMAReg = RegNext(mulOutIsFMA)

  add_pipe.mulToAddVec <> mul_pipe.toAdd

  // 0 indicates mul, 1 indicates add/sub
  val isAddSub = fpCtrl.fmaCmd(1) && !fpCtrl.fmaCmd(0)
  // For FADD, it accepts instructions from io.in and FMUL.
  // When FMUL gives an FMA, FADD accepts this instead of io.in.
  // Since FADD gets FMUL data from add_pipe.mulToAdd, only uop needs Mux.
  add_pipe.io.in.valid := sourcePrepare.io.out.valid && isAddSub || mulOutIsFMA // isAddSub or FMAfromMulPipe
  add_pipe.io.in.bits := sourcePrepare.io.out.bits
  add_pipe.io.in.bits.prestart := Mux(mulOutIsFMA, add_pipe.mulToAddVec.prestart, sourcePrepare.io.out.bits.prestart)
  add_pipe.io.in.bits.mask := Mux(mulOutIsFMA, add_pipe.mulToAddVec.mask, sourcePrepare.io.out.bits.mask)
  add_pipe.io.in.bits.tail := Mux(mulOutIsFMA, add_pipe.mulToAddVec.tail, sourcePrepare.io.out.bits.tail)

  add_pipe.io.redirect := io.redirect
  add_pipe.io.in.bits.uop := Mux(mulOutIsFMA, add_pipe.mulToAddVec.uop, sourcePrepare.io.out.bits.uop)
  add_pipe.isFMA := mulOutIsFMA

  // When the in uop is Add/Sub, we check FADD, otherwise fmul is checked.
  sourcePrepare.io.out.ready := Mux(isAddSub, // is add/sub
    !mulOutIsFMA && add_pipe.io.in.ready,
    mul_pipe.io.in.ready
  )

  // For FMUL:
  // (1) It always accept FMA from FADD (if an FMA wants FMUL, it's never blocked).
  // (2) It has lower writeback arbitration priority than FADD (and may be blocked when FMUL.out.valid).
  // mul_pipe.io.out.ready := mulOutIsFMA || (io.out.ready && !add_pipe.io.out.valid)
  mul_pipe.io.out.ready := (mulOutIsFMA && io.out.ready) || (io.out.ready && !add_pipe.io.out.valid)
  add_pipe.io.out.ready := io.out.ready

//    io.out.bits.uop := RegNext(Mux(add_pipe.io.out.valid,
//      add_pipe.io.out.bits.uop,
//      mul_pipe.io.out.bits.uop
//    ))
//    io.out.bits.vd := Mux(RegNext(add_pipe.io.out.valid),
//      add_pipe.io.out.bits.vd,
//      mul_pipe.io.out.bits.vd
//    )
//    io.out.bits.fflags := Mux(RegNext(add_pipe.io.out.valid),
//      add_pipe.io.out.bits.fflags,
//      mul_pipe.io.out.bits.fflags
//    )
//    // delay 1 cycle to match timing of arithmetic result
//    io.out.valid := RegNext(add_pipe.io.out.valid || (mul_pipe.io.out.valid && !mulOutIsFMA))


  io.out.bits.uop := Mux(add_pipe.io.out.valid,
    add_pipe.io.out.bits.uop,
    mul_pipe.io.out.bits.uop
  )
  io.out.bits.vd := Mux(add_pipe.io.out.valid,
    add_pipe.io.out.bits.vd,
    mul_pipe.io.out.bits.vd
  )
  io.out.bits.fflags := Mux(add_pipe.io.out.valid,
    add_pipe.io.out.bits.fflags,
    mul_pipe.io.out.bits.fflags
  )
  // delay 1 cycle to match timing of arithmetic result
  io.out.valid := add_pipe.io.out.valid || (mul_pipe.io.out.valid && !mulOutIsFMA)


}

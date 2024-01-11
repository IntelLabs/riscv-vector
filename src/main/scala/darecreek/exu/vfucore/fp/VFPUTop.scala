package darecreek.exu.vfucore.fp

import chipsalliance.rocketchip.config.Parameters
import chisel3.{util, _}
import chisel3.util._
// import darecreek.exu.vfucore.{LaneFUInput, LaneFUOutput}
import darecreek.exu.vfucore._
// import darecreek.exu.vfucore.MaskTailData
import xiangshan.Redirect

class VFPUTop(implicit val p: Parameters)
  extends VFPUBaseModule {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val redirect = Input(ValidIO(new Redirect))
    val out = DecoupledIO(new LaneFUOutput)
    val maskKeep = Output(UInt(64.W))
    val maskOff = Output(UInt(64.W))
  })
  val fpu_ready = Wire(Bool())
  val inputGen = Module(new VFInputGenFP)
  inputGen.io.in.bits := io.in.bits
  inputGen.io.redirect := io.redirect
  inputGen.io.in.valid := io.in.valid
  inputGen.io.out.ready := fpu_ready
  io.in.ready := inputGen.io.in.ready

  val fma = Module(new VFFMA)
  val cvt = Module(new VFCVT)
  val misc = Module(new VFMisc)
  val rec = Module(new VFRec)
  // output priority high to low
  val subModules = Seq(fma, cvt, misc, rec)
  subModules.foreach(m => {
    m.io.in.bits := inputGen.io.out.bits
    m.io.redirect := io.redirect
    m.io.in.valid := inputGen.io.out.valid
  })

  // output arbiter ,  rec > fma > cvt > misc
  val outArbiter = Module(new Arbiter(new LaneFloatFUOut, subModules.length))
  outArbiter.io.in.zip(subModules.map(_.io.out)).foreach {
    case (in, out) => in <> out
  }

  val fpu_out_w = Wire(new LaneFUOutput)
  outArbiter.io.out.bits.outputToLaneFU(fpu_out_w)


  //  Redirect handling
  def latency = 1


  val validVec = outArbiter.io.out.valid +: Array.fill(latency)(RegInit(false.B))
  val rdyVec = Array.fill(latency)(Wire(Bool())) :+ io.out.ready
  val uopVec = fpu_out_w.uop +: Array.fill(latency)(Reg(new VExpdUOp))
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

  io.out.valid := validVec.last
  outArbiter.io.out.ready := rdyVec(0)


  //  val fpu_out = Reg(new LaneFUOutput)
  //  outArbiter.io.out.bits.outputToLaneFU(fpu_out)
  //  io.out.bits := fpu_out
  io.out.bits.uop := RegEnable(fpu_out_w.uop, regEnable(1))
  io.out.bits.vd := RegEnable(outArbiter.io.out.bits.vd, 0.U, regEnable(1))
  io.out.bits.fflags := RegEnable(outArbiter.io.out.bits.fflags, 0.U, regEnable(1))
  io.out.bits.vxsat := false.B

  // io.out.valid := RegNext(outArbiter.io.out.valid)
  io.maskKeep := RegEnable(outArbiter.io.out.bits.uop.maskKeep, 0.U, regEnable(1))
  io.maskOff := RegEnable(outArbiter.io.out.bits.uop.maskOff, 0.U, regEnable(1))
  // outArbiter.io.out.ready := io.out.ready


  //  outArbiter.io.out.bits.outputToLaneFU(io.out.bits)
  //  // mask result, override
  //  // io.out.bits.vd := outArbiter.io.out.bits.vd &
  //  //   outArbiter.io.out.bits.uop.maskKeep |
  //  //   outArbiter.io.out.bits.uop.maskOff
  //  io.out.bits.vd := outArbiter.io.out.bits.vd
  //
  //  io.out.valid := outArbiter.io.out.valid
  //  io.maskKeep := outArbiter.io.out.bits.uop.maskKeep
  //  io.maskOff := outArbiter.io.out.bits.uop.maskOff
  //  outArbiter.io.out.ready := io.out.ready

  // // IMPORTANT: TEMPORARY SOLUTION FOR NARROWING ERROR IN ISSUE LOGIC
  // val uop = outArbiter.io.out.bits.uop
  // val uop_adjust = uop
  // when (uop.ctrl.narrow) {
  //   uop_adjust.expdLen := Mux(uop.expdLen === 1.U, 1.U, uop.expdLen >> 1)
  //   uop_adjust.expdIdx := uop.expdIdx >> 1
  // }

  val sel_cvt = inputGen.io.out.bits.uop.vfpCtrl.isCvt
  val sel_misc = inputGen.io.out.bits.uop.vfpCtrl.isMisc
  val sel_rec = inputGen.io.out.bits.uop.vfpCtrl.isRec7 || inputGen.io.out.bits.uop.vfpCtrl.isRecSqrt7

  fpu_ready := fma.io.in.ready
  when(sel_cvt) {
    fpu_ready := cvt.io.in.ready
  }.elsewhen(sel_misc) {
    fpu_ready := misc.io.in.ready
  }.elsewhen(sel_rec) {
    fpu_ready := rec.io.in.ready
  }

  //  io.in.ready := fma.io.in.ready
  //  when(sel_cvt) {
  //    io.in.ready := cvt.io.in.ready
  //  }.elsewhen(sel_misc) {
  //    io.in.ready := misc.io.in.ready
  //  }.elsewhen(sel_rec) {
  //    io.in.ready := rec.io.in.ready
  //  }

  // io.in.ready := subModules.map(_.io.in.ready).reduce(_ && _) // must all ready
}


// comb logic
class VFInputGen(implicit val p: Parameters) extends VFPUBaseModule {
  val io = IO(new Bundle() {
    val in = Input(new LaneFUInput)
    val out = Output(new LaneFloatFUIn)
  })

  val ctrl = io.in.uop.ctrl
  val instCat = Cat(ctrl.funct6, ctrl.vm, ctrl.lsrc(1), ctrl.lsrc(0), ctrl.funct3, ctrl.ldest)
  val typeTag = VFPU.getTypeTagFromVSEW(io.in.uop.info.vsew)
  val vfpCtrl = VFDecoder(instCat).io.fpCtrl
  // src expand
  val rs1_unbox = Wire(UInt(32.W))
  rs1_unbox := VFPU.unbox(io.in.rs1, VFPU.S)
  val rs1Expd = Mux(
    ctrl.vx && typeTag === VFPU.S,
    Cat(Seq.fill(2)(rs1_unbox)),
    Mux(ctrl.vx && typeTag === VFPU.D,
      io.in.rs1,
      io.in.vs1)
  )

  //---- vstart >= vl ----
  val vstart_gte_vl = io.in.uop.info.vstart >= io.in.uop.info.vl

  // mask data generation for inactive elements
  val maskGen = Module(new MaskTailData)
  maskGen.io.mask := io.in.mask
  maskGen.io.tail := io.in.tail
  maskGen.io.prestart := io.in.prestart
  maskGen.io.vstart_gte_vl := vstart_gte_vl
  maskGen.io.oldVd := io.in.old_vd
  maskGen.io.uop := io.in.uop
  maskGen.io.opi := false.B

  io.out.connectFromLaneFUInput(io.in)
  io.out.uop.typeTag := typeTag
  io.out.uop.vfpCtrl := vfpCtrl
  io.out.uop.fWidenEnd := false.B // default false
  io.out.vs1 := rs1Expd
  io.out.uop.maskKeep := Mux(isCmp, maskGen.io.maskKeep_cmp, maskGen.io.maskKeep)
  io.out.uop.maskOff := Mux(isCmp, maskGen.io.maskOff_cmp, maskGen.io.maskOff)

  def isCmp: Bool = {
    io.in.uop.ctrl.funct6(5, 3) === "b011".U
  }
}


// comb logic
class VFInputGenFP(implicit val p: Parameters) extends VFPUBaseModule {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val redirect = Input(ValidIO(new Redirect))
    val out = DecoupledIO(new LaneFloatFUIn)
  })

  val fire = io.in.fire
  val ctrl = io.in.bits.uop.ctrl
  val narrow_to_1 = ctrl.narrow_to_1
  val instCat = Cat(ctrl.funct6, ctrl.vm, ctrl.lsrc(1), ctrl.lsrc(0), ctrl.funct3, ctrl.ldest)
  val typeTag = VFPU.getTypeTagFromVSEW(io.in.bits.uop.info.vsew)
  val vfpCtrl = VFDecoder(instCat).io.fpCtrl
  // src expand
  val rs1Expd = Mux(
    ctrl.vx && typeTag === VFPU.S,
    Cat(Seq.fill(2)(io.in.bits.rs1.tail(VFPU.f32.len))),
    Mux(ctrl.vx && typeTag === VFPU.D,
      io.in.bits.rs1,
      io.in.bits.vs1)
  )

  //  Redirect handling
  def latency = 1


  val validVec = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  val rdyVec = Array.fill(latency)(Wire(Bool())) :+ io.out.ready
  val uopVec = io.in.bits.uop +: Array.fill(latency)(Reg(new VExpdUOp))
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

  io.out.valid := validVec.last & !io.out.bits.uop.sysUop.robIdx.needFlush(io.redirect)
  io.in.ready := rdyVec(0)

  //---- vstart >= vl ----
  val vstart_gte_vl = io.in.bits.uop.info.vstart >= io.in.bits.uop.info.vl

  // mask data generation for inactive elements
  val maskGen = Module(new MaskTailData)
  maskGen.io.mask := io.in.bits.mask
  maskGen.io.tail := io.in.bits.tail
  maskGen.io.prestart := io.in.bits.prestart
  maskGen.io.vstart_gte_vl := vstart_gte_vl
  maskGen.io.oldVd := io.in.bits.old_vd
  maskGen.io.uop := io.in.bits.uop
  maskGen.io.opi := false.B

  val fuop = Reg(new LaneFloatFUIn)
  when(regEnable(1)) {
    fuop.connectFromLaneFUInput(io.in.bits)
  }

  io.out.bits := fuop
  io.out.bits.uop.typeTag := RegEnable(typeTag, regEnable(1))
  io.out.bits.uop.vfpCtrl := RegEnable(vfpCtrl, regEnable(1))
  io.out.bits.uop.fWidenEnd := false.B // default false
  io.out.bits.vs1 := RegEnable(rs1Expd, regEnable(1))
  io.out.bits.uop.maskKeep := RegEnable(Mux(narrow_to_1 & !vstart_gte_vl, maskGen.io.maskKeep_cmp, maskGen.io.maskKeep), regEnable(1))
  io.out.bits.uop.maskOff := RegEnable(Mux(narrow_to_1 & !vstart_gte_vl, maskGen.io.maskOff_cmp, maskGen.io.maskOff), regEnable(1))

}



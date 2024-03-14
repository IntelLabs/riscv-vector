package darecreek.exu.vfucore.reduction

import chipsalliance.rocketchip.config.Parameters
import chisel3.{util, _}
import chisel3.util._
import darecreek.exu.vfucore.fp._
import darecreek.exu.vfucore.MaskTailData
import darecreek.exu.vfucore.{LaneFUInput, LaneFUOutput}
import darecreek.exu.vfucoreconfig.{VUop, Redirect}

class VRedFPU(implicit val p: Parameters)
  extends VFPUBaseModule {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val redirect = Input(new Redirect)
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

  val fadd = Module(new VFADD)
  val misc = Module(new VFMisc)
  // output priority high to low
  val subModules = Seq(fadd, misc)
  subModules.foreach(m => {
    m.io.in.bits := inputGen.io.out.bits
    m.io.redirect := io.redirect
    m.io.in.valid := inputGen.io.out.valid
  })

  // output arbiter ,  rec > fadd > cvt > misc
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
  val uopVec = fpu_out_w.uop +: Array.fill(latency)(Reg(new VUop))
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

  io.out.valid := validVec.last
  outArbiter.io.out.ready := rdyVec(0)

  io.out.bits.uop := RegEnable(fpu_out_w.uop, regEnable(1))
  io.out.bits.vd := RegEnable(outArbiter.io.out.bits.vd, 0.U, regEnable(1))
  io.out.bits.fflags := RegEnable(outArbiter.io.out.bits.fflags, 0.U, regEnable(1))
  io.out.bits.vxsat := false.B

  // io.out.valid := RegNext(outArbiter.io.out.valid)
  io.maskKeep := RegEnable(outArbiter.io.out.bits.uop.maskKeep, 0.U, regEnable(1))
  io.maskOff := RegEnable(outArbiter.io.out.bits.uop.maskOff, 0.U, regEnable(1))

  val sel_misc = inputGen.io.out.bits.uop.vfpCtrl.isMisc

  fpu_ready := fadd.io.in.ready
  when(sel_misc) {
    fpu_ready := misc.io.in.ready
  }

}

class VFInputGenFP(implicit val p: Parameters) extends VFPUBaseModule {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val redirect = Input(new Redirect)
    val out = DecoupledIO(new LaneFloatFUIn)
  })

  val fire = io.in.fire
  val ctrl = io.in.bits.uop.ctrl
  val narrow_to_1 = ctrl.narrow_to_1
  val instCat = Cat(ctrl.funct6, ctrl.vm, ctrl.lsrc(1), ctrl.lsrc(0), ctrl.funct3, ctrl.ldest)
  val typeTag = VFPU.getTypeTagFromVSEW(io.in.bits.uop.info.vsew)
  val vfpCtrl = VFDecoder(instCat).io.fpCtrl
  // src expand

  val rs1_unbox = Wire(UInt(32.W))
  rs1_unbox := VFPU.unbox(io.in.bits.rs1, VFPU.S)

  val rs1Expd = Mux(
    ctrl.vx && typeTag === VFPU.S,
    Cat(Seq.fill(2)(rs1_unbox)),
    Mux(ctrl.vx && typeTag === VFPU.D,
      io.in.bits.rs1,
      io.in.bits.vs1)
  )

  //  Redirect handling
  def latency = 1


  val validVec = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  val rdyVec = Array.fill(latency)(Wire(Bool())) :+ io.out.ready
  val uopVec = io.in.bits.uop +: Array.fill(latency)(Reg(new VUop))
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

  // io.out.valid := validVec.last & !io.out.bits.uop.sysUop.robIdx.needFlush(io.redirect)
  io.out.valid := validVec.last & !io.redirect.needFlush(io.out.bits.uop.robIdx)
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



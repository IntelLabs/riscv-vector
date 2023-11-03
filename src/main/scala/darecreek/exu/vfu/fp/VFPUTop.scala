package darecreek.exu.vfu.fp

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import darecreek.exu.vfu.{LaneFUInput, LaneFUOutput}
import darecreek.exu.vfu.MaskTailData
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
  val inputGen = Module(new VFInputGen)
  inputGen.io.in.bits := io.in.bits
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

  val fpu_out = Reg(new LaneFUOutput)
  outArbiter.io.out.bits.outputToLaneFU(fpu_out)

  io.out.bits := fpu_out
  io.out.bits.vd := RegEnable(outArbiter.io.out.bits.vd, 0.U, outArbiter.io.out.valid)

  io.out.valid := RegNext(outArbiter.io.out.valid)
  io.maskKeep := RegEnable(outArbiter.io.out.bits.uop.maskKeep, 0.U, outArbiter.io.out.valid)
  io.maskOff := RegEnable(outArbiter.io.out.bits.uop.maskOff, 0.U, outArbiter.io.out.valid)
  outArbiter.io.out.ready := io.out.ready


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
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val out = DecoupledIO(new LaneFloatFUIn)
  })

  val fire = io.in.fire
  val ctrl = io.in.bits.uop.ctrl
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
  fuop.connectFromLaneFUInput(io.in.bits)

  io.out.bits := fuop
  io.out.bits.uop.typeTag := RegEnable(typeTag, fire)
  io.out.bits.uop.vfpCtrl := RegEnable(vfpCtrl, fire)
  io.out.bits.uop.fWidenEnd := false.B // default false
  io.out.bits.vs1 := RegEnable(rs1Expd, fire)
  io.out.bits.uop.maskKeep := RegEnable(Mux(isCmp, maskGen.io.maskKeep_cmp, maskGen.io.maskKeep), fire)
  io.out.bits.uop.maskOff := RegEnable(Mux(isCmp, maskGen.io.maskOff_cmp, maskGen.io.maskOff), fire)

  io.out.valid := RegNext(io.in.valid)
  io.in.ready := io.out.ready
  //  io.out.bits.connectFromLaneFUInput(io.in.bits)
  //  io.out.bits.uop.typeTag := typeTag
  //  io.out.bits.uop.vfpCtrl := vfpCtrl
  //  io.out.bits.uop.fWidenEnd := false.B // default false
  //  io.out.bits.vs1 := rs1Expd
  //  io.out.bits.uop.maskKeep := Mux(isCmp, maskGen.io.maskKeep_cmp, maskGen.io.maskKeep)
  //  io.out.bits.uop.maskOff := Mux(isCmp, maskGen.io.maskOff_cmp, maskGen.io.maskOff)

  def isCmp: Bool = {
    io.in.bits.uop.ctrl.funct6(5, 3) === "b011".U
  }
}



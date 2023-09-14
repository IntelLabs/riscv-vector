package darecreek.exu.fp

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{Arbiter, Cat, DecoupledIO}
import darecreek.{LaneFUInput, LaneFUOutput}
import darecreek.exu.fu.alu.MaskTailData

class VFPUTop(implicit val p: Parameters = (new WithVFPUConfig).toInstance)
  extends VFPUBaseModule with HasVFPUParams {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val out = DecoupledIO(new LaneFUOutput)
  })
  val inputGen = Module(new VFInputGen)
  inputGen.io.in := io.in.bits
  val fma = Module(new VFFMA)
  val cvt = Module(new VFCVT)
  val misc = Module(new VFMisc)
  val rec = Module(new VFRec)
  // output priority high to low
  val subModules = Seq(fma, cvt, misc, rec)
  subModules.foreach(m => {
    m.io.in.bits := inputGen.io.out
    m.io.in.valid := io.in.valid
  })

  // output arbiter ,  rec > fma > cvt > misc
  val outArbiter = Module(new Arbiter(new LaneFloatFUOut, subModules.length))
  outArbiter.io.in.zip(subModules.map(_.io.out)).foreach {
    case (in, out) => in <> out
  }
  outArbiter.io.out.bits.outputToLaneFU(io.out.bits)
  // mask result, override
  io.out.bits.vd := outArbiter.io.out.bits.vd &
    outArbiter.io.out.bits.uop.maskKeep |
    outArbiter.io.out.bits.uop.maskOff
  io.out.valid := outArbiter.io.out.valid
  outArbiter.io.out.ready := io.out.ready

  // // IMPORTANT: TEMPORARY SOLUTION FOR NARROWING ERROR IN ISSUE LOGIC
  // val uop = outArbiter.io.out.bits.uop
  // val uop_adjust = uop
  // when (uop.ctrl.narrow) {
  //   uop_adjust.expdLen := Mux(uop.expdLen === 1.U, 1.U, uop.expdLen >> 1)
  //   uop_adjust.expdIdx := uop.expdIdx >> 1
  // }

  io.in.ready := subModules.map(_.io.in.ready).reduce(_ && _) // must all ready
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
  val rs1Expd = Mux(
    ctrl.vx && typeTag === VFPU.S,
    Cat(Seq.fill(2)(io.in.rs1.tail(VFPU.f32.len))),
    io.in.vs1
  )

  // mask data generation for inactive elements
  val maskGen = Module(new MaskTailData)
  maskGen.io.mask := io.in.mask
  maskGen.io.tail := io.in.tail
  maskGen.io.oldVd := io.in.old_vd
  maskGen.io.uop := io.in.uop
  maskGen.io.opi := false.B

  io.out.connectFromLaneFUInput(io.in)
  io.out.uop.typeTag := typeTag
  io.out.uop.vfpCtrl := vfpCtrl
  io.out.uop.fWidenEnd := false.B  // default false
  io.out.vs1 := rs1Expd
  io.out.uop.maskKeep := Mux(isCmp, maskGen.io.maskKeep_cmp, maskGen.io.maskKeep)
  io.out.uop.maskOff := Mux(isCmp, maskGen.io.maskOff_cmp, maskGen.io.maskOff)

  def isCmp: Bool = {
    io.in.uop.ctrl.funct6(5,3) === "b011".U
  }
}



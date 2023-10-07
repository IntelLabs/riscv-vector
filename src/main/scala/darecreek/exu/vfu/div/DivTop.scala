package darecreek.exu.vfu.div

import org.chipsalliance.cde.config.{Config, Field, Parameters}
import chisel3._
import chisel3.util._
// import darecreek.{BundleHelper, DarecreekParameters, LaneFUInput, LaneFUOutput, LaneWidth, xLen}
// import darecreek.exu.fp.{LANE_WIDTH, NAME, VFDivSqrt, VFInputGen, VFPUBaseModule, XLEN}
// import darecreek.exu.fu.alu.MaskTailData
// import darecreek.exu.fp._
// import darecreek.exu.vfu.DarecreekParamVFU._
// import darecreek.exu.vfu.fp.{LANE_WIDTH, NAME, VFDivSqrt, VFInputGen, VFPUBaseModule, XLEN}
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu._

//case object NAME extends Field[String]
//case object LANE_WIDTH extends Field[Int]
//case object XLEN extends Field[Int]
//
// class VDivConfig extends Config((site, here, up) => {
//   case NAME => "VDIV"
//   case LANE_WIDTH => LaneWidth
//   case XLEN => xLen
// }) with DarecreekParametersVFU

// trait HasDivParams {
//   implicit val p: Parameters
//   //  val name = p(NAME)
//   val laneWidth = p(LANE_WIDTH)
//   val xLen = p(XLEN)
// }

// comb logic
class VDivInputGen(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val in = Input(new LaneFUInput)
    val out = Output(new LaneFUWithMaskIn)
  })
  val ctrl = io.in.uop.ctrl
  val sewOH = UIntToOH(io.in.uop.info.vsew)
  // src expand
  val expdSigs = VecInit(Seq.tabulate(4)( i =>
    Cat(Seq.fill(8 >> i)(io.in.rs1((8 << i) - 1 ,0))
  )))
  val rs1Expd = Mux(ctrl.vx, Mux1H(sewOH, expdSigs), io.in.vs1)

  //---- vstart >= vl ----
  val vstart_gte_vl = io.in.uop.info.vstart >= io.in.uop.info.vl

//   mask data generation for inactive elements
  val maskGen = Module(new MaskTailData)
  maskGen.io.mask := io.in.mask
  maskGen.io.tail := io.in.tail
  maskGen.io.prestart := io.in.prestart
  maskGen.io.vstart_gte_vl := vstart_gte_vl
  maskGen.io.oldVd := io.in.old_vd
  maskGen.io.uop := io.in.uop
  maskGen.io.opi := false.B

  io.out.connectFromLaneFUInput(io.in)
  io.out.vs1 := rs1Expd
  io.out.uop.maskKeep := maskGen.io.maskKeep
  io.out.uop.maskOff := maskGen.io.maskOff

}

class DivTop(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val out = DecoupledIO(new LaneFUOutput)
  })
  val inputGen = Module(new VDivInputGen)
  inputGen.io.in := io.in.bits
  val FinputGen = Module(new VFInputGen)
  FinputGen.io.in := io.in.bits

  val idiv = Module(new VIntSRT16TimeplexDivider)
  val fdivsqrt = Module(new VFDivSqrt)

  val uop = io.in.bits.uop
  idiv.io.in.bits := inputGen.io.out
  // idiv.io.in.valid := io.in.valid && uop.ctrl.div && !uop.ctrl.fp
  idiv.io.in.valid := io.in.valid && uop.ctrl.funct3(1, 0) === 2.U //OPMVV/VX
//  idiv.io.out.ready := io.out.ready
  fdivsqrt.io.in.bits := FinputGen.io.out
  // fdivsqrt.io.in.valid := io.in.valid && uop.ctrl.div && uop.ctrl.fp
  fdivsqrt.io.in.valid := io.in.valid && uop.ctrl.funct3(1, 0) === 1.U //OPFVV/VF
//  fdivsqrt.io.out.ready := io.out.ready

  val fdivResult = Wire(new LaneFUOutput)
  fdivsqrt.io.out.bits.outputToLaneFU(fdivResult)
  fdivResult.vd := fdivsqrt.io.out.bits.vd &
    fdivsqrt.io.out.bits.uop.maskKeep |
    fdivsqrt.io.out.bits.uop.maskOff

  val idivResult = Wire(new LaneFUOutput)
  idiv.io.out.bits.outputToLaneFU(idivResult)
  idivResult.vd := idiv.io.out.bits.vd &
    idiv.io.out.bits.uop.maskKeep |
    idiv.io.out.bits.uop.maskOff

  // output arbiter
  io.out.bits := Mux(idiv.io.out.valid, idivResult, fdivResult)
  io.out.valid := idiv.io.out.valid | fdivsqrt.io.out.valid

  // prioritize idiv
  idiv.io.out.ready := io.out.ready
  fdivsqrt.io.out.ready := !idiv.io.out.valid && io.out.ready

  io.in.ready := idiv.io.in.ready && fdivsqrt.io.in.ready // must all ready
}

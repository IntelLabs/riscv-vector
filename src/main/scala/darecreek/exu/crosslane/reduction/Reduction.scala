package darecreek.exu.crosslane.reduction

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.reduction._
import darecreek.exu.vfucoreconfig.{VUop, Redirect}
import darecreek.exu.vfucore.{VFuModule, VFuParamsKey, VFuParameters}
import darecreek._

class Reduction(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VExuInput))
    val redirect = Input(new Redirect)
    val out = Decoupled(new VCrossExuOut)
  })

  val funct6 = io.in.bits.uop.ctrl.funct6
  val funct3 = io.in.bits.uop.ctrl.funct3


  val vfredosum_vs = ((funct6 === "b000011".U) && (funct3 === "b001".U)) || ((funct6 === "b000001".U) && (funct3 === "b001".U))
  // val vfredusum_vs = (funct6 === "b000001".U) && (funct3 === "b001".U)
  val vfredusum_vs = false.B
  val vfredmax_vs = (funct6 === "b000111".U) && (funct3 === "b001".U)
  val vfredmin_vs = (funct6 === "b000101".U) && (funct3 === "b001".U)
  val vfwredosum_vs = ((funct6 === "b110011".U) && (funct3 === "b001".U)) || ((funct6 === "b110001".U) && (funct3 === "b001".U))
  // val vfwredusum_vs = (funct6 === "b110001".U) && (funct3 === "b001".U)
  val vfwredusum_vs = false.B

  val fp_red = vfredosum_vs ||
    vfredusum_vs ||
    vfredmax_vs ||
    vfredmin_vs ||
    vfwredosum_vs ||
    vfwredusum_vs


  val redint = Module(new ReductionInt()(p))
  val redfp = Module(new ReductionFP()(p))


  io.in.ready := redint.io.in.ready && redfp.io.in.ready

  redint.io.in.bits := io.in.bits
  redint.io.in.valid := io.in.valid && io.in.ready && !fp_red
  redint.io.redirect := io.redirect

  redfp.io.in.bits := io.in.bits
  redfp.io.in.valid := io.in.valid && io.in.ready && fp_red
  redfp.io.redirect := io.redirect

  val arb = Module(new Arbiter(new VCrossExuOut, 2))
  arb.io.in(0) <> redint.io.out
  arb.io.in(1) <> redfp.io.out
  io.out <> arb.io.out
}

object VerilogRed extends App {
  println("Generating hardware")
  val p = Parameters.empty
  emitVerilog(new Reduction()(p.alterPartial({ case VFuParamsKey =>
    VFuParameters(VLEN = 256)
  })), Array("--target-dir", "generated",
    "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}
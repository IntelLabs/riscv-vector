package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.crosslane.perm._
import darecreek.exu.crosslane.vmask._
import darecreek.exu.crosslane.reduction._
import darecreek.exu.crosslane.div._
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.{VFuModule, VFuParamsKey, VFuParameters}
import darecreek._
import darecreek.exu.lanevfu.RedirectConvert

// For permutation read register file
class PermRdRF extends Bundle {
  val rd_en = Output(Bool())
  val rd_preg_idx = Output(UInt(VPRegIdxWidth.W))
  val rdata = Input(UInt(VLEN.W))
  val rvalid = Input(Bool())
}

class SVCrossLaneExu extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val bits = Input(new VExuInput)
      val valid = Input(Bool())
      val readys = Output(Vec(NCrossLaneFUs, Bool()))
    }
    val redirect = Input(new Redirect)
    val out = Decoupled(new VCrossExuOut)
    val perm = new PermRdRF
  })

  implicit val p = Parameters.empty.alterPartial({
    case VFuParamsKey => VFuParameters(VLEN = VLEN)
  })

  val permutation = Module(new Permutation)
  val vmask = Module(new VMaskDC)
  val reduction = Module(new Reduction)
  val div = Module(new VDiv)

  reduction.io.in.bits := io.in.bits
  reduction.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.redu
  reduction.io.redirect := io.redirect
  io.in.readys(0) := reduction.io.in.ready

  vmask.io.in.bits := io.in.bits
  vmask.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.mask
  io.in.readys(1) := vmask.io.in.ready

  permutation.io.in.bits := io.in.bits
  permutation.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.perm
  permutation.io.redirect := io.redirect
  io.in.readys(2) := permutation.io.in.ready
  io.perm <> permutation.io.perm

  div.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.div
  div.io.in.bits.uop := io.in.bits.uop
  div.io.in.bits.rs1 := io.in.bits.rs1
  div.io.in.bits.vs1 := io.in.bits.vSrc(0).asUInt
  div.io.in.bits.vs2 := io.in.bits.vSrc(1).asUInt
  div.io.in.bits.oldVd := io.in.bits.vSrc(2).asUInt
  div.io.in.bits.mask := io.in.bits.vSrc(3).asUInt
  div.io.redirect := RedirectConvert(io.redirect)
  io.in.readys(3) := div.io.in.ready

  val arb = Module(new Arbiter(new VCrossExuOut, NCrossLaneFUs))
  arb.io.in(0) <> permutation.io.out
  arb.io.in(1) <> reduction.io.out
  arb.io.in(2) <> vmask.io.out
  arb.io.in(3).valid := div.io.out.valid
  arb.io.in(3).bits.uop := div.io.out.bits.uop
  arb.io.in(3).bits.vd := VecInit(UIntSplit(div.io.out.bits.vd))
  arb.io.in(3).bits.fflags := div.io.out.bits.fflags
  div.io.out.ready := arb.io.in(3).ready

  io.out <> arb.io.out
}

//object VerilogCross extends App {
//  println("Generating the VPU CrossLane hardware")
//  emitVerilog(new VCrossLaneExu(), Array("--target-dir", "generated"))
//}

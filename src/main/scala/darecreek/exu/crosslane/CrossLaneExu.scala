package darecreek

import chisel3._
import chisel3.util._
import darecreek.exu.crosslane.perm._
import darecreek.exu.crosslane.vmask._
import darecreek.exu.crosslane.reduction._
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.{VFuModule, VFuParamsKey, VFuParameters}

class VCrossLaneExu extends Module {

  val io = IO(new Bundle {
    val in = new Bundle {
      val bits = Input(new VExuInput)
      val valid = Input(Bool())
      val readys = Output(Vec(3, Bool()))
    }
    val out = Decoupled(new VCrossExuOut)

    val redirect = Input(new Redirect)
    // For permutation read register file
    val perm = new Bundle {
      val rd_en = Output(Bool())
      val rd_preg_idx = Output(UInt(8.W))
      val rdata = Input(UInt(VLEN.W))
      val rvalid = Input(Bool())
    }
  })

  implicit val p = Parameters.empty.alterPartial({
    case VFuParamsKey => VFuParameters(VLEN = 256)
  })

  val permutation = Module(new Permutation)
  val vmask = Module(new VMaskDC()(p))
  val reduction = Module(new Reduction()(p))

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
  permutation.io.perm.rdata := io.perm.rdata
  permutation.io.perm.rvalid := io.perm.rvalid
  io.perm.rd_en := permutation.io.perm.rd_en
  io.perm.rd_preg_idx := permutation.io.perm.rd_preg_idx

  val arb = Module(new Arbiter(new VCrossExuOut, 3))
  arb.io.in(0) <> permutation.io.out
  arb.io.in(1) <> reduction.io.out
  arb.io.in(2) <> vmask.io.out
  io.out <> arb.io.out
}

object VerilogCross extends App {
  println("Generating the VPU CrossLane hardware")
  emitVerilog(new VCrossLaneExu(), Array("--target-dir", "generated"))
}



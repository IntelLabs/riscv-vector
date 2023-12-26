package darecreek

import chisel3._
import chisel3.util._

class VCrossLaneExu extends Module {

  val io = IO(new Bundle {
    val in = new Bundle {
      val bits = Input(new VExuInput)
      val valid = Input(Bool())
      val readys = Output(Vec(3, Bool()))
    }
    val out = Decoupled(new VCrossExuOut)
  })

  val permutation = Module(new Permutation)
  val vmask = Module(new VMask)
  val reduction = Module(new Reduction)

  reduction.io.in.bits := io.in.bits
  reduction.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.redu
  io.in.readys(0) := reduction.io.in.ready
 
  vmask.io.in.bits := io.in.bits
  vmask.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.mask
  io.in.readys(1) := vmask.io.in.ready
 
  permutation.io.in.bits := io.in.bits
  permutation.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.perm
  io.in.readys(2) := permutation.io.in.ready

  val arb = Module(new Arbiter(new VCrossExuOut, 3))
  arb.io.in(0) <> reduction.io.out
  arb.io.in(1) <> vmask.io.out
  arb.io.in(2) <> permutation.io.out
  io.out <> arb.io.out  
}

object VerilogCross extends App {
  println("Generating the VPU CrossLane hardware")
  emitVerilog(new VCrossLaneExu(), Array("--target-dir", "generated"))
}



/**
  * Vector Lane: 64-bit data-path of FUs
  */

package darecreek

import chisel3._
import chisel3.util._
import darecreek.exu.fp._
import darecreek.exu.fu.alu._
import darecreek.exu.fu.mac._
import darecreek.exu.fu.div._

class DummyLaneFU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LaneFUInput))
    val out = Decoupled(new LaneFUOutput)
  })

  io.out.bits.uop := io.in.bits.uop
  io.out.bits.vd := 0.U
  io.out.valid := false.B
  io.out.bits.fflags := 0.U 
  io.out.bits.vxsat := false.B
  io.in.ready := io.out.ready
}

class VLane extends Module{
  val io = IO(new Bundle {
    val idx = Input(UInt(LaneIdxWidth.W))
    val in = new Bundle {
      val data = Input(new LaneFUInput)
      val valids = Input(Vec(NLaneExuFUs, Bool()))
      val readys = Output(Vec(NLaneExuFUs, Bool()))
    }
    val out = Decoupled(new LaneFUOutput)
  })

  // ALU
  val valu = Module(new VAlu)
  // val valu = Module(new DummyLaneFU)
  // MUL
  val vmac = Module(new VIMac)
  // val vmac = Module(new DummyLaneFU)
  // FP
  // val vfp = Module(new VFPUTop)
  val vfp = Module(new DummyLaneFU)
  // fake div
  // val vdiv = Module(new DivTop)
  val vdiv = Module(new DummyLaneFU)

  // Input of ALU
  valu.io.in.bits := io.in.data
  valu.io.in.valid := io.in.valids(0)
  io.in.readys(0) := valu.io.in.ready
  // Input of MUL
  vmac.io.in.bits := io.in.data
  vmac.io.in.valid := io.in.valids(1)
  io.in.readys(1) := vmac.io.in.ready
  // Input of FP
  vfp.io.in.bits := io.in.data
  vfp.io.in.valid := io.in.valids(2)
  io.in.readys(2) := vfp.io.in.ready
  // Input of div
  vdiv.io.in.bits := io.in.data
  vdiv.io.in.valid := io.in.valids(3)
  io.in.readys(3) := vdiv.io.in.ready

  /**
    * Output arbiter
    */
  val arb = Module(new Arbiter(new LaneFUOutput, 4))
  arb.io.in(0) <> valu.io.out
  arb.io.in(1) <> vmac.io.out
  arb.io.in(2) <> vfp.io.out
  arb.io.in(3) <> vdiv.io.out
  io.out <> arb.io.out  
}
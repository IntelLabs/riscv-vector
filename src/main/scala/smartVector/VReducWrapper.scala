package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.alu.VAlu
import firrtl.Utils
import darecreek.exu.vfu.reduction


class VReducWrapper (implicit p : Parameters) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new VFuInput))
    val out = ValidIO(new VAluOutput)
    val age     = Output(UInt(4.W))
  })

  val vReduc = Module(new reduction.Reduction)

  vReduc.io.in.valid := io.in.valid
  vReduc.io.in.bits := io.in.bits

  io.out := vReduc.io.out
}

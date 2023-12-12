package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.alu.VAlu
import firrtl.Utils


class VAluWrapper (implicit p : Parameters) extends VFuModule {

  val io = IO(new Bundle {
    val in      = Input(ValidIO(new VFuInput))
    val out     = ValidIO(new VAluOutput)
  })

  val vAlu = Module(new VAlu)

  vAlu.io.in.valid := io.in.valid
  vAlu.io.in.bits  := io.in.bits

  io.out := vAlu.io.out
}



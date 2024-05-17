package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.alu.VAlu
import firrtl.Utils
import darecreek.exu.vfu.mac.VMac


class VMacWrapper (implicit p : Parameters) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new VFuInput))
    val out = ValidIO(new VAluOutput)
    val age     = Output(UInt(4.W))
  })

  val vMac = Module(new VMac)

  vMac.io.in.valid := io.in.valid
  vMac.io.in.bits := io.in.bits

  io.out := vMac.io.out
}

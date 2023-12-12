package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import firrtl.Utils
import darecreek.exu.vfu.div.VDiv
import xiangshan.Redirect


class VDivWrapper (implicit p : Parameters) extends Module {

  val io = IO(new Bundle {
    val in  = Flipped(Decoupled((new VFuInput)))
    val out = ValidIO(new VAluOutput)
  })

  val vDiv = Module(new VDiv)

  vDiv.io.in.valid := io.in.valid
  vDiv.io.redirect.valid := false.B 
  vDiv.io.redirect.bits := DontCare
  vDiv.io.in.bits  := io.in.bits

  io.out := vDiv.io.out
  io.in.ready := vDiv.io.in.ready
}

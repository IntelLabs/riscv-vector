package darecreek.exu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import darecreek.{LaneFUInput, LaneFUOutput}

class LaneUnit(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val out = DecoupledIO(new LaneFUOutput)
  })
}

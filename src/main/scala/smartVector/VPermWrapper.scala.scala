package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import firrtl.Utils
import darecreek.exu.vfu.div.VDiv
import xiangshan.Redirect
import darecreek.exu.vfu.perm.Permutation


class VPermWrapper (implicit p : Parameters) extends Module {

  val io = IO(new Bundle {
    val in = Input(new VPermInput)
    val out = Output(new VPermOutput)
  })

  val vPerm = Module(new Permutation)

}

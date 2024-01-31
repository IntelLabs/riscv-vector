package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.alu.VAlu
import firrtl.Utils


class VSFPUWrapper (implicit p : Parameters) extends VFuModule {

  val io = IO(new Bundle {
    val in      = Input(ValidIO(new VFuInput))
    val out = ValidIO(new VFpuOutput)
  })

  val vFPu = Module(new fp.VFPUWrapper)

  vFPu.io.in.valid := RegEnable(io.in.valid, vFPu.io.in.ready)
  vFPu.io.in.bits  := RegEnable(io.in.bits, vFPu.io.in.valid)
  vFPu.io.redirect.valid := false.B 
  vFPu.io.redirect.bits := DontCare
  vFPu.io.out.ready := true.B
  

  io.out.valid := vFPu.io.out.valid
  io.out.bits  := vFPu.io.out.bits
}

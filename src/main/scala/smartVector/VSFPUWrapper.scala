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
    val in  = Flipped(DecoupledIO(new VFuInput))
    val out = ValidIO(new VFpuOutput)
  })

  val vFPu = Module(new fp.VFPUWrapper)
  
  val fpuReqReg = RegInit(0.U.asTypeOf(new VFuInput))
  val fpuReqValidReg = RegInit(false.B)

  when(io.in.valid){
    fpuReqValidReg := true.B
    fpuReqReg := io.in.bits
  }.elsewhen(vFPu.io.in.ready && fpuReqValidReg) {
    fpuReqValidReg := false.B
  }

  val fpuValid = fpuReqValidReg
  val fpuReq = fpuReqReg

  vFPu.io.in.valid := fpuValid
  vFPu.io.in.bits  := fpuReq
  vFPu.io.redirect.valid := false.B 
  vFPu.io.redirect.bits := DontCare
  vFPu.io.out.ready := true.B
  
  io.in.ready := vFPu.io.in.ready
  io.out.valid := vFPu.io.out.valid
  io.out.bits  := vFPu.io.out.bits
}

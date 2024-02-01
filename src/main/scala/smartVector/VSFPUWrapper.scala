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

  //val validReg = RegInit(false.B)
  //val bitsReg = RegInit(0.U.asTypeOf(new VFuInput))
  //
  //when(io.in.valid && vFPu.io.in.ready){
  //  vFPu.io.in.valid := true.B
  //  vFPu.io.in.bits  := io.in.bits
  //}.elsewhen(io.in.valid && ~vFPu.io.in.ready){
  //  vFPu.io.in.valid := false.B
  //  vFPu.io.in.bits  := io.in.bits
  //  validReg := true.B
  //  bitsReg := io.in.bits
  //}.elsewhen(~io.in.valid && validReg && vFPu.io.in.ready){
  //  vFPu.io.in.valid := true.B
  //  vFPu.io.in.bits  := bitsReg
  //  validReg := false.B
  //}.otherwise{
  //  vFPu.io.in.valid := false.B
  //  vFPu.io.in.bits  := io.in.bits
  //  validReg := false.B
  //}
  
  vFPu.io.in.valid := io.in.valid 
  vFPu.io.in.bits  := io.in.bits
  vFPu.io.redirect.valid := false.B 
  vFPu.io.redirect.bits := DontCare
  vFPu.io.out.ready := true.B
  
  io.in.ready := vFPu.io.in.ready
  io.out.valid := vFPu.io.out.valid
  io.out.bits  := vFPu.io.out.bits
}

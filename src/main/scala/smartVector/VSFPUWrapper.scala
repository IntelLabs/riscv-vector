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
    val out = Output(ValidIO(new IexOut))
  })

  val vFPu = Module(new fp.VFPUWrapper)
  
  val fpuReqReg = RegInit(0.U.asTypeOf(new VFuInput))
  val fpuReqValidReg = RegInit(false.B)

  when(io.in.valid){
    fpuReqValidReg := true.B
    fpuReqReg := io.in.bits
  }.elsewhen(vFPu.io.in.ready && fpuReqValidReg){
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

  val fflagsBuffer   = RegInit(0.U(5.W))
  //val fflagsBufferIn = Wire(UInt(5.W))

  when(vFPu.io.out.valid){
      io.out.bits.toRegFileWrite.rfWriteEn         := vFPu.io.out.bits.uop.sysUop.rfWriteEn
      io.out.bits.toRegFileWrite.rfWriteMask       := Fill(128/8, 0.U)
      io.out.bits.toRegFileWrite.rfWriteIdx        := vFPu.io.out.bits.uop.sysUop.ldest
      io.out.bits.toRegFileWrite.rfWriteData       := vFPu.io.out.bits.vd
      io.out.bits.commitInfo.valid                 := vFPu.io.out.bits.uop.uopEnd
      io.out.bits.commitInfo.bits.scalarRegWriteEn := vFPu.io.out.bits.uop.sysUop.scalarRegWriteEn
      io.out.bits.commitInfo.bits.floatRegWriteEn  := vFPu.io.out.bits.uop.sysUop.floatRegWriteEn
      io.out.bits.commitInfo.bits.ldest            := vFPu.io.out.bits.uop.sysUop.ldest
      io.out.bits.commitInfo.bits.data             := vFPu.io.out.bits.vd
      io.out.bits.commitInfo.bits.fflags           := fflagsBuffer | vFPu.io.out.bits.fflags
      fflagsBuffer                                 := fflagsBuffer | vFPu.io.out.bits.fflags
  }.otherwise{
      io.out.bits.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
      io.out.bits.commitInfo     := 0.U.asTypeOf(Valid(new CommitInfo))
  }

  //when(vFPu.io.out.valid & !vFPu.io.out.bits.uop.uopEnd){
  //    fflagsBufferIn := false.B
  //}
  io.out.bits.commitInfo.bits.vxsat := false.B

}

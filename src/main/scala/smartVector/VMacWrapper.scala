package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import firrtl.Utils
import darecreek.exu.vfu.mac.VMac


class VMacWrapper (implicit p : Parameters) extends Module {

  val io = IO(new Bundle {
    val in = Input(ValidIO(new VFuInput))
    val mergeInfo = Input(new MuopMergeAttr)
    val out       = Output(ValidIO(new IexOut)) 
  })

  val vMac = Module(new VMac)

  vMac.io.in.valid := io.in.valid
  vMac.io.in.bits := io.in.bits

  val rfWriteEn         = RegNext(RegEnable(io.mergeInfo.rfWriteEn,        io.in.valid))
  val rfWriteIdx        = RegNext(RegEnable(io.mergeInfo.ldest,            io.in.valid))
  val scalarRegWriteEn  = RegNext(RegEnable(io.mergeInfo.scalarRegWriteEn, io.in.valid))
  val floatRegWriteEn   = RegNext(RegEnable(io.mergeInfo.floatRegWriteEn,  io.in.valid))
  val scalarRegWriteIdx = RegNext(RegEnable(io.mergeInfo.ldest,            io.in.valid))
  val muopEnd           = RegNext(RegEnable(io.mergeInfo.muopEnd,          io.in.valid))
  val regDstIdx         = RegNext(RegEnable(io.mergeInfo.regDstIdx,        io.in.valid))
  //val data              = UInt(128.W)
  val regDataBuffer     = RegInit(0.U(128.W))
  val vxsat             = Bool()
  val vxsatBuffer       = RegInit(false.B)
  
  io.out.bits.toRegFileWrite.rfWriteEn   := rfWriteEn
  io.out.bits.toRegFileWrite.rfWriteMask := Fill(128/8, 0.U)
  io.out.bits.toRegFileWrite.rfWriteIdx  := rfWriteIdx
  io.out.bits.toRegFileWrite.rfWriteData := vMac.io.out.bits.vd

  val vxsatBufferIn = Wire(Bool())
  when(vMac.io.out.valid && !muopEnd){
    vxsatBufferIn := vMac.io.out.bits.vxsat  || vxsatBuffer
    vxsatBuffer := vxsatBufferIn
  }.otherwise{
    vxsatBufferIn := vxsatBuffer
  }

  when(vMac.io.out.valid && muopEnd){
    vxsatBuffer := false.B
  }

  io.out.bits.commitInfo.valid                 := vMac.io.out.valid && muopEnd
  io.out.bits.commitInfo.bits.data             := vMac.io.out.bits.vd
  io.out.bits.commitInfo.bits.scalarRegWriteEn := scalarRegWriteEn
  io.out.bits.commitInfo.bits.floatRegWriteEn  := floatRegWriteEn
  io.out.bits.commitInfo.bits.ldest            := scalarRegWriteIdx
  io.out.bits.commitInfo.bits.vxsat            := vxsatBufferIn
  io.out.bits.commitInfo.bits.fflags           := false.B

  io.out.valid := vMac.io.out.valid

}

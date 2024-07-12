package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import firrtl.Utils
import darecreek.exu.vfu.vmask.VMask


class VMaskWrapper (implicit p : Parameters) extends Module {

  val io = IO(new Bundle {
    val in = Input(ValidIO(new VFuInput))
    val mergeInfo = Input(new MuopMergeAttr)
    val out       = Output(ValidIO(new IexOut))
  })

  val vMask = Module(new VMask)

  vMask.io.in.valid := io.in.valid
  vMask.io.in.bits := io.in.bits

  val rfWriteEn         = RegEnable(io.mergeInfo.rfWriteEn,        io.in.valid)
  val rfWriteIdx        = RegEnable(io.mergeInfo.ldest,            io.in.valid)
  val scalarRegWriteEn  = RegEnable(io.mergeInfo.scalarRegWriteEn, io.in.valid)
  val floatRegWriteEn   = RegEnable(io.mergeInfo.floatRegWriteEn,  io.in.valid)
  val scalarRegWriteIdx = RegEnable(io.mergeInfo.ldest,            io.in.valid)
  val muopEnd           = RegEnable(io.mergeInfo.muopEnd,          io.in.valid)
  val regDstIdx         = RegEnable(io.mergeInfo.regDstIdx,        io.in.valid)
  //val data              = UInt(128.W)
  val regDataBuffer     = RegInit(0.U(128.W))
  val vxsat             = Bool()
  val vxsatBuffer       = RegInit(false.B)
  
  io.out.bits.toRegFileWrite.rfWriteEn   := rfWriteEn
  io.out.bits.toRegFileWrite.rfWriteMask := Fill(128/8, 0.U)
  io.out.bits.toRegFileWrite.rfWriteIdx  := rfWriteIdx
  io.out.bits.toRegFileWrite.rfWriteData := vMask.io.out.bits.vd

  val vxsatBufferIn = Wire(Bool())
  when(vMask.io.out.valid && !muopEnd){
    vxsatBufferIn := vMask.io.out.bits.vxsat  || vxsatBuffer
    vxsatBuffer := vxsatBufferIn
  }.otherwise{
    vxsatBufferIn := vxsatBuffer
  }

  when(vMask.io.out.valid && muopEnd){
    vxsatBuffer := false.B
  }

  io.out.bits.commitInfo.valid                 := vMask.io.out.valid && muopEnd
  io.out.bits.commitInfo.bits.data             := vMask.io.out.bits.vd
  io.out.bits.commitInfo.bits.scalarRegWriteEn := scalarRegWriteEn
  io.out.bits.commitInfo.bits.floatRegWriteEn  := floatRegWriteEn
  io.out.bits.commitInfo.bits.ldest            := scalarRegWriteIdx
  io.out.bits.commitInfo.bits.vxsat            := vxsatBufferIn
  io.out.bits.commitInfo.bits.fflags           := false.B

  io.out.valid := vMask.io.out.valid

}


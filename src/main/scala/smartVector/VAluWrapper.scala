package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.alu.VAlu
import firrtl.Utils


class VAluWrapper (implicit p : Parameters) extends VFuModule {

  val io = IO(new Bundle {
    val in        = Input(ValidIO(new VFuInput))
    val mergeInfo = Input(new MuopMergeAttr)
    val out       = Output(ValidIO(new IexOut))
  })

  val vAlu = Module(new VAlu)
  
  vAlu.io.in.valid := io.in.valid
  vAlu.io.in.bits  := io.in.bits

  val rfWriteEn         = RegEnable(io.mergeInfo.rfWriteEn,        io.in.valid)
  val rfWriteIdx        = RegEnable(io.mergeInfo.ldest,            io.in.valid)
  val regBackWidth      = RegEnable(io.mergeInfo.regBackWidth,     io.in.valid)
  val regWriteMuopIdx   = RegEnable(io.mergeInfo.regWriteMuopIdx,  io.in.valid)
  val scalarRegWriteEn  = RegEnable(io.mergeInfo.scalarRegWriteEn, io.in.valid)
  val floatRegWriteEn   = RegEnable(io.mergeInfo.floatRegWriteEn,  io.in.valid)
  val scalarRegWriteIdx = RegEnable(io.mergeInfo.ldest,            io.in.valid)
  val muopEnd           = RegEnable(io.mergeInfo.muopEnd,          io.in.valid)
  val regDstIdx         = RegEnable(io.mergeInfo.regDstIdx,        io.in.valid)
  //val data              = UInt(128.W)
  val regDataBuffer     = RegInit(0.U(128.W))
  val vxsat             = Bool()
  val vxsatBuffer       = RegInit(false.B)
  
  when(vAlu.io.out.valid && rfWriteEn){
    when(regBackWidth === "b111".U){
        io.out.bits.toRegFileWrite.rfWriteEn   := true.B
        io.out.bits.toRegFileWrite.rfWriteMask := Fill(VLEN/8, 0.U)
        io.out.bits.toRegFileWrite.rfWriteIdx  := rfWriteIdx
        io.out.bits.toRegFileWrite.rfWriteData := vAlu.io.out.bits.vd
    }.elsewhen(regBackWidth === "b11".U){
        when(regWriteMuopIdx === 0.U){
            when(muopEnd){
                io.out.bits.toRegFileWrite.rfWriteEn   := true.B
                io.out.bits.toRegFileWrite.rfWriteMask := Fill(VLEN/8, 0.U)
                io.out.bits.toRegFileWrite.rfWriteIdx  := rfWriteIdx
                io.out.bits.toRegFileWrite.rfWriteData := vAlu.io.out.bits.vd
            }                 
            .otherwise{
                io.out.bits.commitInfo.valid  := false.B
                regDataBuffer := vAlu.io.out.bits.vd
                io.out.bits.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
            }               
        }.otherwise{
          io.out.bits.toRegFileWrite.rfWriteEn  := true.B
          io.out.bits.toRegFileWrite.rfWriteMask := Fill(VLEN/8, 0.U)
          io.out.bits.toRegFileWrite.rfWriteIdx := rfWriteIdx
          io.out.bits.toRegFileWrite.rfWriteData := 
          Cat(vAlu.io.out.bits.vd(127,64), regDataBuffer(63,0))
        }
    }.otherwise{
          io.out.bits.commitInfo.valid  := false.B
                regDataBuffer := vAlu.io.out.bits.vd
                io.out.bits.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
        }
  }.otherwise{
          io.out.bits.commitInfo.valid  := false.B
                regDataBuffer := vAlu.io.out.bits.vd
                io.out.bits.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
  }

  val vxsatBufferIn = Wire(Bool())
  when(vAlu.io.out.valid && !muopEnd){
    vxsatBufferIn := vAlu.io.out.bits.vxsat  || vxsatBuffer
    vxsatBuffer := vxsatBufferIn
  }.otherwise{
    vxsatBufferIn := vxsatBuffer
  }

  when(vAlu.io.out.valid && muopEnd){
    vxsatBuffer := false.B
  }

  io.out.bits.commitInfo.valid                 := vAlu.io.out.valid && muopEnd
  io.out.bits.commitInfo.bits.data             := vAlu.io.out.bits.vd
  io.out.bits.commitInfo.bits.scalarRegWriteEn := scalarRegWriteEn
  io.out.bits.commitInfo.bits.floatRegWriteEn  := floatRegWriteEn
  io.out.bits.commitInfo.bits.ldest            := scalarRegWriteIdx
  io.out.bits.commitInfo.bits.vxsat            := vxsatBufferIn
  io.out.bits.commitInfo.bits.fflags           := false.B

  io.out.valid := vAlu.io.out.valid

  
}



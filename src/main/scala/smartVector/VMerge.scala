package smartVector

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.alu.VAlu
import firrtl.Utils
import SmartParam._
import darecreek.exu.vfu.VAluOutput
import darecreek.exu.vfu.VPermOutput

class VMerge (implicit p : Parameters) extends Module {

    val io = IO(new Bundle{
        val in = new Bundle{
            val mergeInfo = Input(ValidIO(new MuopMergeAttr))
            val aluIn = Input(ValidIO(new IexOutput))
            val lsuIn = Input(ValidIO(new LsuOutput))
            val permIn = Input(new VPermOutput)
        }
        val out = new Bundle{
            //update register file
            val toRegFileWrite = Output(new regWriteIn)
            val commitInfo = ValidIO(new CommitInfo)
        } 
        val scoreBoardCleanIO = Flipped(new ScoreboardClearIO)
    }
    )
    
    val regDataBuffer = RegInit(0.U(128.W))
    val vxsatBuffer   = RegInit(false.B)
    val vxsatBufferIn = Wire(Bool())
    val fflagsBuffer   = RegInit(0.U(5.W))
    val fflagsBufferIn = Wire(UInt(5.W))
  
    val rfWriteEn         = RegEnable(io.in.mergeInfo.bits.rfWriteEn,        io.in.mergeInfo.valid)
    val rfWriteIdx        = RegEnable(io.in.mergeInfo.bits.ldest,            io.in.mergeInfo.valid)
    val regBackWidth      = RegEnable(io.in.mergeInfo.bits.regBackWidth,     io.in.mergeInfo.valid)
    val regWriteMuopIdx   = RegEnable(io.in.mergeInfo.bits.regWriteMuopIdx,  io.in.mergeInfo.valid)
    val scalarRegWriteEn  = RegEnable(io.in.mergeInfo.bits.scalarRegWriteEn, io.in.mergeInfo.valid)
    val floatRegWriteEn   = RegEnable(io.in.mergeInfo.bits.floatRegWriteEn,  io.in.mergeInfo.valid)
    val scalarRegWriteIdx = RegEnable(io.in.mergeInfo.bits.ldest,            io.in.mergeInfo.valid)
    val muopEnd           = RegEnable(io.in.mergeInfo.bits.muopEnd,          io.in.mergeInfo.valid)
    val permExpdLen       = RegEnable(io.in.mergeInfo.bits.permExpdLen,      io.in.mergeInfo.valid)
    val regDstIdx         = RegEnable(io.in.mergeInfo.bits.regDstIdx,        io.in.mergeInfo.valid)
         
    when(io.in.aluIn.valid && rfWriteEn){
        when(regBackWidth === "b111".U){
            io.out.toRegFileWrite.rfWriteEn   := true.B
            io.out.toRegFileWrite.rfWriteMask := Fill(VLEN/8, 0.U)
            io.out.toRegFileWrite.rfWriteIdx  := rfWriteIdx
            io.out.toRegFileWrite.rfWriteData := io.in.aluIn.bits.vd
        }.elsewhen(regBackWidth === "b11".U){
            when(regWriteMuopIdx === 0.U){
                when(muopEnd){
                    io.out.toRegFileWrite.rfWriteEn   := true.B
                    io.out.toRegFileWrite.rfWriteMask := Fill(VLEN/8, 0.U)
                    io.out.toRegFileWrite.rfWriteIdx  := rfWriteIdx
                    io.out.toRegFileWrite.rfWriteData := io.in.aluIn.bits.vd
                }.otherwise{
                    io.out.toRegFileWrite.rfWriteEn  := false.B
                    regDataBuffer := io.in.aluIn.bits.vd
                    io.out.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
                }               
            }.otherwise{
                io.out.toRegFileWrite.rfWriteEn  := true.B
                io.out.toRegFileWrite.rfWriteMask := Fill(VLEN/8, 0.U)
                io.out.toRegFileWrite.rfWriteIdx := rfWriteIdx
                io.out.toRegFileWrite.rfWriteData := 
                    Cat(io.in.aluIn.bits.vd(127,64), regDataBuffer(63,0))
            }
        }.otherwise{
            io.out.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
        }
    }.elsewhen(io.in.lsuIn.valid && io.in.lsuIn.bits.rfWriteEn)
    {
        io.out.toRegFileWrite.rfWriteEn   := true.B
        io.out.toRegFileWrite.rfWriteMask := io.in.lsuIn.bits.rfWriteMask
        io.out.toRegFileWrite.rfWriteIdx  := io.in.lsuIn.bits.rfWriteIdx
        io.out.toRegFileWrite.rfWriteData := io.in.lsuIn.bits.data
    }.otherwise{
        io.out.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
    }

    when(io.in.aluIn.valid){
        vxsatBufferIn  := io.in.aluIn.bits.vxsat  || vxsatBuffer
        fflagsBufferIn := io.in.aluIn.bits.fflags | fflagsBuffer
    }.otherwise{
        vxsatBufferIn := vxsatBuffer
        fflagsBufferIn := fflagsBuffer
    }
    vxsatBuffer   := vxsatBufferIn
    fflagsBuffer   := fflagsBufferIn

    when(io.in.aluIn.valid && muopEnd){
        io.out.commitInfo.valid := true.B
        io.out.commitInfo.bits.scalarRegWriteEn := scalarRegWriteEn
        io.out.commitInfo.bits.floatRegWriteEn  := floatRegWriteEn
        io.out.commitInfo.bits.ldest            := scalarRegWriteIdx
        io.out.commitInfo.bits.data             := io.in.aluIn.bits.vd
        io.out.commitInfo.bits.vxsat            := vxsatBufferIn
        vxsatBuffer                             := false.B
        io.out.commitInfo.bits.fflags           := fflagsBufferIn
        fflagsBuffer                             := false.B
    }.elsewhen(io.in.lsuIn.valid && io.in.lsuIn.bits.muopEnd){
        io.out.commitInfo.valid := true.B
        io.out.commitInfo.bits.scalarRegWriteEn := false.B
        io.out.commitInfo.bits.floatRegWriteEn  := false.B
        io.out.commitInfo.bits.ldest            := DontCare
        io.out.commitInfo.bits.data             := io.in.lsuIn.bits.data
        io.out.commitInfo.bits.vxsat            := false.B
        io.out.commitInfo.bits.fflags           := 0.U
    }otherwise{
        io.out.commitInfo.valid := false.B
        io.out.commitInfo.bits := 0.U.asTypeOf(new CommitInfo)
    }

    //Perm is not same as others, need to deal seperately
    val permWriteNum = RegInit(0.U(4.W))
    when(io.in.permIn.wb_vld){
        io.out.toRegFileWrite.rfWriteEn   := true.B
        io.out.toRegFileWrite.rfWriteMask := Fill(VLEN/8, 0.U)
        io.out.toRegFileWrite.rfWriteIdx  := regDstIdx + permWriteNum
        io.out.toRegFileWrite.rfWriteData := io.in.permIn.wb_data
        permWriteNum := permWriteNum + 1.U
    }
    when(io.in.permIn.wb_vld && (permWriteNum + 1.U === permExpdLen)){
        permWriteNum := 0.U
        io.out.commitInfo.valid := true.B
        io.out.commitInfo.bits.scalarRegWriteEn := scalarRegWriteEn
        io.out.commitInfo.bits.floatRegWriteEn  := floatRegWriteEn
        io.out.commitInfo.bits.ldest            := scalarRegWriteIdx
        io.out.commitInfo.bits.data             := io.in.permIn.wb_data
    }
    
    // xcpt occurs or is fault-only-first
    val ldstXcpt = (io.in.lsuIn.bits.xcpt.exception_vld || io.in.lsuIn.bits.xcpt.update_vl)

    // for load, when muopEnd or xcpt occurs, clear scoreboard
    val sboardClearMulti = io.in.lsuIn.valid && io.in.lsuIn.bits.muopEnd && io.in.lsuIn.bits.isSegLoad
    val sboardClearAll = io.in.lsuIn.valid && ldstXcpt

    io.scoreBoardCleanIO.clearEn        := io.out.toRegFileWrite.rfWriteEn && !(io.in.lsuIn.valid && io.in.lsuIn.bits.isSegLoad)                                          
    io.scoreBoardCleanIO.clearAddr      := Mux(sboardClearMulti, io.in.lsuIn.bits.regStartIdx, 
                                           Mux(sboardClearAll, 0.U(4.W), 
                                            io.out.toRegFileWrite.rfWriteIdx))
    io.scoreBoardCleanIO.clearMultiEn   := sboardClearMulti || sboardClearAll
    io.scoreBoardCleanIO.clearNum       := Mux(sboardClearAll, 32.U(6.W), io.in.lsuIn.bits.regCount)
    //io.scoreBoardCleanIO.clearAll       := sboardClearAll
}
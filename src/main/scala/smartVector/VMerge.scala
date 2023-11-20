package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.alu.VAlu
import firrtl.Utils
import SmartParam._
import freechips.rocketchip.diplomaticobjectmodel.model.R

class CommitInfo extends Bundle{
    val scalarRegWriteEn = Bool() 
    val ldest = UInt(5.W)
    val data = UInt(64.W)
}

class LsuOutput extends Bundle{
    val data = UInt(VLEN.W)
    val rfWriteEn         = Bool()
    val rfWriteIdx        = UInt(5.W)
    val scalarRegWriteEn  = Bool()
    val scalarRegWriteIdx = UInt(5.W)
    val muopEnd           = Bool()
}
class VMerge (implicit p : Parameters) extends VFuModule {

    val io = IO(new Bundle{
        val in = new Bundle{
            val mergeInfo = Input(new MuopMergeAttr)
            val aluIn = Input(ValidIO(new VAluOutput))
            val lsuIn = Input(ValidIO(new LsuOutput))
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

    val rfWriteEn         = Reg(Bool())
    val rfWriteIdx        = Reg(UInt(5.W))
    val regBackWidth      = Reg(UInt(3.W))
    val regWriteMuopIdx   = Reg(UInt(4.W))
    val scalarRegWriteEn  = Reg(Bool())
    val scalarRegWriteIdx = Reg(UInt(5.W))
    val muopEnd           = Reg(Bool())

    when(io.in.mergeInfo.alu){
        rfWriteEn         := io.in.mergeInfo.rfWriteEn
        rfWriteIdx        := io.in.mergeInfo.ldest
        regBackWidth      := io.in.mergeInfo.regBackWidth
        regWriteMuopIdx   := io.in.mergeInfo.regWriteMuopIdx
        scalarRegWriteEn  := io.in.mergeInfo.scalarRegWriteEn
        scalarRegWriteIdx := io.in.mergeInfo.ldest
        muopEnd           := io.in.mergeInfo.muopEnd
    }.otherwise{
        //TODO: ALU has no output valid, other iex has valid, when valid is high, let the signal pass
        rfWriteEn         := io.in.mergeInfo.rfWriteEn
        rfWriteIdx        := io.in.mergeInfo.ldest
        regBackWidth      := io.in.mergeInfo.regBackWidth
        regWriteMuopIdx   := io.in.mergeInfo.regWriteMuopIdx
        scalarRegWriteEn  := io.in.mergeInfo.scalarRegWriteEn
        scalarRegWriteIdx := io.in.mergeInfo.ldest
        muopEnd           := io.in.mergeInfo.muopEnd
    }
    
       
    when(io.in.aluIn.valid && rfWriteEn){
        when(regBackWidth === "b111".U){
            io.out.toRegFileWrite.rfWriteEn  := true.B
            io.out.toRegFileWrite.rfWriteIdx := rfWriteIdx
            io.out.toRegFileWrite.rfWriteData := io.in.aluIn.bits.vd
        }.elsewhen(regBackWidth === "b11".U){
            when(regWriteMuopIdx === 0.U){
                io.out.toRegFileWrite.rfWriteEn  := false.B
                regDataBuffer := io.in.aluIn.bits.vd
                io.out.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
            }.otherwise{
                io.out.toRegFileWrite.rfWriteEn  := true.B
                io.out.toRegFileWrite.rfWriteIdx := rfWriteIdx
                io.out.toRegFileWrite.rfWriteData := 
                    Cat(io.in.aluIn.bits.vd(63,0), regDataBuffer(63,0))
            }
        }.otherwise{
            io.out.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
        }
    }.elsewhen(io.in.lsuIn.valid && io.in.lsuIn.bits.rfWriteEn)
    {
        io.out.toRegFileWrite.rfWriteEn  := true.B
        io.out.toRegFileWrite.rfWriteIdx := io.in.lsuIn.bits.rfWriteIdx
        io.out.toRegFileWrite.rfWriteData := io.in.lsuIn.bits.data
    }.otherwise{
            io.out.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
    }
    io.scoreBoardCleanIO.clearEn   := io.out.toRegFileWrite.rfWriteEn
    io.scoreBoardCleanIO.clearAddr := io.out.toRegFileWrite.rfWriteIdx

    when(io.in.aluIn.valid && muopEnd){
        io.out.commitInfo.valid := true.B
        io.out.commitInfo.bits.scalarRegWriteEn := scalarRegWriteEn
        io.out.commitInfo.bits.ldest            := scalarRegWriteIdx
        io.out.commitInfo.bits.data             := io.in.aluIn.bits.vd
    }.elsewhen(io.in.lsuIn.valid && io.in.lsuIn.bits.muopEnd){
        io.out.commitInfo.valid := true.B
        io.out.commitInfo.bits.scalarRegWriteEn := io.in.lsuIn.bits.scalarRegWriteEn
        io.out.commitInfo.bits.ldest            := io.in.lsuIn.bits.scalarRegWriteIdx
        io.out.commitInfo.bits.data             := io.in.lsuIn.bits.data
    }otherwise{
        io.out.commitInfo.valid := false.B
        io.out.commitInfo.bits := 0.U.asTypeOf(new CommitInfo)
    }
}

package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.alu.VAlu
import firrtl.Utils

class MUopRegAttr extends Bundle{
    //000:8bit
    //001:16bit
    //010:32bit
    //011:64bit
    //111:128bit
    val regBackWidth = UInt(3.W)
    val regWriteMuopIdx  = UInt(4.W)
    //val expdLen = UInt(4.W)
}

class CommitInfo extends Bundle{
    val uopAttribute = new UopAttribute
    val data = UInt(64.W)
}

class VSplit (implicit p : Parameters) extends VFuModule {

    val io = IO(new Bundle{
        val in = new Bundle{
            val decodeIn    = Flipped(Decoupled(new VDecodeOutput))
            //vs1 and vs2 data
            val regRileIn   = Input(new regOut)
            val aluIn       = Input(ValidIO(new VAluOutput))
            //val mUopRegAttr = Input(new MUopRegAttr)
        }
        val out = new Bundle{
            val mUop = ValidIO(new UopQueueOutput)
            //update register file
            val toRegFileRead = Output(new regReadIn)
            val toRegFileWrite = Output(new regWriteIn)
            val commitInfo = ValidIO(new CommitInfo)
        } 
    })

    val uopQueue = Module(new UopQueue)

    uopQueue.io.in.decodeIn.valid  := io.in.decodeIn.valid
    uopQueue.io.in.decodeIn.bits  := io.in.decodeIn.bits
    uopQueue.io.in.regFileIn := io.in.regRileIn
    io.out.mUop := RegNext(uopQueue.io.out.mUop)
    io.out.toRegFileRead := uopQueue.io.out.toRegFile
    io.in.decodeIn.ready := uopQueue.io.in.decodeIn.ready

    val regDataBuffer = RegInit(0.U(128.W))
    
    val rfWriteEn = RegNext(RegNext(uopQueue.io.out.mUop.bits.uopRegInfo.rfWriteEn))
    val rfWriteIdx = RegNext(RegNext(uopQueue.io.out.mUop.bits.uopAttribute.ldest))
    val regBackWidth = RegNext(RegNext(uopQueue.io.out.mUopRegAttr.regBackWidth))
    val regWriteMuopIdx  = RegNext(RegNext(uopQueue.io.out.mUopRegAttr.regWriteMuopIdx))
    val scalarRegWriteEn = RegNext(RegNext(uopQueue.io.out.mUop.bits.uopAttribute.scalarRegWriteEn))
    val scalarRegWriteIdx = RegNext(RegNext(uopQueue.io.out.mUop.bits.uopAttribute.ldest))
    //val expdLen = RegNext(RegNext(uopQueue.io.out.mUopRegAttr.expdLen))
    //val muopIdx = RegNext(RegNext(uopQueue.io.out.mUop.bits.uop.uopIdx))
    val muopEnd = RegNext(RegNext(uopQueue.io.out.mUop.bits.uop.uopEnd))
       
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
    }.otherwise{
            io.out.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
    }

    when(muopEnd){
        io.out.commitInfo.valid := true.B
        when(io.in.aluIn.valid && scalarRegWriteEn){
            io.out.commitInfo.bits.uopAttribute.scalarRegWriteEn := true.B
            io.out.commitInfo.bits.uopAttribute.ldest := scalarRegWriteIdx
            io.out.commitInfo.bits.data := io.in.aluIn.bits.vd
        }.otherwise{
            io.out.commitInfo.bits := 0.U.asTypeOf(new CommitInfo)
        }
    }.otherwise{
        io.out.commitInfo.valid := false.B
        io.out.commitInfo.bits := 0.U.asTypeOf(new CommitInfo)
    }
}

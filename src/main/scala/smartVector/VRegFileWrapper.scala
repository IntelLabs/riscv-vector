package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu.VAluOutput
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import SmartParam._


class SVRegFileWrapper(implicit p : Parameters) extends Module{ 

    val io = IO(new Bundle{
        val in = new Bundle{
            val readIn = Input(new regReadIn)
            val writeIn = Input(new regWriteIn)
        }
        val out = Output(new regOut)
        //TODO: This is reserved for verification, delete it later
        val rfData = Output(Vec(NVPhyRegs, UInt(VLEN.W)))
    })

    val nVRFReadPorts = 2
    val nVRFWritePorts = 1
  
    val regFile = Module(new SVRegFile(nVRFReadPorts, nVRFWritePorts))

    val writeData = Wire(Vec(NLanes, UInt((VLEN/NLanes).W)))
    writeData(0) := io.in.writeIn.rfWriteData(63,0)
    writeData(1) := io.in.writeIn.rfWriteData(127, 64)  
    
    regFile.io.write(0).wen  := io.in.writeIn.rfWriteEn
    regFile.io.write(0).addr := io.in.writeIn.rfWriteIdx
    regFile.io.write(0).data := writeData

    regFile.io.read(0).ren  := io.in.readIn.rfReadEn(0)
    regFile.io.read(1).ren  := io.in.readIn.rfReadEn(1)
    regFile.io.read(0).addr := io.in.readIn.rfReadIdx(0)
    regFile.io.read(1).addr := io.in.readIn.rfReadIdx(1)

    val regWriteDone = RegInit(false.B)

    //Because VRF has no write conflict, so 
    regWriteDone := regFile.io.write(0).wen

    io.out.writeDone := regWriteDone
    io.out.readVld(0) := RegNext(io.in.readIn.rfReadEn(0))
    io.out.readVld(1) := RegNext(io.in.readIn.rfReadEn(1))

    val readDataOUt = Wire(Vec(2, UInt(VLEN.W)))
    readDataOUt(0) := Cat(regFile.io.read(0).data(1), regFile.io.read(0).data(0))
    readDataOUt(1) := Cat(regFile.io.read(1).data(1), regFile.io.read(1).data(0))
    io.out.readData(0) := Mux(io.in.readIn.rfReadIdx(0) === io.in.writeIn.rfWriteIdx, io.in.writeIn.rfWriteData, readDataOUt(0))
    io.out.readData(1) := Mux(io.in.readIn.rfReadIdx(1) === io.in.writeIn.rfWriteIdx, io.in.writeIn.rfWriteData, readDataOUt(1))

    //TODO: This is reserved for verification, delete it later
    io.rfData := regFile.io.rfData
}

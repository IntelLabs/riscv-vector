package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu.VAluOutput
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import SmartParam._


class SVRegFileWrapper(implicit p : Parameters) extends Module{ 

    val io = IO(new Bundle{
        val config = new SmartParameters {}
        val in = Input(new regIn)
        val out = Output(new regOut)
    })

    val nVRFReadPorts = 2
    val nVRFWritePorts = 1
  
    val regFile = Module(new SVRegFile(nVRFReadPorts, nVRFWritePorts))

    val writeData = Wire(Vec(NLanes, UInt((VLEN/NLanes).W)))
    writeData(0) := io.in.rfWriteData(63,0)
    writeData(1) := io.in.rfWriteData(127, 64)  
    
    regFile.io.write(0).wen  := io.in.rfWriteEn
    regFile.io.write(0).addr := io.in.rfWriteIdx
    regFile.io.write(0).data := writeData

    regFile.io.read(0).addr := io.in.rfReadIdx(0)
    regFile.io.read(1).addr := io.in.rfReadIdx(1)

    val regWriteDone = RegInit(false.B)

    //Because VRF has no write conflict, so 
    regWriteDone := regFile.io.write(0).wen

    io.out.writeDone := regWriteDone
    io.out.readData(0) := regFile.io.read(0).data(0)
    io.out.readData(1) := regFile.io.read(1).data(0)
}

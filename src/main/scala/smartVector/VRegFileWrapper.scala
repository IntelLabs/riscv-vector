package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu.VAluOutput
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import SmartParam._
import darecreek.exu.vfu.VPermOutput


class SVRegFileWrapper(implicit p : Parameters) extends Module{ 

    val io = IO(new Bundle{
        val in = new Bundle{
            val readIn = Input(new regReadIn)
            val writeIn = Input(new regWriteIn)
            val permReadIn = Input(new VPermOutput)
        }
        val out = Output(new regOut)
        val permReadOut = Output(new VPermRegIn)
        //TODO: This is reserved for verification, delete it later
        val rfData = Output(Vec(NVPhyRegs, UInt(VLEN.W)))
    })

    val nVRFReadPorts = 5
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
    regFile.io.read(2).ren  := io.in.readIn.rfReadEn(2)
    regFile.io.read(3).ren  := io.in.readIn.rfReadEn(3)
    regFile.io.read(0).addr := io.in.readIn.rfReadIdx(0)
    regFile.io.read(1).addr := io.in.readIn.rfReadIdx(1)
    regFile.io.read(2).addr := io.in.readIn.rfReadIdx(2)
    regFile.io.read(3).addr := io.in.readIn.rfReadIdx(3)

    //permutation need to read register
    regFile.io.read(4).ren  := io.in.permReadIn.rd_en
    regFile.io.read(4).addr := io.in.permReadIn.rd_preg_idx
    io.permReadOut.rvalid := regFile.io.read(4).ren
    io.permReadOut.rdata := Cat(regFile.io.read(4).data(1), regFile.io.read(4).data(0))

    val regWriteDone = RegInit(false.B)

    //Because VRF has no write conflict, so 
    regWriteDone := regFile.io.write(0).wen

    io.out.writeDone := regWriteDone
    io.out.readVld(0) := io.in.readIn.rfReadEn(0)
    io.out.readVld(1) := io.in.readIn.rfReadEn(1)
    io.out.readVld(2) := io.in.readIn.rfReadEn(2)
    io.out.readVld(3) := io.in.readIn.rfReadEn(3)

    val readDataOUt = Wire(Vec(4, UInt(VLEN.W)))
    readDataOUt(0) := Cat(regFile.io.read(0).data(1), regFile.io.read(0).data(0))
    readDataOUt(1) := Cat(regFile.io.read(1).data(1), regFile.io.read(1).data(0))
    readDataOUt(2) := Cat(regFile.io.read(2).data(1), regFile.io.read(2).data(0))
    readDataOUt(3) := Cat(regFile.io.read(3).data(1), regFile.io.read(3).data(0))

    //bypass
    io.out.readData(0) := Mux(io.in.writeIn.rfWriteEn & (io.in.readIn.rfReadIdx(0) === io.in.writeIn.rfWriteIdx), io.in.writeIn.rfWriteData, readDataOUt(0))
    io.out.readData(1) := Mux(io.in.writeIn.rfWriteEn & (io.in.readIn.rfReadIdx(1) === io.in.writeIn.rfWriteIdx), io.in.writeIn.rfWriteData, readDataOUt(1))
    io.out.readData(2) := Mux(io.in.writeIn.rfWriteEn & (io.in.readIn.rfReadIdx(2) === io.in.writeIn.rfWriteIdx), io.in.writeIn.rfWriteData, readDataOUt(2))
    io.out.readData(3) := Mux(io.in.writeIn.rfWriteEn & (io.in.readIn.rfReadIdx(3) === io.in.writeIn.rfWriteIdx), io.in.writeIn.rfWriteData, readDataOUt(3))

    //TODO: This is reserved for verification, delete it later
    io.rfData := regFile.io.rfData
}

package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu.VAluOutput
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import SmartParam._
import darecreek.exu.vfu.VPermOutput
import matrix.MatrixParameters.hasMatrix


class SVRegFileWrapper(implicit p : Parameters) extends Module{ 

    val io = IO(new Bundle{
        val in = new Bundle{
            val readIn = Input(new regReadIn)
            val writeIn = Input(new regWriteIn)
            val mma_toRegFileWrite = if (hasMatrix) Some(Input(Vec(NVPhyRegs / 2, new regWriteIn))) else None
            val permReadIn = Input(new VPermOutput)
        }
        val out = Output(new regOut)
        val permReadOut = Output(new VPermRegIn)
        //TODO: This is reserved for verification, delete it later
        val rfData = Output(Vec(NVPhyRegs, UInt(VLEN.W)))
    })

    val nVRFReadPorts = 5
    val nVRFWritePorts = 16
  
    val regFile = Module(new SVRegFile()(p, nVRFReadPorts, nVRFWritePorts))

    val writeData = Wire(Vec(NLanes, UInt((VLEN/NLanes).W)))
    val writeMask = Wire(Vec(NLanes, UInt((VLEN/NLanes/8).W)))

    for (i <- 0 until NLanes){
        writeData(i) := io.in.writeIn.rfWriteData((i+1)*VLEN/NLanes-1, i*VLEN/NLanes)
        writeMask(i) := io.in.writeIn.rfWriteMask((i+1)*VLEN/NLanes/8-1, i*VLEN/NLanes/8)
    }

   for (i <- 0 until 16) {
       regFile.io.write(i).wen   := false.B
       regFile.io.write(i).addr  := io.in.writeIn.rfWriteIdx
       regFile.io.write(i).data  := writeData
       regFile.io.write(i).wmask := writeMask
   }

    regFile.io.mma_toRegFileWrite.get := io.in.mma_toRegFileWrite.get
    regFile.io.write(0).wen   := io.in.writeIn.rfWriteEn
    regFile.io.write(0).addr  := io.in.writeIn.rfWriteIdx
    regFile.io.write(0).data  := writeData
    regFile.io.write(0).wmask := writeMask

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

    // Because VRF has no write conflict
    regWriteDone := regFile.io.write(0).wen

    io.out.writeDone := regWriteDone
    for (i <- 0 until nVRFReadPorts - 1) {
        io.out.readVld(i) := io.in.readIn.rfReadEn(i)
    }

    val readDataOut = Wire(Vec(4, UInt(VLEN.W)))
    for (i <- 0 until nVRFReadPorts - 1) {
        readDataOut(i) := Cat(regFile.io.read(i).data(1), regFile.io.read(i).data(0))
    }

    // Bypass
    for (i <- 0 until nVRFReadPorts - 1) {
        io.out.readData(i) := Mux(io.in.writeIn.rfWriteEn & (io.in.readIn.rfReadIdx(i) === io.in.writeIn.rfWriteIdx), io.in.writeIn.rfWriteData, readDataOut(i))
    }

    //TODO: This is reserved for verification, delete it later
    io.rfData := regFile.io.rfData
}

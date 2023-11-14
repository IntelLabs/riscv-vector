package smartVector

import chisel3._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.formal.MonitorDirection._
import chisel3.util._
import darecreek.exu.vfu.VFuParamsKey
import darecreek.exu.vfu.VFuParameters
import xiangshan.XSCoreParamsKey
import xiangshan.XSCoreParameters

class RVUTestResult extends Bundle {
    val commit_vld   = Output(Bool())
    val alu_data     = Output(UInt(128.W))
    //val reg_data     = Output(UInt(128.W))
}

class SmartVector extends Module {
    val io = IO(new Bundle{
        val in = Flipped(Decoupled(new RVUissue))
        val out = Output(new Bundle{
            val rvuCommit = new RVUCommit
            val rvuExtra  = new RVUExtra
        })
        //TODO: This is reserved for verification, delete it later
        val rfData = Output(Vec(NVPhyRegs, UInt(regLen.W)))
    })


    val p = Parameters.empty.alterPartial({
        case SmartParamsKey => SmartParameters(VLEN = 128)
        case VFuParamsKey   => VFuParameters(XLEN = 64, VLEN = 128)
        case XSCoreParamsKey => XSCoreParameters()
    })

    val decoder = Module(new SVDecodeUnit()(p))
    val split   = Module(new VSplit()(p))
    val iex     = Module(new VIexWrapper()(p))
    val regFile = Module(new SVRegFileWrapper()(p))
    
    decoder.io.in.bits  := io.in.bits
    decoder.io.in.valid := io.in.valid
    split.io.in.decodeIn <> decoder.io.out
    split.io.in.regRileIn <> regFile.io.out
    split.io.in.aluIn <> iex.io.out
    //uopQueue.io.in.valid := decoder.io.out.valid
    iex.io.in <> split.io.out.mUop
    regFile.io.in.readIn := split.io.out.toRegFileRead
    regFile.io.in.writeIn := split.io.out.toRegFileWrite

    //TODO: This is reserved for verification, delete it later
    io.rfData := regFile.io.rfData

//
    //The code is based on the "all the muop is done in one cycle"
    //val scalarRegWriteEn_E3 = Reg(Bool())
    //scalarRegWriteEn_E3 := split.io.out.mUop.bits.uopAttribute.scalarRegWriteEn
//
    //val scalarRegWriteEn_E4 = Reg(Bool())
    //scalarRegWriteEn_E4 := scalarRegWriteEn_E3
//
    //val scalarRegWriteIdx_E3 = Reg(UInt(5.W))
    //scalarRegWriteIdx_E3 := split.io.out.mUop.bits.uopAttribute.ldest
//
    //val scalarRegWriteIdx_E4 = Reg(UInt(5.W))
    //scalarRegWriteIdx_E4 := scalarRegWriteIdx_E3    
//
    //val scalarRegWriteData_E4 = Reg(UInt(64.W))
    //scalarRegWriteData_E4 := iex.SValu.io.out.bits.vd

    io.out.rvuCommit.commit_vld      := split.io.out.commitInfo.valid
    io.out.rvuCommit.exception_vld   := false.B
    io.out.rvuCommit.update_vl       := false.B
    io.out.rvuCommit.update_vl_data  := 0.U
    io.out.rvuCommit.illegal_inst    := false.B
    io.out.rvuCommit.return_data_vld := split.io.out.commitInfo.bits.uopAttribute.scalarRegWriteEn
    io.out.rvuCommit.return_data     := split.io.out.commitInfo.bits.data
    io.out.rvuCommit.return_reg_idx  := split.io.out.commitInfo.bits.uopAttribute.ldest
    //io.out.rvuExtra.vpu_ready := 

    //Just For Test
    //io.out.commit_vld := regFile.io.out.writeDone
    //io.out.alu_data := RegNext(iex.io.out.bits.toReg.bits.rfWriteData)
    //io.rvuExtra.vpu_ready := io.decode_ready
    io.in.ready := decoder.io.in.ready
}

object Main extends App {
  println("Generating the VPU Core hardware")
  emitVerilog(new SmartVector(), Array("--target-dir", "generated"))
}

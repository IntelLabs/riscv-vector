package smartVector

import chisel3._
import org.chipsalliance.cde.config.{Config, Field, Parameters}
//import freechips.rocketchip.formal.MonitorDirection._
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
        //val out = Output(new Bundle{
        //    val rvuCommit = new RVUCommit
        //    val rvuExtra  = new RVUExtra})
        val out = Output(new RVUTestResult)
        //val rvuExtra  = Output(new RVUExtra)
    })


    val p = Parameters.empty.alterPartial({
        case SmartParamsKey => SmartParameters(VLEN = 128)
        case VFuParamsKey   => VFuParameters(XLEN = 64, VLEN = 128)
        case XSCoreParamsKey => XSCoreParameters()
    })

    val decoder = Module(new SVDecodeUnit()(p))
    val uopQueue = Module(new UopQueue()(p))
    val iex = Module(new VIexWrapper()(p))
    val regFile = Module(new SVRegFileWrapper()(p))
    
    decoder.io.in.bits  := io.in.bits
    decoder.io.in.valid := io.in.valid
    uopQueue.io.in <> decoder.io.out
    //uopQueue.io.in.valid := decoder.io.out.valid
    iex.io.in <> uopQueue.io.out
    
    when(decoder.io.out.bits.toReg.valid && iex.io.out.bits.toReg.valid){
        regFile.io.in.rfReadEn    := decoder.io.out.bits.toReg.bits.rfReadEn
        regFile.io.in.rfReadIdx   := decoder.io.out.bits.toReg.bits.rfReadIdx
        regFile.io.in.rfWriteEn   := iex.io.out.bits.toReg.bits.rfWriteEn
        regFile.io.in.rfWriteData := iex.io.out.bits.toReg.bits.rfWriteData
        regFile.io.in.rfWriteIdx  := iex.io.out.bits.toReg.bits.rfWriteIdx
        regFile.io.in.vxsat       := iex.io.out.bits.toReg.bits.vxsat
    }.elsewhen(decoder.io.out.bits.toReg.valid && !iex.io.out.bits.toReg.valid){
        regFile.io.in := decoder.io.out.bits.toReg.bits
    }.elsewhen(!decoder.io.out.bits.toReg.valid && iex.io.out.bits.toReg.valid){
        regFile.io.in := iex.io.out.bits.toReg.bits
    }.otherwise{
        regFile.io.in.rfReadEn(0) := false.B
        regFile.io.in.rfReadEn(1) := false.B
        regFile.io.in.rfWriteEn   := false.B
        regFile.io.in.rfWriteData := DontCare
        regFile.io.in.rfWriteIdx  := DontCare
        regFile.io.in.rfReadIdx   := DontCare
        regFile.io.in.vxsat       := false.B
    }

    //io.out.rvuCOmmit.commit_vld := regFile.io.out.writeDone
    //io.out.rvuCOmmit.exception_vld := false.B
    //io.out.rvuCOmmit.update_vl := false.B
    //io.out.rvuCOmmit.update_vl_data := 0.U
    //io.out.rvuCOmmit.illegal_inst := false.B
    //io.out.rvuCOmmit.return_data_vld := false.B 
    //io.out.rvuCOmmit.return_data := 0.U
    //io.out.rvuCOmmit.return_reg_idx := 0.U
    //io.out.rvuExtra.vpu_ready := 

    io.out.commit_vld := regFile.io.out.writeDone
    io.out.alu_data := RegNext(iex.io.out.bits.toReg.bits.rfWriteData)
    //io.rvuExtra.vpu_ready := io.decode_ready
    io.in.ready := decoder.io.in.ready
}

object Main extends App {
  println("Generating the VPU Core hardware")
  emitVerilog(new SmartVector(), Array("--target-dir", "generated"))
}


















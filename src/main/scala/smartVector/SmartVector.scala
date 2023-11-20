package smartVector

import chisel3._
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import freechips.rocketchip.formal.MonitorDirection._
import chisel3.util._
import darecreek.exu.vfu.VFuParamsKey
import darecreek.exu.vfu.VFuParameters
import xiangshan.XSCoreParamsKey
import xiangshan.XSCoreParameters
import freechips.rocketchip.tile.TileKey

class RVUTestResult extends Bundle {
    val commit_vld   = Output(Bool())
    val alu_data     = Output(UInt(128.W))
    //val reg_data     = Output(UInt(128.W))
}

class SmartVector(implicit tileParams: Parameters) extends Module {
    val io = IO(new Bundle{
        val in = Flipped(Decoupled(new RVUissue))
        val out = Output(new Bundle{
            val rvuCommit = new RVUCommit
            val rvuExtra  = new RVUExtra
        })
        val hellacache = new freechips.rocketchip.rocket.HellaCacheIO
    })


    val p = Parameters.empty.alterPartial({
        case SmartParamsKey => SmartParameters(VLEN = 128)
        case VFuParamsKey   => VFuParameters(XLEN = 64, VLEN = 128)
        case XSCoreParamsKey => XSCoreParameters()
    })

    val decoder = Module(new SVDecodeUnit()(p))
    val split = Module(new VSplit()(p))
    val iex = Module(new VIexWrapper()(p))
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

    val lsu = Module(new SVlsu()(tileParams, p))
    lsu.io.mUop <> split.io.out.mUop

    // /*******************hellacache******************/
    lsu.io.hellacache.req <> io.hellacache.req
    io.hellacache.s1_kill := lsu.io.hellacache.s1_kill
    io.hellacache.s1_data := lsu.io.hellacache.s1_data
    io.hellacache.s2_kill := lsu.io.hellacache.s2_kill
    lsu.io.hellacache.s2_paddr := io.hellacache.s2_paddr
    lsu.io.hellacache.s2_uncached := io.hellacache.s2_uncached

    // 使用 <> 连接 resp、replay_next、s2_xcpt、s2_gpa、s2_gpa_is_pte、uncached_resp、ordered、perf
    lsu.io.hellacache.resp <> io.hellacache.resp
    lsu.io.hellacache.replay_next := io.hellacache.replay_next
    lsu.io.hellacache.s2_xcpt := io.hellacache.s2_xcpt
    lsu.io.hellacache.s2_gpa := io.hellacache.s2_gpa
    lsu.io.hellacache.s2_gpa_is_pte := io.hellacache.s2_gpa_is_pte
    lsu.io.hellacache.s2_nack := io.hellacache.s2_nack
    lsu.io.hellacache.s2_nack_cause_raw := io.hellacache.s2_nack_cause_raw
    // lsu.io.hellacache.uncached_resp <> io.hellacache.uncached_resp
    lsu.io.hellacache.uncached_resp.foreach { ucResp =>
        ucResp.valid := io.hellacache.uncached_resp.get.valid
        io.hellacache.uncached_resp.get.ready := ucResp.ready
        ucResp.bits := io.hellacache.uncached_resp.get.bits
    }
    lsu.io.hellacache.ordered := io.hellacache.ordered
    lsu.io.hellacache.perf := io.hellacache.perf

    io.hellacache.keep_clock_enabled := lsu.io.hellacache.keep_clock_enabled
    lsu.io.hellacache.clock_enabled := io.hellacache.clock_enabled
    /*---------------hellacache-------------------*/
        
    //arb register file's read and write port
    //when(uopQueue.io.out.toRegFile.valid && iex.io.out.bits.toReg.valid){
    //    regFile.io.in.rfReadEn    := uopQueue.io.out.toRegFile.bits.rfReadEn
    //    regFile.io.in.rfReadIdx   := uopQueue.io.out.toRegFile.bits.rfReadIdx
    //    regFile.io.in.rfWriteEn   := iex.io.out.bits.toReg.bits.rfWriteEn
    //    regFile.io.in.rfWriteData := iex.io.out.bits.toReg.bits.rfWriteData
    //    regFile.io.in.rfWriteIdx  := iex.io.out.bits.toReg.bits.rfWriteIdx
    //    regFile.io.in.vxsat       := iex.io.out.bits.toReg.bits.vxsat
    //}.elsewhen(uopQueue.io.out.toRegFile.valid && !iex.io.out.bits.toReg.valid){
    //    regFile.io.in := uopQueue.io.out.toRegFile.bits
    //}.elsewhen(!uopQueue.io.out.toRegFile.valid && iex.io.out.bits.toReg.valid){
    //    regFile.io.in := iex.io.out.bits.toReg.bits
    //}.otherwise{
    //    regFile.io.in.rfReadEn(0) := false.B
    //    regFile.io.in.rfReadEn(1) := false.B
    //    regFile.io.in.rfWriteEn   := false.B
    //    regFile.io.in.rfWriteData := DontCare
    //    regFile.io.in.rfWriteIdx  := DontCare
    //    regFile.io.in.rfReadIdx   := DontCare
    //    regFile.io.in.vxsat       := false.B
    //}
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

// object Main extends App {
//   println("Generating the VPU Core hardware")
//   emitVerilog(new SmartVector(), Array("--target-dir", "generated"))
// }

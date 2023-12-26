package smartVector

import chisel3._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.formal.MonitorDirection._
import chisel3.util._
import darecreek.exu.vfu.VFuParamsKey
import darecreek.exu.vfu.VFuParameters
import xiangshan.XSCoreParamsKey
import xiangshan.XSCoreParameters
import SmartParam._

class RVUTestResult extends Bundle {
    val commit_vld   = Output(Bool())
    val alu_data     = Output(UInt(128.W))
    //val reg_data     = Output(UInt(128.W))
}

class ScoreboardSetIO extends Bundle {
    val setEn         = Input(Bool())
    val setAddr       = Input(UInt(log2Ceil(NVPhyRegs).W))
}

class ScoreboardClearIO extends Bundle {
    val clearEn       = Input(Bool())
    val clearAddr     = Input(UInt(log2Ceil(NVPhyRegs).W))
}

class ScoreboardReadIO extends Bundle {
    val readAddr1     = Input(UInt(log2Ceil(NVPhyRegs).W))
    val readAddr2     = Input(UInt(log2Ceil(NVPhyRegs).W))
    val read1         = Output(Bool())
    val readBypassed1 = Output(Bool())
    val read2         = Output(Bool())
    val readBypassed2 = Output(Bool())
}

class CommitInfo extends Bundle{
    val scalarRegWriteEn = Bool() 
    val ldest = UInt(5.W)
    val data = UInt(64.W)
}


class SmartVector extends Module {
    val io = IO(new Bundle{
        val in = Flipped(Decoupled(new RVUissue))
        val out = Output(new Bundle{
            val rvuCommit = new RVUCommit
            val rvuExtra  = new RVUExtra
        })
        val rvuMemory = new RVUMemory
        //TODO: This is reserved for verification, delete it later
        //val rfData = Output(Vec(NVPhyRegs, UInt(VLEN.W)))
        val rfData = Output(Vec(NVPhyRegs, UInt(VLEN.W)))
    })


    val p = Parameters.empty.alterPartial({
        case SmartParamsKey => SmartParameters(VLEN = 128)
        case VFuParamsKey   => VFuParameters(XLEN = 64, VLEN = 128)
        case XSCoreParamsKey => XSCoreParameters()
    })

    val decoder = Module(new SVDecodeUnit()(p))
    val split   = Module(new Vsplit()(p))
    val merge   = Module(new VMerge()(p))
    val commit  = Module(new VCommit())
    val iex     = Module(new VIexWrapper()(p))
    val regFile = Module(new SVRegFileWrapper()(p))
    val svlsu   = Module(new SVlsu()(p))


    decoder.io.in.bits  := io.in.bits
    decoder.io.in.valid := io.in.valid
    split.io.in.decodeIn <> decoder.io.out
    split.io.in.regFileIn <> regFile.io.out
    iex.io.in <> RegNext(split.io.out.mUop)
    merge.io.in.aluIn <> iex.io.out
    commit.io.in.commitInfo <> merge.io.out.commitInfo
    commit.io.in.excpInfo <> split.io.excpInfo
    io.out.rvuCommit <> commit.io.out.commitInfo

    svlsu.io.mUop <> split.io.out.mUop
    svlsu.io.mUopMergeAttr <> split.io.out.mUopMergeAttr
    split.io.vLSUXcpt <> svlsu.io.xcpt

    //ChenLu change
    split.io.lsuStallSplit := ~svlsu.io.lsuReady
    merge.io.in.lsuIn <> svlsu.io.lsuOut
    
    merge.io.in.mergeInfo <> split.io.out.mUopMergeAttr  
    regFile.io.in.readIn  <> split.io.out.toRegFileRead
    regFile.io.in.writeIn <> merge.io.out.toRegFileWrite

    //stall pipeline
    split.io.iexNeedStall := iex.io.iexNeedStall
    decoder.io.iexNeedStall := iex.io.iexNeedStall

    //TODO: This is reserved for verification, delete it later
    io.rfData := regFile.io.rfData

    svlsu.io.segmentIdx := 0.U

    io.rvuMemory.req.valid      := svlsu.io.dataExchange.req.valid
    io.rvuMemory.req.bits.idx   := svlsu.io.dataExchange.req.bits.idx
    io.rvuMemory.req.bits.addr  := svlsu.io.dataExchange.req.bits.addr
    io.rvuMemory.req.bits.cmd   := svlsu.io.dataExchange.req.bits.cmd
    io.rvuMemory.req.bits.data  := svlsu.io.dataExchange.req.bits.data
    io.rvuMemory.req.bits.mask  := svlsu.io.dataExchange.req.bits.mask

    // io.rvuMemory.resp.ready := svlsu.io.dataExchange.resp.ready
    svlsu.io.dataExchange.req.ready             := io.rvuMemory.req.ready
    svlsu.io.dataExchange.resp.valid            := io.rvuMemory.resp.valid
    svlsu.io.dataExchange.resp.bits.idx         := io.rvuMemory.resp.bits.idx
    svlsu.io.dataExchange.resp.bits.data        := io.rvuMemory.resp.bits.data
    svlsu.io.dataExchange.resp.bits.has_data    := io.rvuMemory.resp.bits.has_data
    svlsu.io.dataExchange.resp.bits.mask        := io.rvuMemory.resp.bits.mask
    svlsu.io.dataExchange.resp.bits.nack        := io.rvuMemory.resp.bits.nack

    svlsu.io.dataExchange.xcpt.ma.ld := io.rvuMemory.xcpt.ma.ld
    svlsu.io.dataExchange.xcpt.ma.st := io.rvuMemory.xcpt.ma.st
    svlsu.io.dataExchange.xcpt.pf.ld := io.rvuMemory.xcpt.pf.ld
    svlsu.io.dataExchange.xcpt.pf.st := io.rvuMemory.xcpt.pf.st
    svlsu.io.dataExchange.xcpt.gf.ld := io.rvuMemory.xcpt.gf.ld
    svlsu.io.dataExchange.xcpt.gf.st := io.rvuMemory.xcpt.gf.st
    svlsu.io.dataExchange.xcpt.ae.ld := io.rvuMemory.xcpt.ae.ld
    svlsu.io.dataExchange.xcpt.ae.st := io.rvuMemory.xcpt.ae.st
    
    val sboard  = new Scoreboard(NVPhyRegs, false)
    sboard.clear(merge.io.scoreBoardCleanIO.clearEn, merge.io.scoreBoardCleanIO.clearAddr)
    sboard.set(split.io.scoreBoardSetIO.setEn, split.io.scoreBoardSetIO.setAddr)
    split.io.scoreBoardReadIO.read1 := sboard.read(split.io.scoreBoardReadIO.readAddr1)
    split.io.scoreBoardReadIO.read2 := sboard.read(split.io.scoreBoardReadIO.readAddr2)
    split.io.scoreBoardReadIO.readBypassed1 := sboard.readBypassed(split.io.scoreBoardReadIO.readAddr1)
    split.io.scoreBoardReadIO.readBypassed2 := sboard.readBypassed(split.io.scoreBoardReadIO.readAddr2)
    
    io.in.ready := decoder.io.in.ready
}

object Main extends App {
  println("Generating the VPU Core hardware")
  emitVerilog(new SmartVector(), Array("--target-dir", "generated"))
}

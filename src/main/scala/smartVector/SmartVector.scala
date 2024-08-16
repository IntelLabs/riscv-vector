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
    val setMultiEn    = Input(Bool())
    val setNum        = Input(UInt(log2Ceil(NVPhyRegs).W))
    val setAddr2       = Input(UInt(log2Ceil(NVPhyRegs).W))
    val setMultiEn2    = Input(Bool())
    val setNum2        = Input(UInt(log2Ceil(NVPhyRegs).W))
}

class ScoreboardClearIO extends Bundle {
    val clearEn       = Input(Bool())
    val clearAddr     = Input(UInt(log2Ceil(NVPhyRegs).W))
    val clearMultiEn  = Input(Bool())
    val clearNum      = Input(UInt((log2Ceil(NVPhyRegs)+1).W))
    val clearAll      = Input(Bool())
}

class ScoreboardReadIO extends Bundle {
    val readAddr1      = Input(UInt(log2Ceil(NVPhyRegs).W))
    val readAddr2      = Input(UInt(log2Ceil(NVPhyRegs).W))
    val readAddr3      = Input(UInt(log2Ceil(NVPhyRegs).W))
    val readMaskAddr   = Input(UInt(log2Ceil(NVPhyRegs).W))
    val readNum1       = Input(UInt(3.W))
    val readNum2       = Input(UInt(3.W))
    val readBypassed1  = Output(Bool())
    val readBypassed1N = Output(Bool())
    val readBypassed2  = Output(Bool())
    val readBypassed2N = Output(Bool())
    val readBypassed3  = Output(Bool())
    val readBypassed3N = Output(Bool())
    val readBypassed4  = Output(Bool())
}

class CommitInfo extends Bundle{
    val scalarRegWriteEn = Bool()
    val floatRegWriteEn = Bool()
    val ldest = UInt(5.W)
    val fflags = UInt(5.W)
    val data = UInt(64.W)
    val vxsat = Bool()
    val sdId  = UInt(5.W)
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
    // val svlsu   = Module(new SVlsu()(p))
    // val svSegLsu = Module(new SVSegLsu()(p))

    val svlsuWrapper = Module(new SVlsuWrapper()(p))
    
    decoder.io.in.bits  := io.in.bits
    decoder.io.in.valid := io.in.valid & io.in.ready
    split.io.in.decodeIn <> decoder.io.out
    split.io.in.regFileIn <> regFile.io.out
    split.io.lsuEmpty := svlsuWrapper.io.lsuEmpty
    iex.io.in.bits.mUop <> split.io.out.mUop.bits
    iex.io.in.valid := split.io.out.mUop.valid
    iex.io.in.bits.mergeInfo <> split.io.out.mUopMergeAttr.bits
    merge.io.in.iexIn <> iex.io.out
    //merge.io.in.permIn <> iex.io.permOut
    commit.io.in.commitInfo <> merge.io.out.commitInfo
    commit.io.in.excpInfo <> split.io.excpInfo
    io.out.rvuCommit <> commit.io.out.commitInfo

    svlsuWrapper.io.mUop <> split.io.out.mUop
    svlsuWrapper.io.mUopMergeAttr <> split.io.out.mUopMergeAttr
    split.io.vLSUXcpt := Mux(svlsuWrapper.io.lsuOut.valid, svlsuWrapper.io.lsuOut.bits.xcpt, 0.U.asTypeOf(new VLSUXcpt))
    decoder.io.vLSUXcpt := Mux(svlsuWrapper.io.lsuOut.valid, svlsuWrapper.io.lsuOut.bits.xcpt, 0.U.asTypeOf(new VLSUXcpt))
    //ChenLu change
    split.io.lsuStallSplit := ~svlsuWrapper.io.lsuReady
    merge.io.in.lsuIn <> svlsuWrapper.io.lsuOut
    
    //merge.io.in.mergeInfo <> split.io.out.mUopMergeAttr  
    regFile.io.in.readIn  <> split.io.out.toRegFileRead
    regFile.io.in.writeIn <> merge.io.out.toRegFileWrite

    //perm read register file
    regFile.io.in.permReadIn <> iex.io.permOut
    iex.io.permRegIn <> regFile.io.permReadOut

    //stall pipeline
    split.io.iexNeedStall := iex.io.iexNeedStall
    //decoder.io.iexNeedStall := iex.io.iexNeedStall

    //TODO: This is reserved for verification, delete it later
    io.rfData := regFile.io.rfData

    io.rvuMemory.req.valid      := svlsuWrapper.io.dataExchange.req.valid
    io.rvuMemory.req.bits.idx   := svlsuWrapper.io.dataExchange.req.bits.idx
    io.rvuMemory.req.bits.addr  := svlsuWrapper.io.dataExchange.req.bits.addr
    io.rvuMemory.req.bits.cmd   := svlsuWrapper.io.dataExchange.req.bits.cmd
    io.rvuMemory.req.bits.data  := svlsuWrapper.io.dataExchange.req.bits.data
    io.rvuMemory.req.bits.mask  := svlsuWrapper.io.dataExchange.req.bits.mask

    // io.rvuMemory.resp.ready := svlsuWrapper.io.dataExchange.resp.ready
    svlsuWrapper.io.dataExchange.req.ready             := io.rvuMemory.req.ready
    svlsuWrapper.io.dataExchange.resp.valid            := io.rvuMemory.resp.valid
    svlsuWrapper.io.dataExchange.resp.bits.idx         := io.rvuMemory.resp.bits.idx
    svlsuWrapper.io.dataExchange.resp.bits.data        := io.rvuMemory.resp.bits.data
    svlsuWrapper.io.dataExchange.resp.bits.has_data    := io.rvuMemory.resp.bits.has_data
    svlsuWrapper.io.dataExchange.resp.bits.mask        := io.rvuMemory.resp.bits.mask
    svlsuWrapper.io.dataExchange.resp.bits.nack        := io.rvuMemory.resp.bits.nack

    svlsuWrapper.io.dataExchange.xcpt.ma.ld := io.rvuMemory.xcpt.ma.ld
    svlsuWrapper.io.dataExchange.xcpt.ma.st := io.rvuMemory.xcpt.ma.st
    svlsuWrapper.io.dataExchange.xcpt.pf.ld := io.rvuMemory.xcpt.pf.ld
    svlsuWrapper.io.dataExchange.xcpt.pf.st := io.rvuMemory.xcpt.pf.st
    svlsuWrapper.io.dataExchange.xcpt.gf.ld := io.rvuMemory.xcpt.gf.ld
    svlsuWrapper.io.dataExchange.xcpt.gf.st := io.rvuMemory.xcpt.gf.st
    svlsuWrapper.io.dataExchange.xcpt.ae.ld := io.rvuMemory.xcpt.ae.ld
    svlsuWrapper.io.dataExchange.xcpt.ae.st := io.rvuMemory.xcpt.ae.st
    
    val writeSboard  = new Scoreboard(NVPhyRegs, false)
    val readSboard  = new Scoreboard(NVPhyRegs, false)
    writeSboard.clear(merge.io.scoreBoardCleanIO.clearEn, merge.io.scoreBoardCleanIO.clearAddr)
    writeSboard.clearN(merge.io.scoreBoardCleanIO.clearMultiEn, merge.io.scoreBoardCleanIO.clearAddr, merge.io.scoreBoardCleanIO.clearNum)
    writeSboard.set(split.io.writeSBoardSetIO.setEn, split.io.writeSBoardSetIO.setAddr)
    writeSboard.setN1(split.io.writeSBoardSetIO.setMultiEn, split.io.writeSBoardSetIO.setAddr, split.io.writeSBoardSetIO.setNum)
    writeSboard.clearAll(merge.io.scoreBoardCleanIO.clearAll)    
    readSboard.setN1(iex.io.readSBoardSetIO.setMultiEn, iex.io.readSBoardSetIO.setAddr, iex.io.readSBoardSetIO.setNum)
    readSboard.setN2(iex.io.readSBoardSetIO.setMultiEn2, iex.io.readSBoardSetIO.setAddr2, iex.io.readSBoardSetIO.setNum2)
    split.io.writeSBoardReadIO.readBypassed1  := writeSboard.readBypassed(split.io.writeSBoardReadIO.readAddr1)
    split.io.writeSBoardReadIO.readBypassed2  := writeSboard.readBypassed(split.io.writeSBoardReadIO.readAddr2)
    split.io.writeSBoardReadIO.readBypassed3  := writeSboard.readBypassed(split.io.writeSBoardReadIO.readAddr3)
    split.io.writeSBoardReadIO.readBypassed4  := writeSboard.readBypassed(split.io.writeSBoardReadIO.readMaskAddr)
    split.io.writeSBoardReadIO.readBypassed1N := writeSboard.readBypassedN(split.io.writeSBoardReadIO.readNum1, split.io.writeSBoardReadIO.readAddr1)
    split.io.writeSBoardReadIO.readBypassed2N := writeSboard.readBypassedN(split.io.writeSBoardReadIO.readNum2, split.io.writeSBoardReadIO.readAddr2)
    split.io.writeSBoardReadIO.readBypassed3N := writeSboard.readBypassedN(1.U, split.io.writeSBoardReadIO.readAddr3)    

    io.in.ready := decoder.io.in.ready
}

object Main extends App {
    
  println("Generating the VPU Core hardware")
  emitVerilog(new SmartVector(), Array("--target-dir", "generated"))
}

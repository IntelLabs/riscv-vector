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

class SmartVector extends Module {
    val io = IO(new Bundle{
        val in = Flipped(Decoupled(new RVUissue))
        val out = Output(new Bundle{
            val rvuCommit = new RVUCommit
            val rvuExtra  = new RVUExtra
        })
        //TODO: This is reserved for verification, delete it later
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


    decoder.io.in.bits  := io.in.bits
    decoder.io.in.valid := io.in.valid
    split.io.in.decodeIn <> decoder.io.out
    split.io.in.regFileIn <> regFile.io.out
    iex.io.in <> split.io.out.mUop
    merge.io.in.aluIn <> iex.io.out
    commit.io.in.commitInfo <> merge.io.out.commitInfo

    //ChenLu change
    split.io.stallSplit     := false.B
    merge.io.in.lsuIn.valid := false.B
    merge.io.in.lsuIn.bits  := 0.U.asTypeOf(new LsuOutput)
    
    merge.io.in.mergeInfo <> split.io.out.mUopMergeAttr  
    regFile.io.in.readIn  <> split.io.out.toRegFileRead
    regFile.io.in.writeIn <> merge.io.out.toRegFileWrite

    //TODO: This is reserved for verification, delete it later
    io.rfData := regFile.io.rfData

    io.out.rvuCommit.commit_vld      := commit.io.out.commitInfo.valid
    io.out.rvuCommit.exception_vld   := false.B
    io.out.rvuCommit.update_vl       := false.B
    io.out.rvuCommit.update_vl_data  := 0.U
    io.out.rvuCommit.illegal_inst    := false.B
    io.out.rvuCommit.return_data_vld := commit.io.out.commitInfo.bits.scalarRegWriteEn
    io.out.rvuCommit.return_data     := commit.io.out.commitInfo.bits.data
    io.out.rvuCommit.return_reg_idx  := commit.io.out.commitInfo.bits.ldest

    
    val sboard  = new Scoreboard(NVPhyRegs, false)
    sboard.clear(merge.io.scoreBoardCleanIO.clearEn, merge.io.scoreBoardCleanIO.clearAddr)
    sboard.set(split.io.scoreBoardSetIO.setEn, split.io.scoreBoardSetIO.setAddr)
    split.io.scoreBoardReadIO.read1 := sboard.read(split.io.scoreBoardReadIO.readAddr1)
    split.io.scoreBoardReadIO.read2 := sboard.read(split.io.scoreBoardReadIO.readAddr2)
    split.io.scoreBoardReadIO.readBypassed1 := sboard.readBypassed(split.io.scoreBoardReadIO.readAddr1)
    split.io.scoreBoardReadIO.readBypassed2 := sboard.readBypassed(split.io.scoreBoardReadIO.readAddr2)
    
    io.in.ready := decoder.io.in.ready
}

// object Main extends App {
//   println("Generating the VPU Core hardware")
//   emitVerilog(new SmartVector(), Array("--target-dir", "generated"))
// }

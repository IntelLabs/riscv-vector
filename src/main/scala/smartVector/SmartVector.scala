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
  val commit_vld = Output(Bool())
  val alu_data   = Output(UInt(128.W))
  // val reg_data     = Output(UInt(128.W))
}

class ScoreboardSetIO extends Bundle {
  val setEn      = Input(Bool())
  val setAddr    = Input(UInt(log2Ceil(NVPhyRegs).W))
  val setMultiEn = Input(Bool())
  val setNum     = Input(UInt(log2Ceil(NVPhyRegs).W))
}

class ScoreboardClearIO extends Bundle {
  val clearEn      = Input(Bool())
  val clearAddr    = Input(UInt(log2Ceil(NVPhyRegs).W))
  val clearMultiEn = Input(Bool())
  val clearNum     = Input(UInt((log2Ceil(NVPhyRegs) + 1).W))
  val clearAll     = Input(Bool())
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

class CommitInfo extends Bundle {
  val scalarRegWriteEn = Bool()
  val floatRegWriteEn  = Bool()
  val ldest            = UInt(5.W)
  val fflags           = UInt(5.W)
  val data             = UInt(64.W)
  val vxsat            = Bool()
}

class SmartVector extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RVUissue))
    val out = Output(new Bundle {
      val rvuCommit = new RVUCommit
      val rvuExtra  = new RVUExtra
    })
    val rvuMemory = new RVUMemory
    // TODO: This is reserved for verification, delete it later
    // val rfData = Output(Vec(NVPhyRegs, UInt(VLEN.W)))
    val rfData = Output(Vec(NVPhyRegs, UInt(VLEN.W)))
  })

  val p = Parameters.empty.alterPartial {
    case SmartParamsKey  => SmartParameters(VLEN = 128)
    case VFuParamsKey    => VFuParameters(XLEN = 64, VLEN = 128)
    case XSCoreParamsKey => XSCoreParameters()
  }

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
  iex.io.in <> split.io.out.mUop
  merge.io.in.aluIn <> iex.io.out
  merge.io.in.permIn <> iex.io.permOut
  commit.io.in.commitInfo <> merge.io.out.commitInfo
  commit.io.in.excpInfo <> split.io.excpInfo
  io.out.rvuCommit <> commit.io.out.commitInfo

  svlsuWrapper.io.mUop <> split.io.out.mUop
  svlsuWrapper.io.mUopMergeAttr <> split.io.out.mUopMergeAttr
  split.io.vLSUXcpt   := Mux(svlsuWrapper.io.lsuOut.valid, svlsuWrapper.io.lsuOut.bits.xcpt, 0.U.asTypeOf(new VLSUXcpt))
  decoder.io.vLSUXcpt := Mux(svlsuWrapper.io.lsuOut.valid, svlsuWrapper.io.lsuOut.bits.xcpt, 0.U.asTypeOf(new VLSUXcpt))
  // ChenLu change
  split.io.lsuStallSplit := ~svlsuWrapper.io.lsuReady
  merge.io.in.lsuIn <> svlsuWrapper.io.lsuOut

  merge.io.in.mergeInfo <> split.io.out.mUopMergeAttr
  regFile.io.in.readIn <> split.io.out.toRegFileRead
  regFile.io.in.writeIn <> merge.io.out.toRegFileWrite

  // perm read register file
  regFile.io.in.permReadIn <> iex.io.permOut
  iex.io.permRegIn <> regFile.io.permReadOut

  // stall pipeline
  split.io.iexNeedStall := iex.io.iexNeedStall
  // decoder.io.iexNeedStall := iex.io.iexNeedStall

  // TODO: This is reserved for verification, delete it later
  io.rfData := regFile.io.rfData

  io.rvuMemory.req <> svlsuWrapper.io.dataExchange.req
  io.rvuMemory.resp <> svlsuWrapper.io.dataExchange.resp
  svlsuWrapper.io.dataExchange.xcpt := io.rvuMemory.xcpt

  val sboard = new Scoreboard(NVPhyRegs, false)
  sboard.clear(merge.io.scoreBoardCleanIO.clearEn, merge.io.scoreBoardCleanIO.clearAddr)
  sboard.clearN(
    merge.io.scoreBoardCleanIO.clearMultiEn,
    merge.io.scoreBoardCleanIO.clearAddr,
    merge.io.scoreBoardCleanIO.clearNum,
  )
  sboard.set(split.io.scoreBoardSetIO.setEn, split.io.scoreBoardSetIO.setAddr)
  sboard.setN(split.io.scoreBoardSetIO.setMultiEn, split.io.scoreBoardSetIO.setAddr, split.io.scoreBoardSetIO.setNum)
  sboard.clearAll(merge.io.scoreBoardCleanIO.clearAll)
  split.io.scoreBoardReadIO.readBypassed1 := sboard.readBypassed(split.io.scoreBoardReadIO.readAddr1)
  split.io.scoreBoardReadIO.readBypassed2 := sboard.readBypassed(split.io.scoreBoardReadIO.readAddr2)
  split.io.scoreBoardReadIO.readBypassed3 := sboard.readBypassed(split.io.scoreBoardReadIO.readAddr3)
  split.io.scoreBoardReadIO.readBypassed4 := sboard.readBypassed(split.io.scoreBoardReadIO.readMaskAddr)
  split.io.scoreBoardReadIO.readBypassed1N := sboard.readBypassedN(
    split.io.scoreBoardReadIO.readNum1,
    split.io.scoreBoardReadIO.readAddr1,
  )
  split.io.scoreBoardReadIO.readBypassed2N := sboard.readBypassedN(
    split.io.scoreBoardReadIO.readNum2,
    split.io.scoreBoardReadIO.readAddr2,
  )
  split.io.scoreBoardReadIO.readBypassed3N := sboard.readBypassedN(1.U, split.io.scoreBoardReadIO.readAddr3)
  io.in.ready                              := decoder.io.in.ready
}

object Main extends App {

  println("Generating the VPU Core hardware")
  emitVerilog(new SmartVector(), Array("--target-dir", "generated"))
}

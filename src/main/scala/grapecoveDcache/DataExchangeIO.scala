package grapecoveDCache

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import MemoryOpConstants._

class DataExchangeReq extends Bundle {
  val source = UInt(MasterSource.width.W)
  val paddr  = UInt(paddrWidth.W)
  val cmd    = UInt(M_SZ.W)
  // 64 -> 2, 512 -> 3
  val size    = UInt(log2Up(log2Up(dataBytes)).W)
  val signed  = Bool()
  val wdata   = UInt(dataWidth.W)
  val wmask   = UInt(dataBytes.W)
  val noAlloc = Bool()
  val dest    = UInt(destWidth.W) // reg addr or lsq idx
  // for test fill
  // operate a specific cache way
  val isRefill  = Bool()
  val refillWay = UInt(log2Up(nWays).W)
  val refillCoh = UInt(cohBits.W)

}

class DataExchangeResp extends Bundle {
  val source  = UInt(MasterSource.width.W)
  val dest    = UInt(destWidth.W)
  val status  = CacheRespStatus()
  val hasData = Bool()
  val data    = UInt(dataWidth.W)
}

class DataExchangeIO extends Bundle {
  val req         = Flipped(Decoupled(new DataExchangeReq))
  val resp        = Valid(new DataExchangeResp)
  val nextCycleWb = Bool() // next cycle occupy wb stage
  val s0_kill     = Input(Bool())
  val s1_kill     = Input(Bool())
}

class MainPipeReq extends Bundle {
  val source  = UInt(MasterSource.width.W)
  val paddr   = UInt(paddrWidth.W)
  val cmd     = UInt(M_SZ.W)
  val size    = UInt(log2Up(log2Up(dataBytes)).W)
  val signed  = Bool()
  val wdata   = UInt(dataWidth.W)
  val wmask   = UInt(dataBytes.W)
  val isMMIO  = Bool()
  val noAlloc = Bool()
  val dest    = UInt(destWidth.W) // reg addr or lsq idx

  val isFromCore = Bool()
  val isProbe    = Bool()
  val isRefill   = Bool()
  val probePerm  = UInt(TLPermissions.bdWidth.W) // probe permission
  val refillWay  = UInt(log2Up(nWays).W)
  val refillCoh  = UInt(cohBits.W)
}

object MainPipeReqConverter {
  // DataExchangeIO => MainPipeReq
  def apply(req: DataExchangeReq): MainPipeReq = {
    val mainPipeReq = WireInit(0.U.asTypeOf(new MainPipeReq))

    mainPipeReq.source  := req.source
    mainPipeReq.paddr   := req.paddr
    mainPipeReq.cmd     := req.cmd
    mainPipeReq.size    := req.size
    mainPipeReq.signed  := req.signed
    mainPipeReq.wdata   := req.wdata
    mainPipeReq.wmask   := req.wmask
    mainPipeReq.noAlloc := req.noAlloc
    mainPipeReq.dest    := req.dest
    // for test
    mainPipeReq.isFromCore := Mux(req.isRefill, false.B, true.B)
    mainPipeReq.isRefill   := req.isRefill
    mainPipeReq.refillWay  := req.refillWay
    mainPipeReq.refillCoh  := req.refillCoh
    mainPipeReq
  }

  // ProbeReq => MainPipeReq
  def apply(req: TLBundleB): MainPipeReq = {
    val mainPipeReq = WireInit(0.U.asTypeOf(new MainPipeReq))

    mainPipeReq.source    := req.source
    mainPipeReq.paddr     := req.address
    mainPipeReq.cmd       := req.opcode
    mainPipeReq.isProbe   := true.B
    mainPipeReq.probePerm := req.param
    mainPipeReq
  }

  // RefillReq => MainPipeReq
  def apply(req: MSHRReplace, victimWay: UInt): MainPipeReq = {
    val mainPipeReq = WireInit(0.U.asTypeOf(new MainPipeReq))

    mainPipeReq.paddr     := req.lineAddr << blockOffBits
    mainPipeReq.isRefill  := true.B
    mainPipeReq.refillCoh := req.state
    mainPipeReq.wdata     := req.data
    mainPipeReq.refillWay := victimWay
    mainPipeReq
  }

}

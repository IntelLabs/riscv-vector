package coincreekDCache

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import MemoryOpConstants._

class DataExchangeReq extends Bundle {
  val source = UInt(srcWidth.W)
  val paddr  = UInt(paddrWidth.W)
  val cmd    = UInt(M_SZ.W)
  // 64 -> 2, 512 -> 3
  val size    = UInt(log2Up(log2Up(dataBytes)).W)
  val signed  = Bool()
  val wdata   = UInt(dataWidth.W)
  val wmask   = UInt(dataBytes.W)
  val noAlloc = Bool()

  // for replace & test fill
  // operate a specific cache way
  val specifyValid = Bool()
  val specifyWay   = UInt(log2Up(nWays).W)

}

class DataExchangeResp extends Bundle {
  val source      = UInt(srcWidth.W)
  val hit         = Bool()
  val replay      = Bool()
  val hasData     = Bool()
  val nextCycleWb = Bool()
  val data        = UInt(dataWidth.W)
}

class DataExchangeIO extends Bundle {
  val req     = Flipped(Decoupled(new DataExchangeReq))
  val resp    = Valid(new DataExchangeResp)
  val s0_kill = Input(Bool())
  val s1_kill = Input(Bool())
}

class MainPipeReq extends Bundle {
  val source  = UInt(srcWidth.W)
  val paddr   = UInt(paddrWidth.W)
  val cmd     = UInt(M_SZ.W)
  val size    = UInt(log2Up(log2Up(dataBytes)).W)
  val signed  = Bool()
  val wdata   = UInt(dataWidth.W)
  val wmask   = UInt(dataBytes.W)
  val noAlloc = Bool()

  val isFromCore   = Bool()
  val isProbe      = Bool()
  val isReplace    = Bool()
  val perm         = UInt(TLPermissions.bdWidth.W) // probe permission
  val specifyValid = Bool()
  val specifyWay   = UInt(log2Up(nWays).W)
}

object MainPipeReqConverter {
  // DataExchangeIO => MainPipeReq
  def apply(req: DataExchangeReq): MainPipeReq = {
    val mainPipeReq = WireInit(0.U.asTypeOf(new MainPipeReq))

    mainPipeReq.source       := req.source
    mainPipeReq.paddr        := req.paddr
    mainPipeReq.cmd          := req.cmd
    mainPipeReq.size         := req.size
    mainPipeReq.signed       := req.signed
    mainPipeReq.wdata        := req.wdata
    mainPipeReq.wmask        := req.wmask
    mainPipeReq.noAlloc      := req.noAlloc
    mainPipeReq.isFromCore   := true.B
    mainPipeReq.specifyValid := req.specifyValid
    mainPipeReq.specifyWay   := req.specifyWay
    mainPipeReq
  }

  // ProbeReq => MainPipeReq
  def apply(req: TLBundleB): MainPipeReq = {
    val mainPipeReq = WireInit(0.U.asTypeOf(new MainPipeReq))

    mainPipeReq.source  := req.source
    mainPipeReq.paddr   := req.address
    mainPipeReq.cmd     := req.opcode
    mainPipeReq.isProbe := true.B
    mainPipeReq.perm    := req.param
    mainPipeReq
  }

}

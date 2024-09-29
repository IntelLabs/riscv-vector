package grapecoveDCache

import chisel3._
import chisel3.util._
import util._
import freechips.rocketchip.tilelink._

/////// MSHR Entry IO
class MSHREntryIO extends Bundle() {
  val reqValid      = Input(Bool())
  val req           = Input(MSHRReqType())
  val reqCmd        = Input(UInt(M_SZ.W))
  val allocLineAddr = Input(UInt(lineAddrWidth.W))
  val probeLineAddr = Input(UInt(lineAddrWidth.W))
  val isUpgrade     = Input(Bool()) // for BtoT

  val allocLineAddrMatch = Output(Bool()) // mshr inner tag match

  // replay & refill signals
  val replayFinish = Input(Bool())

  val writeCounter  = Output(UInt(log2Up(nMSHRMetas).W))
  val replayCounter = Output(UInt(log2Up(nMSHRMetas).W))

  // state flag for allocate
  val isEmpty  = Output(Bool())
  val stallReq = Output(Bool())

  // mshr sender port
  val senderResp       = Input(Bool()) // permitted to send this entry now
  val senderPermission = Output(UInt(TLPermissions.aWidth.W))
  val senderLineAddr   = Output(UInt(lineAddrWidth.W))

  // probe permission
  val probeValid         = Input(Bool())
  val probeLineAddrMatch = Output(Bool())
  val probePermission    = Input(UInt(TLPermissions.bdWidth.W))
  val probeState         = Output(UInt(ProbeMSHRState.width.W))
}

/////// replay reg IOs
class MSHRPipeResp extends Bundle() {
  val sourceId = UInt(MasterSource.width.W)
  val regIdx   = UInt(regAddrWidth.W)
  val regData  = UInt(blockBits.W)

  val nextCycleWb = Bool()
}

class MSHRReplace extends Bundle() {
  val state    = UInt(cohBits.W)       // for s0
  val lineAddr = UInt(lineAddrWidth.W) // for s0
  val data     = UInt(blockBits.W)     // for s2
}

class MSHRInner extends Bundle() {
  val perm     = UInt(TLPermissions.bdWidth.W)
  val lineAddr = UInt(lineAddrWidth.W)

  val meta = new ReqMetaBundle() // current tick read meta array
  val mask = UInt(blockBytes.W)
  val data = UInt(blockBits.W)

  val entryIdx = UInt(log2Up(nMSHRs).W) // init replay reg, set read meta array base addr
  val counter  = UInt(log2Up(nMSHRMetas).W)
}

class ReplayModuleIO extends Bundle() {
  val toPipe = ValidIO(new MSHRPipeResp())

  val toReplace     = DecoupledIO(new MSHRReplace())
  val replaceStatus = Input(ReplaceStatus())

  val innerIO = Flipped(DecoupledIO(new MSHRInner()))

  val idxMeta   = Output(UInt(log2Up(nMSHRMetas).W))
  val replayIdx = Output(UInt(log2Up(nMSHRs).W))
}

/////// MSHR file IO
class MetaBundle extends Bundle() {
  val sourceId = UInt(MasterSource.width.W)
  val regIdx   = UInt(regAddrWidth.W)
  val size     = UInt(log2Up(log2Up(dataBytes)).W)
  val signed   = Bool()
  val offset   = UInt(log2Up(blockBytes).W)
}

class ReqMetaBundle extends MetaBundle() {
  val cmd = UInt(M_SZ.W)
}

class CachepipeMSHRFile extends Bundle() {
  val isUpgrade = Bool()
  val lineAddr  = UInt(lineAddrWidth.W)
  val meta      = new ReqMetaBundle
  val mask      = UInt(blockBytes.W)
  val data      = UInt(blockBits.W)
}

class MSHRFileL2 extends Bundle() {
  val perm     = UInt(TLPermissions.aWidth.W)
  val entryId  = UInt(log2Up(nMSHRs + nWBQEntries + nMMIOs).W)
  val lineAddr = UInt(lineAddrWidth.W)
}

class RefillMSHRFile extends Bundle() {
  val entryId = UInt(log2Up(nMSHRs + nWBQEntries + nMMIOs).W)
  val perm    = UInt(TLPermissions.bdWidth.W)
  val data    = UInt(blockBits.W)
  val hasData = Bool()

  val probeMatch = Bool()
}

class ProbeMSHRFile extends Bundle() {
  val valid           = Input(Bool())
  val probePermission = Input(UInt(TLPermissions.bdWidth.W))
  val lineAddr        = Input(UInt(lineAddrWidth.W))

  val pass                      = Output(Bool())
  val hit                       = Output(Bool())
  val probeHitPassTLAcquirePerm = Output(UInt(TLPermissions.aWidth.W))

  val replaceFinish = Output(Bool())
}

class ProbeRefill extends Bundle() {
  val entryId = UInt(log2Up(nMSHRs).W)
}

package grapecoveDCache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import _root_.circt.stage.ChiselStage
//import grapecoveDCache._

class MSHRWrapper(
    implicit edge: TLEdgeOut
) extends Module {
  val io = IO(new Bundle {
    val req       = Flipped(DecoupledIO(new MainPipeReq))
    val cacheable = Input(Bool())
    val isUpgrade = Input(Bool())

    val l2Req      = DecoupledIO(new TLBundleA(edge.bundle))
    val fromRefill = Flipped(DecoupledIO(new RefillMSHRFile()))

    val nextCycleWb = Output(Bool())
    val resp        = ValidIO(new DataExchangeResp)

    val probeCheck    = new ProbeMSHRFile
    val probeRefill   = ValidIO(new ProbeRefill)
    val toReplace     = DecoupledIO(new MSHRReplace())
    val replaceStatus = Input(ReplaceStatus())
  })

  val mshrs   = Module(new MSHRFile())
  val iomshrs = Module(new IOMSHRFile())

  val toIOMSHR = !io.cacheable || isAMO(io.req.bits.cmd) || io.req.bits.noAlloc

  // req signal connect
  mshrs.io.pipelineReq.valid              := io.req.valid && !toIOMSHR
  mshrs.io.pipelineReq.bits.isUpgrade     := io.isUpgrade
  mshrs.io.pipelineReq.bits.data          := io.req.bits.wdata // FIXME
  mshrs.io.pipelineReq.bits.mask          := io.req.bits.wmask
  mshrs.io.pipelineReq.bits.lineAddr      := AddrDecoder.getLineAddr(io.req.bits.paddr)
  mshrs.io.pipelineReq.bits.meta.sourceId := io.req.bits.source
  mshrs.io.pipelineReq.bits.meta.offset   := AddrDecoder.getBlockOffset(io.req.bits.paddr)
  mshrs.io.pipelineReq.bits.meta.cmd      := io.req.bits.cmd
  mshrs.io.pipelineReq.bits.meta.regIdx   := io.req.bits.dest
  mshrs.io.pipelineReq.bits.meta.size     := io.req.bits.size
  mshrs.io.pipelineReq.bits.meta.signed   := io.req.bits.signed

  iomshrs.io.req.valid := io.req.valid && toIOMSHR
  iomshrs.io.req.bits  := io.req.bits

  io.req.ready := Mux(toIOMSHR, iomshrs.io.req.ready, mshrs.io.pipelineReq.ready)

  // request L2 using TL A
  val acquire = edge.AcquireBlock(
    fromSource = mshrs.io.toL2Req.bits.entryId,
    toAddress = mshrs.io.toL2Req.bits.lineAddr << blockOffBits,
    lgSize = log2Ceil(blockBytes).U,
    growPermissions = mshrs.io.toL2Req.bits.perm,
  )._2

  io.l2Req.valid         := mshrs.io.toL2Req.valid || iomshrs.io.l2Req.valid
  mshrs.io.toL2Req.ready := io.l2Req.ready
  iomshrs.io.l2Req.ready := io.l2Req.ready && !mshrs.io.toL2Req.valid
  io.l2Req.bits          := Mux(mshrs.io.toL2Req.valid, acquire, iomshrs.io.l2Req.bits)

  // refill data
  val refillMSHR = io.fromRefill.bits.entryId < nMSHRs.asUInt
  val refillIOMSHR = io.fromRefill.bits.entryId >= (nMSHRs + nWBQEntries).asUInt &&
    io.fromRefill.bits.entryId < (nMSHRs + nWBQEntries + nMMIOs).asUInt

//  assert(!refillMSHR && !refillIOMSHR && io.fromRefill.valid)

  mshrs.io.fromRefill.valid := io.fromRefill.valid && refillMSHR
  mshrs.io.fromRefill.bits  := io.fromRefill.bits

  iomshrs.io.fromRefill.valid := io.fromRefill.valid && refillIOMSHR // && io.fromRefill.bits.hasData
  iomshrs.io.fromRefill.bits  := io.fromRefill.bits

  io.fromRefill.ready := Mux(
    refillMSHR,
    mshrs.io.fromRefill.ready,
    Mux(refillIOMSHR, iomshrs.io.fromRefill.ready, false.B),
  )

  // load resp to cpu
  val mshrsResp = Wire(new DataExchangeResp)
  mshrsResp.status  := CacheRespStatus.refill
  mshrsResp.source  := mshrs.io.toPipeline.bits.sourceId
  mshrsResp.dest    := mshrs.io.toPipeline.bits.regIdx
  mshrsResp.data    := mshrs.io.toPipeline.bits.regData
  mshrsResp.hasData := mshrs.io.toPipeline.valid

  val iomshrWbSucc = RegInit(false.B)
  iomshrWbSucc := Mux(iomshrWbSucc || mshrs.io.toPipeline.bits.nextCycleWb, false.B, iomshrs.io.resp.valid)

  io.nextCycleWb        := mshrs.io.toPipeline.bits.nextCycleWb || (iomshrs.io.resp.valid && !iomshrWbSucc)
  io.resp.valid         := mshrs.io.toPipeline.valid || iomshrWbSucc
  io.resp.bits          := Mux(iomshrWbSucc, iomshrs.io.resp.bits, mshrsResp)
  iomshrs.io.resp.ready := iomshrWbSucc

  // others for MSHR
  io.toReplace <> mshrs.io.toReplace
  io.probeRefill <> mshrs.io.probeRefill
  io.probeCheck <> mshrs.io.probeCheck
  mshrs.io.replaceStatus := io.replaceStatus

}

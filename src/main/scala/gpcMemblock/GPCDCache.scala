package gpc.core

import grapecoveDCache._
import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.TLB
import freechips.rocketchip.rocket.TLBConfig
import freechips.rocketchip.rocket.HellaCacheExceptions
import _root_.circt.stage.ChiselStage

class GPCDCacheWrapper(staticIdForMetadataUseOnly: Int)(
    implicit p: Parameters
) extends MemBlock(staticIdForMetadataUseOnly)(p) {
  override lazy val module = new GPCDCacheWrapperModule(this)

  val dcacheClient = LazyModule(new GPCDCache()(p))

  override val node = dcacheClient.node
}

class GPCDCacheWrapperModule(outer: GPCDCacheWrapper) extends MemBlockModule(outer) {

  val (tlBus, _) = outer.node.out(0)
  val dcache     = outer.dcacheClient.module

  // FIXME
  val coreDataBytes = 64
  val nTLBSets      = 64
  val nTLBWays      = 4

  // * s0 begin
  io.tlb_port.req.ready := true.B

  val s0_valid     = io.cpu.req.fire
  val s0_req       = io.cpu.req.bits
  val s0_read      = isRead(s0_req.cmd)
  val s0_write     = isWrite(s0_req.cmd)
  val s0_readwrite = s0_read || s0_write || isPrefetch(s0_req.cmd)

  val s0_tlbReq = WireInit(io.tlb_port.req.bits)
  when(!io.tlb_port.req.fire) {
    s0_tlbReq.passthrough := s0_req.phys
    s0_tlbReq.vaddr       := s0_req.addr
    s0_tlbReq.size        := s0_req.size
    s0_tlbReq.cmd         := s0_req.cmd
    s0_tlbReq.prv         := s0_req.dprv
    s0_tlbReq.v           := s0_req.dv
  }

  // TLB
  val dtlb = Module(new TLB(false, log2Ceil(coreDataBytes), TLBConfig(nTLBSets, nTLBWays)))
  io.ptw <> dtlb.io.ptw

  dtlb.io.kill      := false.B
  dtlb.io.req.valid := io.tlb_port.req.fire || s0_valid && s0_readwrite
  dtlb.io.req.bits  := s0_tlbReq

  dtlb.io.sfence.valid     := s0_valid && (s0_req.cmd === M_SFENCE)
  dtlb.io.sfence.bits.rs1  := s0_req.size(0)
  dtlb.io.sfence.bits.rs2  := s0_req.size(1)
  dtlb.io.sfence.bits.addr := s0_req.addr
  dtlb.io.sfence.bits.asid := io.cpu.asid // FIXME: error, we should get s0_data
  dtlb.io.sfence.bits.hv   := s0_req.cmd === M_HFENCEV
  dtlb.io.sfence.bits.hg   := s0_req.cmd === M_HFENCEG

  io.tlb_port.s1_resp := dtlb.io.resp
  // * s0 end

  // * s1 begin

  val s1_req = RegEnable(s0_req, s0_valid)
  val s1_valid = RegNext(s0_valid) && !io.cpu.s1_kill &&
    !(s1_req.cmd === M_SFENCE || s1_req.cmd === M_HFENCEV || s1_req.cmd === M_HFENCEG)
  // tlb resp
  val s1_tlbResp    = RegEnable(dtlb.io.resp, s0_valid)
  val s1_tlbMiss    = s1_tlbResp.miss
  val s1_tlbXcpt    = WireInit(0.U.asTypeOf(new HellaCacheExceptions))
  val s1_tlbHasXcpt = s1_tlbXcpt.asUInt.orR
  s1_tlbXcpt := s1_tlbResp

  // construct dcache request
  val cacheReqValid = s1_valid && !s1_tlbHasXcpt && !s1_tlbMiss // FIXME
  val s1_replayReq  = cacheReqValid && !dcache.io.req.ready

  val cacheReq = WireInit(0.U.asTypeOf(new DataExchangeReq))
  cacheReq.paddr  := s1_tlbResp.paddr
  cacheReq.cmd    := s1_req.cmd
  cacheReq.size   := s1_req.size
  cacheReq.signed := s1_req.signed
  cacheReq.wdata  := io.cpu.s1_wdata
  cacheReq.wmask  := io.cpu.s1_wmask
  // FIXME
  cacheReq.noAlloc := false.B
  cacheReq.source  := s1_req.source
  cacheReq.dest    := s1_req.dest

  // DCache
  dcache.io.s1_kill   := io.cpu.s2_kill
  dcache.io.req.valid := cacheReqValid
  dcache.io.req.bits  := cacheReq

  // * s2 begin
  val s2_valid          = RegNext(s1_valid) && !io.cpu.s2_kill
  val s2_tlbResp        = RegEnable(s1_tlbResp, s1_valid)
  val s2_cacheRespValid = dcache.io.resp.valid
  val s2_cacheResp      = dcache.io.resp.bits
  val s2_replayReq      = RegNext(s1_replayReq)

  // cache resp
  io.cpu.resp.valid        := s2_cacheRespValid
  io.cpu.resp.bits.status  := s2_cacheResp.status
  io.cpu.resp.bits.hasData := s2_cacheResp.hasData
  io.cpu.resp.bits.data    := s2_cacheResp.data
  io.cpu.resp.bits.source  := s2_cacheResp.source
  io.cpu.resp.bits.dest    := s2_cacheResp.dest

  io.cpu.fenceRdy    := dcache.io.fenceRdy
  io.cpu.nextCycleWb := dcache.io.nextCycleWb

  // resp related with TLB
  when(s2_valid) {
    io.cpu.s2_xcpt := s2_tlbResp
  }.otherwise {
    io.cpu.s2_xcpt := 0.U.asTypeOf(new HellaCacheExceptions)
  }

  io.cpu.req.ready := true.B
  when(!dtlb.io.req.ready && !s0_req.phys || s1_replayReq) {
    io.cpu.req.ready := false.B
  }
}

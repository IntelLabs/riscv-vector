
package freechips.rocketchip.rocket

import grapecoveDCache._
import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import _root_.circt.stage.ChiselStage

class GPCDCacheWrapper(staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends HellaCache(staticIdForMetadataUseOnly)(p) {
  override lazy val module = new GPCDCacheWrapperModule(this)

  val dcacheClient = LazyModule(new GPCDCache()(p))

  override val node = dcacheClient.node
}

class GPCDCacheWrapperModule(outer: GPCDCacheWrapper) extends HellaCacheModule(outer) {

  val (tlBus, _) = outer.node.out(0)
  val dcache = outer.dcacheClient.module

  dontTouch(io)

  // * s0 begin
  io.tlb_port.req.ready := true.B

  val s0_valid = io.cpu.req.fire
  val s0_req = io.cpu.req.bits
  val s0_read = isRead(s0_req.cmd)
  val s0_write = isWrite(s0_req.cmd)
  val s0_readwrite = s0_read || s0_write || isPrefetch(s0_req.cmd)

  val s0_tlbReq = WireInit(io.tlb_port.req.bits)
  when(!io.tlb_port.req.fire) {
    s0_tlbReq.passthrough := s0_req.phys
    s0_tlbReq.vaddr := s0_req.addr
    s0_tlbReq.size := s0_req.size
    s0_tlbReq.cmd := s0_req.cmd
    s0_tlbReq.prv := s0_req.dprv
    s0_tlbReq.v := s0_req.dv
  }

  // TLB
  val dtlb = Module(new TLB(false, log2Ceil(coreDataBytes), TLBConfig(nTLBSets, nTLBWays)))
  io.ptw <> dtlb.io.ptw

  dtlb.io.kill := false.B
  dtlb.io.req.valid := io.tlb_port.req.fire || s0_valid && s0_readwrite
  dtlb.io.req.bits := s0_tlbReq

  dtlb.io.sfence.valid := s0_valid && (s0_req.cmd === M_SFENCE)
  dtlb.io.sfence.bits.rs1 := s0_req.size(0)
  dtlb.io.sfence.bits.rs2 := s0_req.size(1)
  dtlb.io.sfence.bits.addr := s0_req.addr
  dtlb.io.sfence.bits.asid := io.cpu.s1_data.data // FIXME: error, we should get s0_data
  dtlb.io.sfence.bits.hv := s0_req.cmd === M_HFENCEV
  dtlb.io.sfence.bits.hg := s0_req.cmd === M_HFENCEG

  io.tlb_port.s1_resp := dtlb.io.resp
  // * s0 end

  // * s1 begin
  val s1_req = RegEnable(s0_req, s0_valid)
  val s1_valid = RegNext(s0_valid) && !io.cpu.s1_kill && 
    !(s1_req.cmd === M_SFENCE || s1_req.cmd === M_HFENCEV || s1_req.cmd === M_HFENCEG)
  
    // tlb resp
  val s1_tlbResp = RegEnable(dtlb.io.resp, s0_valid)
  val s1_tlbMiss = s1_tlbResp.miss
  val s1_tlbXcpt = WireInit(0.U.asTypeOf(new HellaCacheExceptions))
  val s1_tlbHasXcpt = s1_tlbXcpt.asUInt.orR
  s1_tlbXcpt := s1_tlbResp

  // construct dcache request
  val cacheReqValid = s1_valid && !s1_tlbHasXcpt && !s1_tlbMiss // FIXME
  val s1_replayReq = cacheReqValid && !dcache.io.req.ready

  val cacheReq = WireInit(0.U.asTypeOf(new DataExchangeReq))
  cacheReq.paddr := s1_tlbResp.paddr
  cacheReq.cmd := s1_req.cmd
  cacheReq.size := s1_req.size
  cacheReq.signed := s1_req.signed
  cacheReq.wdata := io.cpu.s1_data.data
  cacheReq.wmask := io.cpu.s1_data.mask
  // FIXME
  cacheReq.noAlloc := false.B
  cacheReq.source := s1_req.tag(1, 0) // only fpu
  cacheReq.dest := s1_req.tag(6, 2) //
  
  // DCache
  dcache.io.s1_kill := io.cpu.s2_kill
  dcache.io.req.valid := cacheReqValid
  dcache.io.req.bits := cacheReq


  // * s2 begin
  val s2_valid = RegNext(s1_valid) && !io.cpu.s2_kill
  val s2_tlbResp = RegEnable(s1_tlbResp, s1_valid)
  val s2_cacheRespValid = dcache.io.resp.valid
  val s2_cacheResp = dcache.io.resp.bits
  val s2_replayReq = RegNext(s1_replayReq)

  // Resp
  io.cpu := DontCare
  io.errors := DontCare
  io.cpu.s2_nack := (s2_valid && s2_tlbResp.miss) || 
    ((s2_cacheRespValid && s2_cacheResp.status === CacheRespStatus.replay)) || 
    s2_replayReq
  io.cpu.s2_nack_cause_raw := false.B
  io.cpu.s2_uncached := false.B
  io.cpu.ordered := dcache.io.fenceRdy

  // cache resp 
  dontTouch(s2_cacheResp)
  io.cpu.resp.bits := DontCare
  io.cpu.resp.valid := s2_cacheRespValid && (s2_cacheResp.status =/= CacheRespStatus.miss && s2_cacheResp.status =/= CacheRespStatus.replay)
  io.cpu.resp.bits.has_data := s2_cacheResp.hasData
  io.cpu.resp.bits.data := s2_cacheResp.data
  io.cpu.resp.bits.data_word_bypass := s2_cacheResp.data // FIXME loadgen.word
  io.cpu.resp.bits.replay := (s2_cacheResp.status === CacheRespStatus.refill)
  io.cpu.resp.bits.data_raw := DontCare // ignore
  io.cpu.resp.bits.tag := Cat(s2_cacheResp.dest, s2_cacheResp.source)
  io.cpu.s2_paddr := DontCare // FIXME
  io.cpu.store_pending := false.B // FIXME
  io.cpu.replay_next := dcache.io.nextCycleWb
  io.cpu.perf := 0.U.asTypeOf(new HellaCachePerfEvents)
  io.cpu.clock_enabled := true.B

  // resp related with TLB
  when(s2_valid) {
    io.cpu.s2_xcpt := s2_tlbResp
  }.otherwise {
    io.cpu.s2_xcpt := 0.U.asTypeOf(new HellaCacheExceptions)
  }
  io.cpu.s2_gpa := s2_tlbResp.gpa
  io.cpu.s2_gpa_is_pte := s2_tlbResp.gpa_is_pte


  io.cpu.req.ready := true.B 
  when(!dtlb.io.req.ready && !s0_req.phys || s1_replayReq) {
    io.cpu.req.ready := false.B
  }

  // * s2 end
}
package gpc.core

import chisel3._
import chisel3.util._
import chisel3.{withClock,withReset}
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLWidthWidget}
import freechips.rocketchip.util._
import freechips.rocketchip.util.property

import freechips.rocketchip.rocket.{CanHavePTW, CanHavePTWModule}
import freechips.rocketchip.rocket.{TLB, TLBConfig, ICacheParams}
import freechips.rocketchip.rocket.{M_XRD}

/*
  For dual-issue fetch group 64 bits
----------------------------------------
  fetchWidth = 4 (change from 2 to 4)
  coreInstBits = 16 (not change)
  coreInstBytes = 2 (not change)
  fetchBytes = fetchWidth * instBytes = 4 * 2 = 8
    8 byte size fetched by CPU for each cycle (change from 4 to 8)
  retireWidth = 2 (change from 1 to 2)
*/ 

class FrontendGpc(val icacheParams: ICacheParams, staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends LazyModule {
  lazy val module = new FrontendModuleGpc(this)
  val icache = LazyModule(new ICache(icacheParams, staticIdForMetadataUseOnly))
  val masterNode = icache.masterNode
  //val slaveNode = icache.slaveNode
  val resetVectorSinkNode = BundleBridgeSink[UInt](Some(() => UInt(masterNode.edges.out.head.bundle.addressBits.W)))
}

class FrontendModuleGpc(outer: FrontendGpc) extends LazyModuleImp(outer) 
      with HasGpcCoreParameters
      with HasL1ICacheParameters {
  val io = IO(new FrontendBundle(outer))
  val io_reset_vector = outer.resetVectorSinkNode.bundle
  implicit val edge = outer.masterNode.edges.out(0)

  /* -------------------------- BPU ---------------------------- */
  val btb = Module(new BTB)
  /* -------------------------- TLB ---------------------------- */
  val tlb = Module(new TLB(true, log2Ceil(fetchBytes), TLBConfig(nTLBSets, nTLBWays, outer.icacheParams.nTLBBasePageSectors, outer.icacheParams.nTLBSuperpages)))
  /* ------------------------ I-Cache -------------------------- */
  val icache = outer.icache.module
  require(fetchWidth*coreInstBytes == outer.icacheParams.fetchBytes)
  /* ---------------------- Pre-Decoder ------------------------ */
  val decode_insts = Module(new PreDecoder)
  /* --------------------- Fetch Buffer ------------------------ */
  val fb = Module(new FetchBuffer)
  //val fq = withReset(reset.asBool || io.cpu.req.valid) { Module(new ShiftQueue(new FrontendResp, 5, flow = true)) }

  val clock_en_reg = Reg(Bool())
  val clock_en = clock_en_reg || io.cpu.might_request

  assert(!(io.cpu.req.valid || io.cpu.sfence.valid || io.cpu.flush_icache || io.cpu.bht_update.valid || io.cpu.btb_update.valid) || io.cpu.might_request)
  val gated_clock =
    if (!gpcParams.clockGate) clock
    else ClockGate(clock, clock_en, "icache_clock_gate")

  icache.clock := gated_clock
  icache.io.clock_enabled := clock_en
  withClock (gated_clock) { // entering gated-clock domain

  //---------------------------------------------------------------
  //                         F0 stage
  //---------------------------------------------------------------
  val f0_pc = WireInit(0.U(vaddrBitsExtended.W))
  val f0_valid = WireInit(false.B)
  val f0_fb_has_space = Wire(Bool())
  val f0_speculative = Wire(Bool())

  f0_valid := io.cpu.req.valid || f0_fb_has_space 
  
  btb.io.req.valid := f0_valid && !io.cpu.sfence.valid
  btb.io.req.bits.addr := f0_pc 

  icache.io.req.valid := f0_valid
  icache.io.req.bits.addr := f0_pc
  icache.io.invalidate := io.cpu.flush_icache

  val recent_progress_counter_init = 3.U
  val recent_progress_counter = RegInit(recent_progress_counter_init)
  val recent_progress = recent_progress_counter > 0.U
  when(io.ptw.req.fire && recent_progress) { recent_progress_counter := recent_progress_counter - 1.U }
  when(io.cpu.progress) { recent_progress_counter := recent_progress_counter_init }
  //---------------------------------------------------------------
  //                         F1 stage
  //---------------------------------------------------------------
  val f1_pc = RegNext(f0_pc)
  val f1_valid = Reg(Bool())
  val f1_speculative = Reg(Bool())
  val f1_next_fetch = nextFetch(f1_pc)
  val f1_predicted_taken = WireInit(false.B)
  val f1_target = Mux(f1_predicted_taken,btb.io.resp.bits.target.sextTo(vaddrBitsExtended),f1_next_fetch)
  val f1f2_clear = WireInit(false.B)
  val f1_specualtive = Reg(Bool())
  
  f1_valid := f0_valid
  f1_predicted_taken := btb.io.resp.valid && btb.io.resp.bits.bht.taken
  
  btb.io.btb_update := io.cpu.btb_update
  btb.io.bht_update := io.cpu.bht_update
  btb.io.ijtp_update.valid := false.B
  btb.io.ijtp_update.bits := DontCare
  btb.io.ras_update.valid := false.B
  btb.io.ras_update.bits := DontCare
  btb.io.bht_advance.valid := false.B
  btb.io.bht_advance.bits := DontCare
  
  tlb.io.req.valid := f1_valid && !f1f2_clear 
  tlb.io.req.bits.cmd := M_XRD // Frontend only reads
  tlb.io.req.bits.vaddr := f1_pc
  tlb.io.req.bits.passthrough := false.B
  tlb.io.req.bits.size := log2Ceil(coreInstBytes*fetchWidth).U
  tlb.io.req.bits.prv := io.ptw.status.prv
  tlb.io.req.bits.v := io.ptw.status.v
  tlb.io.sfence := io.cpu.sfence
  io.ptw <> tlb.io.ptw                                                               
  
  icache.io.s1_paddr := tlb.io.resp.paddr

  //---------------------------------------------------------------
  //                         F2 stage
  //---------------------------------------------------------------
  val f2_pc = RegInit(t = UInt(vaddrBitsExtended.W), alignPC(io_reset_vector))
  val f2_base_pc = fetchAlign(f2_pc)
  val f2_valid = RegInit(false.B)
  val f2_btb_resp_valid = if (usingBTB) Reg(Bool()) else false.B
  val f2_btb_resp_bits = Reg(new BTBResp)
  val f2_btb_taken = f2_btb_resp_valid && f2_btb_resp_bits.taken
  val f2_tlb_resp = Reg(tlb.io.resp.cloneType)
  val f2_xcpt = f2_tlb_resp.ae.inst || f2_tlb_resp.pf.inst //|| f2_tlb_resp.gf.inst
  val f2_speculative = RegInit(false.B)
  val f2_prev_half = Reg(UInt(16.W))
  val f2_prev_is_half = RegInit(false.B)
  val f2_replay = Wire(Bool())
  val wrong_path = RegInit(false.B)
  val f2_can_speculatively_refill = f2_tlb_resp.cacheable 
  val f2_kill_speculative_tlb_refill = f2_speculative && !recent_progress
  val f2_correct_redirect = WireInit(false.B) 
  val f2_next_fetch = RegNext(nextFetch(f1_pc))
  val f2_target = Mux(f2_replay,f2_pc,Mux(f2_correct_redirect,Mux(decode_insts.io.redirect_return,btb.io.ras_head.bits,decode_insts.io.predict_npc),f2_next_fetch))
  val f2_redirect = WireInit(false.B)

  assert(!(f2_speculative && !icache.io.s2_kill))

  f0_pc := Mux(io.cpu.req.valid,io.cpu.req.bits.pc,Mux(f2_redirect,f2_target,f1_target))
  f0_fb_has_space := !fb.io.mask(fb.io.mask.getWidth-3) || 
                      !fb.io.mask(fb.io.mask.getWidth-2) && (!f1_valid || !f2_valid) || 
                      !fb.io.mask(fb.io.mask.getWidth-1) && (!f1_valid && !f2_valid)
  f0_speculative := f2_valid && !f2_speculative || f1_predicted_taken

  f1_speculative := Mux(io.cpu.req.valid, io.cpu.req.bits.speculative,Mux(f2_replay, f2_speculative, f0_speculative))

  //A replay is triggered if the handshake is not successfully completed
  f2_replay := (f2_valid && !fb.io.enq.fire) || RegNext(f2_replay && !f0_valid, true.B)
  dontTouch(fb.io.enq)
  //TODO: wether 'f2_valid && !icache.io.resp.valid' or not
  f1f2_clear := io.cpu.req.valid || f2_replay || f2_redirect
  f2_correct_redirect := decode_insts.io.correct_redirect
  f2_redirect := f2_replay || f2_correct_redirect //|| f1_valid && (f1_pc =/= f2_target)
  f2_valid := !f2_redirect

  tlb.io.kill := !f2_valid || f2_kill_speculative_tlb_refill
  
  icache.io.s2_vaddr := f2_pc
  icache.io.s2_cacheable := f2_tlb_resp.cacheable
  icache.io.s2_prefetch := f2_tlb_resp.prefetchable
  icache.io.s1_kill := tlb.io.resp.miss || f2_redirect //|| f1f2_clear
  icache.io.s2_kill := f2_speculative && !f2_can_speculatively_refill || f2_xcpt 
  
  when (!f1f2_clear) {
  //f2_valid := !f2_redirect
    f2_pc := f1_pc
    f2_speculative := f1_speculative
    f2_tlb_resp := tlb.io.resp
    f2_btb_resp_valid := btb.io.resp.valid
    f2_btb_resp_bits := btb.io.resp.bits
  }
  //redirect
  when (io.cpu.req.valid){
    f2_prev_is_half := false.B
  }
  when (icache.io.resp.valid && icache.io.resp.bits.ae) { fb.io.enq.bits.xcpt.ae.inst := true.B }
  //TODO: need to complete Predecode
  decode_insts.io.stage_valid := f2_valid
  decode_insts.io.pc := f2_pc
  decode_insts.io.data := icache.io.resp.bits.data
  decode_insts.io.btb_resp.bits := f2_btb_resp_bits
  decode_insts.io.btb_resp.valid := f2_btb_resp_valid
  decode_insts.io.prev_is_half := f2_prev_is_half
  decode_insts.io.prev_half_inst := f2_prev_half

    when(f2_valid && fb.io.enq.ready ){
      f2_prev_is_half := decode_insts.io.end_half.valid && !f2_correct_redirect
      f2_prev_half := decode_insts.io.end_half.bits
    }
  
  btb.io.flush := decode_insts.io.flush 
  
  // Early update of the BTB
  when (!io.cpu.btb_update.valid) {
      btb.io.btb_update.valid := fb.io.enq.fire && !wrong_path && decode_insts.io.btb_update
      btb.io.btb_update.bits.prediction.entry := tileParams.btb.get.nEntries.U
      btb.io.btb_update.bits.isValid := true.B
      btb.io.btb_update.bits.cfiType := btb.io.ras_update.bits.cfiType
      btb.io.btb_update.bits.br_pc := decode_insts.io.inst
      btb.io.btb_update.bits.pc := f2_base_pc
    }

  // Update RAS after pre-decoding
  btb.io.ras_update.valid := (decode_insts.io.is_call || decode_insts.io.is_return) && f2_valid && fb.io.enq.fire && !wrong_path
  btb.io.ras_update.bits.cfiType := Mux(decode_insts.io.is_return, CFIType.ret,
                                          Mux(decode_insts.io.is_call, CFIType.call,
                                          Mux(decode_insts.io.is_branch, CFIType.branch,
                                          Mux(decode_insts.io.is_jump, CFIType.jump,
                                          CFIType.indrjump))))
  btb.io.ras_update.bits.returnAddr := f2_base_pc + (decode_insts.io.redirect_bridx << log2Ceil(coreInstBytes)) + Mux(decode_insts.io.npc_plus4_mask(decode_insts.io.redirect_bridx), 4.U, 2.U)
  
  // Update IJTP
  when( btb.io.btb_update.valid && btb.io.btb_update.bits.cfiType === CFIType.indrjump){
      btb.io.ijtp_update.valid := true.B
      btb.io.ijtp_update.bits.pc := btb.io.btb_update.bits.pc
      btb.io.ijtp_update.bits.taken := btb.io.btb_update.bits.taken
      btb.io.ijtp_update.bits.jumpTarget := btb.io.btb_update.bits.br_pc
    }


  //TODO: need to complet Fetch Buffer
  fb.io.enq.valid := RegNext(f1_valid) && f2_valid && (icache.io.resp.valid || 
                                                      (f2_kill_speculative_tlb_refill && f2_tlb_resp.miss) || 
                                                      (!f2_tlb_resp.miss && icache.io.s2_kill))
  fb.io.enq.bits.inst_mask := decode_insts.io.inst_mask
  fb.io.enq.bits.btb_resp := f2_btb_resp_bits
  fb.io.enq.bits.pcs := decode_insts.io.inst_pcs
  fb.io.enq.bits.inst_exp := decode_insts.io.inst_exp
  // need next_pc.valid?
  fb.io.enq.bits.next_pc.valid := f2_correct_redirect
  fb.io.enq.bits.next_pc.bits := f2_target
  fb.io.enq.bits.raw_insts := decode_insts.io.raw_insts
  fb.io.enq.bits.rvc := decode_insts.io.rvc
  fb.io.enq.bits.xcpt := f2_tlb_resp
  fb.io.enq.bits.replay := (icache.io.resp.bits.replay ||   //need it
                                      icache.io.s2_kill && !icache.io.resp.valid && !f2_xcpt) || 
                                      (f2_kill_speculative_tlb_refill && f2_tlb_resp.miss)
  fb.io.flush := false.B
  io.cpu.resp <> fb.io.deq

  // performance events
  io.cpu.perf.acquire := icache.io.perf.acquire
  io.cpu.perf.tlbMiss := io.ptw.req.fire
  io.errors := icache.io.errors

  // gate the clock
  clock_en_reg := !gpcParams.clockGate.B ||
    io.cpu.might_request || // chicken bit
    icache.io.keep_clock_enabled || // I$ miss or ITIM access
    f1_valid || f2_valid || // some fetch in flight
    !tlb.io.req.ready || // handling TLB miss
    !fb.io.mask(fb.io.mask.getWidth-1) // queue not full
  } // leaving gated-clock domain

  def alignPC(pc: UInt) = ~(~pc | (coreInstBytes - 1).U)
  def fetchAlign(addr: UInt) = ~(~addr | (fetchBytes-1).U) // fetch group align
  def nextFetch(addr: UInt) = fetchAlign(addr) + fetchBytes.U
  
}

/** Mix-ins for constructing tiles that have an ICache-based pipeline frontend */
trait HasICacheFrontendGpc extends CanHavePTW { this: BaseTile =>
  val module: HasICacheFrontendGpcModule
  val frontend = LazyModule(new FrontendGpc(ICacheParamsGpc.ICacheParams_gpc, tileId))
  tlMasterXbar.node := TLWidthWidget(ICacheParamsGpc.ICacheParams_gpc.rowBits/8) := frontend.masterNode
  frontend.resetVectorSinkNode := resetVectorNexusNode
  nPTWPorts += 1

  // This should be a None in the case of not having an ITIM address, when we
  // don't actually use the device that is instantiated in the frontend.
  private val deviceOpt = None
}

trait HasICacheFrontendGpcModule extends CanHavePTWModule {
  val outer: HasICacheFrontendGpc
  ptwPorts += outer.frontend.module.io.ptw
}
// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

trait HasMissInfo extends Bundle with HasL1HellaCacheParameters {
  val tag_match = Bool()
  val old_meta = new L1Metadata
  val way_en = Bits(nWays.W)
}

class L1DataReadReq(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
  val way_en = Bits(nWays.W)
  val addr   = Bits(untagBits.W)
}

class L1DataWriteReq(implicit p: Parameters) extends L1DataReadReq()(p) {
  val wmask  = Bits(rowWords.W)
  val data   = Bits(encRowBits.W)
}

class L1RefillReq(implicit p: Parameters) extends L1DataReadReq()(p)

class Replay(implicit p: Parameters) extends HellaCacheReqInternal()(p) with HasCoreData

class ReplayInternal(implicit p: Parameters) extends HellaCacheReqInternal()(p)
    with HasL1HellaCacheParameters {
  val sdq_id = UInt(log2Up(cfg.nSDQ).W)
}

class MSHRReq(implicit p: Parameters) extends Replay()(p) with HasMissInfo

class MSHRReqInternal(implicit p: Parameters) extends ReplayInternal()(p) with HasMissInfo

class WritebackReq(params: TLBundleParameters)(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
  val tag = Bits(tagBits.W)
  val idx = Bits(idxBits.W)
  val source = UInt(params.sourceBits.W)
  val param = UInt(TLPermissions.cWidth.W) 
  val way_en = Bits(nWays.W)
  val voluntary = Bool()

}

class IOMSHR(id: Int)(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p) {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new HellaCacheReq))
    val resp = Decoupled(new HellaCacheResp)
    val mem_access = Decoupled(new TLBundleA(edge.bundle))
    val mem_ack = Flipped(Valid(new TLBundleD(edge.bundle)))
    val replay_next = Output(Bool())
  })

  def beatOffset(addr: UInt) = addr.extract(beatOffBits - 1, wordOffBits)

  def wordFromBeat(addr: UInt, dat: UInt) = {
    val shift = Cat(beatOffset(addr), 0.U((wordOffBits + log2Up(wordBytes)).W))
    (dat >> shift)(wordBits - 1, 0)
  }

  val req = Reg(new HellaCacheReq)
  val grant_word = Reg(UInt(wordBits.W))

  val s_idle :: s_mem_access :: s_mem_ack :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  io.req.ready := (state === s_idle)

  val loadgen = new LoadGen(req.size, req.signed, req.addr, grant_word, false.B, wordBytes)
 
  val a_source = id.U
  val a_address = req.addr
  val a_size = req.size
  val a_data = Fill(beatWords, req.data)

  val get     = edge.Get(a_source, a_address, a_size)._2
  val put     = edge.Put(a_source, a_address, a_size, a_data)._2
  val atomics = if (edge.manager.anySupportLogical) {
    MuxLookup(req.cmd, (0.U).asTypeOf(new TLBundleA(edge.bundle)), Array(
      M_XA_SWAP -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.SWAP)._2,
      M_XA_XOR  -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.XOR) ._2,
      M_XA_OR   -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.OR)  ._2,
      M_XA_AND  -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.AND) ._2,
      M_XA_ADD  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.ADD)._2,
      M_XA_MIN  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MIN)._2,
      M_XA_MAX  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MAX)._2,
      M_XA_MINU -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MINU)._2,
      M_XA_MAXU -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MAXU)._2))
  } else {
    // If no managers support atomics, assert fail if processor asks for them
    assert(state === s_idle || !isAMO(req.cmd))
    (0.U).asTypeOf(new TLBundleA(edge.bundle))
  }
  assert(state === s_idle || req.cmd =/= M_XSC)

  io.mem_access.valid := (state === s_mem_access)
  io.mem_access.bits := Mux(isAMO(req.cmd), atomics, Mux(isRead(req.cmd), get, put))

  io.replay_next := (state === s_mem_ack) || io.resp.valid && !io.resp.ready
  io.resp.valid := (state === s_resp)
  io.resp.bits.addr := req.addr
  io.resp.bits.idx.foreach(_ := req.idx.get)
  io.resp.bits.tag := req.tag
  io.resp.bits.cmd := req.cmd
  io.resp.bits.size := req.size
  io.resp.bits.signed := req.signed
  io.resp.bits.dprv := req.dprv
  io.resp.bits.dv := req.dv
  io.resp.bits.mask := req.mask
  io.resp.bits.has_data := isRead(req.cmd)
  io.resp.bits.data := loadgen.data
  io.resp.bits.data_raw := grant_word
  io.resp.bits.data_word_bypass := loadgen.wordData
  io.resp.bits.store_data := req.data
  io.resp.bits.replay := true.B

  when (io.req.fire) {
    req := io.req.bits
    state := s_mem_access
  }

  when (io.mem_access.fire) {
    state := s_mem_ack
  }

  when (state === s_mem_ack && io.mem_ack.valid) {
    state := s_resp
    when (isRead(req.cmd)) {
      grant_word := wordFromBeat(req.addr, io.mem_ack.bits.data)
    }
  }

  when (io.resp.fire) {
    state := s_idle
  }
}

class MSHR(id: Int)(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p) {
  val io = IO(new Bundle {
    val req_pri_val    = Input(Bool())
    val req_pri_rdy    = Output(Bool())
    val req_sec_val    = Input(Bool())
    val req_sec_rdy    = Output(Bool())
    val req_bits       = Input(new MSHRReqInternal())

    val idx_match       = Output(Bool())
    val tag             = Output(Bits(tagBits.W))

    val mem_acquire  = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(Valid(new TLBundleD(edge.bundle)))
    val mem_finish = Decoupled(new TLBundleE(edge.bundle))

    val refill = Output(new L1RefillReq()) // Data is bypassed
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay = Decoupled(new ReplayInternal)
    val wb_req = Decoupled(new WritebackReq(edge.bundle))
    val probe_rdy = Output(Bool())
  })

  val s_invalid :: s_wb_req :: s_wb_resp :: s_meta_clear :: s_refill_req :: s_refill_resp :: s_meta_write_req :: s_meta_write_resp :: s_drain_rpq :: Nil = Enum(9)
  val state = RegInit(s_invalid)

  val req = Reg(new MSHRReqInternal)
  val req_idx = req.addr(untagBits-1,blockOffBits)
  val req_tag = req.addr >> untagBits
  val req_block_addr = (req.addr >> blockOffBits) << blockOffBits
  val idx_match = req_idx === io.req_bits.addr(untagBits-1,blockOffBits)

  val new_coh = RegInit(ClientMetadata.onReset)
  val (_, shrink_param, coh_on_clear)    = req.old_meta.coh.onCacheControl(M_FLUSH)
  val grow_param                                  = new_coh.onAccess(req.cmd)._2
  val coh_on_grant                                = new_coh.onGrant(req.cmd, io.mem_grant.bits.param)
  // We only accept secondary misses if we haven't yet sent an Acquire to outer memory
  // or if the Acquire that was sent will obtain a Grant with sufficient permissions
  // to let us replay this new request. I.e. we don't handle multiple outstanding
  // Acquires on the same block for now.
  val (cmd_requires_second_acquire, is_hit_again, _, dirtier_coh, dirtier_cmd) =
    new_coh.onSecondaryAccess(req.cmd, io.req_bits.cmd)

  val states_before_refill = Seq(s_wb_req, s_wb_resp, s_meta_clear)
  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)
  val sec_rdy = idx_match &&
                  (state.isOneOf(states_before_refill) ||
                    (state.isOneOf(s_refill_req, s_refill_resp) &&
                      !cmd_requires_second_acquire && !refill_done))

  val rpq = Module(new Queue(new ReplayInternal, cfg.nRPQ))
  rpq.io.enq.valid := (io.req_pri_val && io.req_pri_rdy || io.req_sec_val && sec_rdy) && !isPrefetch(io.req_bits.cmd)
  rpq.io.enq.bits := io.req_bits
  rpq.io.deq.ready := (io.replay.ready && state === s_drain_rpq) || state === s_invalid

  val acked = Reg(Bool())
  when (io.mem_grant.valid) { acked := true.B }

  when (state === s_drain_rpq && !rpq.io.deq.valid) {
    state := s_invalid
  }
  when (state === s_meta_write_resp) {
    // this wait state allows us to catch RAW hazards on the tags via nack_victim
    state := s_drain_rpq
  }
  when (state === s_meta_write_req && io.meta_write.ready) {
    state := s_meta_write_resp
  }
  when (state === s_refill_resp && refill_done) {
    new_coh := coh_on_grant
    state := s_meta_write_req
  }
  when (io.mem_acquire.fire) { // s_refill_req
    state := s_refill_resp
  }
  when (state === s_meta_clear && io.meta_write.ready) {
    state := s_refill_req
  }
  when (state === s_wb_resp && io.wb_req.ready && acked) {
    state := s_meta_clear
  }
  when (io.wb_req.fire) { // s_wb_req
    state := s_wb_resp
  }
  when (io.req_sec_val && io.req_sec_rdy) { // s_wb_req, s_wb_resp, s_refill_req
    //If we get a secondary miss that needs more permissions before we've sent
    //  out the primary miss's Acquire, we can upgrade the permissions we're 
    //  going to ask for in s_refill_req
    req.cmd := dirtier_cmd
    when (is_hit_again) {
      new_coh := dirtier_coh
    }
  }
  when (io.req_pri_val && io.req_pri_rdy) {
    req := io.req_bits
    acked := false.B
    val old_coh = io.req_bits.old_meta.coh
    val needs_wb = old_coh.onCacheControl(M_FLUSH)._1
    val (is_hit, _, coh_on_hit) = old_coh.onAccess(io.req_bits.cmd)
    when (io.req_bits.tag_match) {
      when (is_hit) { // set dirty bit
        new_coh := coh_on_hit
        state := s_meta_write_req
      }.otherwise { // upgrade permissions
        new_coh := old_coh
        state := s_refill_req
      }
    }.otherwise { // writback if necessary and refill
      new_coh := ClientMetadata.onReset
      state := Mux(needs_wb, s_wb_req, s_meta_clear)
    }
  }

  val grantackq = Module(new Queue(new TLBundleE(edge.bundle), 1))
  val can_finish = state.isOneOf(s_invalid, s_refill_req)
  grantackq.io.enq.valid := refill_done && edge.isRequest(io.mem_grant.bits)
  grantackq.io.enq.bits := edge.GrantAck(io.mem_grant.bits)
  io.mem_finish.valid := grantackq.io.deq.valid && can_finish
  io.mem_finish.bits := grantackq.io.deq.bits
  grantackq.io.deq.ready := io.mem_finish.ready && can_finish

  io.idx_match := (state =/= s_invalid) && idx_match
  io.refill.way_en := req.way_en
  io.refill.addr := req_block_addr | refill_address_inc
  io.tag := req_tag 
  io.req_pri_rdy := state === s_invalid
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  val meta_hazard = RegInit(0.U(2.W))
  when (meta_hazard =/= 0.U) { meta_hazard := meta_hazard + 1.U }
  when (io.meta_write.fire) { meta_hazard := 1.U }
  io.probe_rdy := !idx_match || (!state.isOneOf(states_before_refill) && meta_hazard === 0.U)

  io.meta_write.valid := state.isOneOf(s_meta_write_req, s_meta_clear)
  io.meta_write.bits.idx := req_idx
  io.meta_write.bits.tag := io.tag
  io.meta_write.bits.data.coh := Mux(state === s_meta_clear, coh_on_clear, new_coh)
  io.meta_write.bits.data.tag := io.tag
  io.meta_write.bits.way_en := req.way_en

  io.wb_req.valid := state === s_wb_req
  io.wb_req.bits.source := id.U
  io.wb_req.bits.tag := req.old_meta.tag
  io.wb_req.bits.idx := req_idx
  io.wb_req.bits.param := shrink_param
  io.wb_req.bits.way_en := req.way_en
  io.wb_req.bits.voluntary := true.B

  io.mem_acquire.valid := state === s_refill_req && grantackq.io.enq.ready
  io.mem_acquire.bits := edge.AcquireBlock(
                                fromSource = id.U,
                                toAddress = Cat(io.tag, req_idx) << blockOffBits,
                                lgSize = lgCacheBlockBytes.U,
                                growPermissions = grow_param)._2

  io.meta_read.valid := state === s_drain_rpq
  io.meta_read.bits.idx := req_idx
  io.meta_read.bits.tag := io.tag
  io.meta_read.bits.way_en := ~(0.U(nWays.W))

  io.replay.valid := state === s_drain_rpq && rpq.io.deq.valid
  io.replay.bits := rpq.io.deq.bits
  io.replay.bits.phys := true.B
  io.replay.bits.addr := Cat(io.tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0))

  when (!io.meta_read.ready) {
    rpq.io.deq.ready := false.B
    io.replay.bits.cmd := M_FLUSH_ALL /* nop */
  }
}

class MSHRFile(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p) {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new MSHRReq))
    val resp = Decoupled(new HellaCacheResp)
    val secondary_miss = Output(Bool())

    val mem_acquire  = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(Valid(new TLBundleD(edge.bundle)))
    val mem_finish = Decoupled(new TLBundleE(edge.bundle))

    val refill = Output(new L1RefillReq())
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay = Decoupled(new Replay)
    val wb_req = Decoupled(new WritebackReq(edge.bundle))

    val probe_rdy = Output(Bool())
    val fence_rdy = Output(Bool())
    val replay_next = Output(Bool())
  })

  // determine if the request is cacheable or not
  val cacheable = edge.manager.supportsAcquireBFast(io.req.bits.addr, lgCacheBlockBytes.U)

  val sdq_val = RegInit(0.U(cfg.nSDQ.W))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(cfg.nSDQ-1,0))
  val sdq_rdy = !sdq_val.andR
  val sdq_enq = io.req.valid && io.req.ready && cacheable && isWrite(io.req.bits.cmd)
  val sdq = Mem(cfg.nSDQ, UInt(coreDataBits.W))
  when (sdq_enq) { sdq(sdq_alloc_id) := io.req.bits.data }

  val idxMatch = Wire(Vec(cfg.nMSHRs, Bool()))
  val tagList = Wire(Vec(cfg.nMSHRs, Bits(tagBits.W)))
  val tag_match = Mux1H(idxMatch, tagList) === io.req.bits.addr >> untagBits

  val wbTagList = Wire(Vec(cfg.nMSHRs, Bits()))
  val refillMux = Wire(Vec(cfg.nMSHRs, new L1RefillReq))
  val meta_read_arb = Module(new Arbiter(new L1MetaReadReq, cfg.nMSHRs))
  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq, cfg.nMSHRs))
  val wb_req_arb = Module(new Arbiter(new WritebackReq(edge.bundle), cfg.nMSHRs))
  val replay_arb = Module(new Arbiter(new ReplayInternal, cfg.nMSHRs))
  val alloc_arb = Module(new Arbiter(Bool(), cfg.nMSHRs))
  alloc_arb.io.in.foreach(_.bits := DontCare)

  var idx_match = false.B
  var pri_rdy = false.B
  var sec_rdy = false.B

  io.fence_rdy := true.B
  io.probe_rdy := true.B

  val mshrs = (0 until cfg.nMSHRs) map { i =>
    val mshr = Module(new MSHR(i))

    idxMatch(i) := mshr.io.idx_match
    tagList(i) := mshr.io.tag
    wbTagList(i) := mshr.io.wb_req.bits.tag

    alloc_arb.io.in(i).valid := mshr.io.req_pri_rdy
    mshr.io.req_pri_val := alloc_arb.io.in(i).ready

    mshr.io.req_sec_val := io.req.valid && sdq_rdy && tag_match
    mshr.io.req_bits.viewAsSupertype(new HellaCacheReqInternal) := io.req.bits.viewAsSupertype(new HellaCacheReqInternal)
    mshr.io.req_bits.tag_match := io.req.bits.tag_match
    mshr.io.req_bits.old_meta := io.req.bits.old_meta
    mshr.io.req_bits.way_en := io.req.bits.way_en
    mshr.io.req_bits.sdq_id := sdq_alloc_id

    meta_read_arb.io.in(i) <> mshr.io.meta_read
    meta_write_arb.io.in(i) <> mshr.io.meta_write
    wb_req_arb.io.in(i) <> mshr.io.wb_req
    replay_arb.io.in(i) <> mshr.io.replay

    mshr.io.mem_grant.valid := io.mem_grant.valid && io.mem_grant.bits.source === i.U
    mshr.io.mem_grant.bits := io.mem_grant.bits
    refillMux(i) := mshr.io.refill

    pri_rdy = pri_rdy || mshr.io.req_pri_rdy
    sec_rdy = sec_rdy || mshr.io.req_sec_rdy
    idx_match = idx_match || mshr.io.idx_match

    when (!mshr.io.req_pri_rdy) { io.fence_rdy := false.B }
    when (!mshr.io.probe_rdy) { io.probe_rdy := false.B }

    mshr
  }


  alloc_arb.io.out.ready := io.req.valid && sdq_rdy && cacheable && !idx_match

  io.meta_read <> meta_read_arb.io.out
  io.meta_write <> meta_write_arb.io.out
  io.wb_req <> wb_req_arb.io.out

  val mmio_alloc_arb = Module(new Arbiter(Bool(), nIOMSHRs))
  mmio_alloc_arb.io.in.foreach(_.bits := DontCare)
  val resp_arb = Module(new Arbiter(new HellaCacheResp, nIOMSHRs))

  var mmio_rdy = false.B
  io.replay_next := false.B

  val mmios = (0 until nIOMSHRs) map { i =>
    val id = cfg.nMSHRs + i
    val mshr = Module(new IOMSHR(id))

    mmio_alloc_arb.io.in(i).valid := mshr.io.req.ready
    mshr.io.req.valid := mmio_alloc_arb.io.in(i).ready
    mshr.io.req.bits := io.req.bits

    mmio_rdy = mmio_rdy || mshr.io.req.ready

    mshr.io.mem_ack.bits := io.mem_grant.bits
    mshr.io.mem_ack.valid := io.mem_grant.valid && io.mem_grant.bits.source === id.U

    resp_arb.io.in(i) <> mshr.io.resp

    when (!mshr.io.req.ready) { io.fence_rdy := false.B }
    when (mshr.io.replay_next) { io.replay_next := true.B }

    mshr
  }

  mmio_alloc_arb.io.out.ready := io.req.valid && !cacheable

  TLArbiter.lowestFromSeq(edge, io.mem_acquire, mshrs.map(_.io.mem_acquire) ++ mmios.map(_.io.mem_access))
  TLArbiter.lowestFromSeq(edge, io.mem_finish,  mshrs.map(_.io.mem_finish))

  io.resp <> resp_arb.io.out
  io.req.ready := Mux(!cacheable,
                    mmio_rdy,
                    sdq_rdy && Mux(idx_match, tag_match && sec_rdy, pri_rdy))
  io.secondary_miss := idx_match
  io.refill := refillMux(io.mem_grant.bits.source)

  val free_sdq = io.replay.fire && isWrite(io.replay.bits.cmd)
  io.replay.bits.data := sdq(RegEnable(replay_arb.io.out.bits.sdq_id, free_sdq))
  io.replay.bits.mask := 0.U
  io.replay.valid := replay_arb.io.out.valid
  replay_arb.io.out.ready := io.replay.ready
  io.replay.bits.viewAsSupertype(new HellaCacheReqInternal) <> replay_arb.io.out.bits.viewAsSupertype(new HellaCacheReqInternal)

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(replay_arb.io.out.bits.sdq_id) & Fill(cfg.nSDQ, free_sdq)) |
               PriorityEncoderOH(~sdq_val(cfg.nSDQ-1,0)) & Fill(cfg.nSDQ, sdq_enq)
  }
}

class WritebackUnit(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p) {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new WritebackReq(edge.bundle)))
    val meta_read = Decoupled(new L1MetaReadReq)
    val data_req = Decoupled(new L1DataReadReq)
    val data_resp = Input(Bits(encRowBits.W))
    val release = Decoupled(new TLBundleC(edge.bundle))
  })

  val req = Reg(new WritebackReq(edge.bundle))
  val active = RegInit(false.B)
  val r1_data_req_fired = RegInit(false.B)
  val r2_data_req_fired = RegInit(false.B)
  val data_req_cnt = RegInit(0.U(log2Up(refillCycles+1).W)) //TODO Zero width
  val (_, last_beat, all_beats_done, beat_count) = edge.count(io.release)

  io.release.valid := false.B
  when (active) {
    r1_data_req_fired := false.B
    r2_data_req_fired := r1_data_req_fired
    when (io.data_req.fire && io.meta_read.fire) {
      r1_data_req_fired := true.B
      data_req_cnt := data_req_cnt + 1.U
    }
    when (r2_data_req_fired) {
      io.release.valid := true.B
      when(!io.release.ready) {
        r1_data_req_fired := false.B
        r2_data_req_fired := false.B
        data_req_cnt := data_req_cnt - Mux[UInt]((refillCycles > 1).B && r1_data_req_fired, 2.U, 1.U)
      }
      when(!r1_data_req_fired) {
        // We're done if this is the final data request and the Release can be sent
        active := data_req_cnt < refillCycles.U || !io.release.ready
      }
    }
  }
  when (io.req.fire) {
    active := true.B
    data_req_cnt := 0.U
    req := io.req.bits
  }

  io.req.ready := !active

  val fire = active && data_req_cnt < refillCycles.U

  // We reissue the meta read as it sets up the mux ctrl for s2_data_muxed
  io.meta_read.valid := fire
  io.meta_read.bits.idx := req.idx
  io.meta_read.bits.tag := req.tag
  io.meta_read.bits.way_en := ~(0.U(nWays.W))

  io.data_req.valid := fire
  io.data_req.bits.way_en := req.way_en
  io.data_req.bits.addr := (if(refillCycles > 1) 
                              Cat(req.idx, data_req_cnt(log2Up(refillCycles)-1,0))
                            else req.idx) << rowOffBits

  val r_address = Cat(req.tag, req.idx) << blockOffBits
  val probeResponse = edge.ProbeAck(
                          fromSource = req.source,
                          toAddress = r_address,
                          lgSize = lgCacheBlockBytes.U,
                          reportPermissions = req.param,
                          data = io.data_resp)

  val voluntaryRelease = edge.Release(
                          fromSource = req.source,
                          toAddress = r_address,
                          lgSize = lgCacheBlockBytes.U,
                          shrinkPermissions = req.param,
                          data = io.data_resp)._2
                          
  io.release.bits := Mux(req.voluntary, voluntaryRelease, probeResponse)
}

class ProbeUnit(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p) {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new TLBundleB(edge.bundle)))
    val rep = Decoupled(new TLBundleC(edge.bundle))
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val wb_req = Decoupled(new WritebackReq(edge.bundle))
    val way_en = Input(Bits(nWays.W))
    val mshr_rdy = Input(Bool())
    val block_state = Input(new ClientMetadata())
  })

  val (s_invalid :: s_meta_read :: s_meta_resp :: s_mshr_req ::
       s_mshr_resp :: s_release :: s_writeback_req :: s_writeback_resp :: 
       s_meta_write :: Nil) = Enum(9)
  val state = RegInit(s_invalid)

  val req = Reg(new TLBundleB(edge.bundle))
  val req_idx = req.address(idxMSB, idxLSB)
  val req_tag = req.address >> untagBits

  val way_en = Reg(Bits())
  val tag_matches = way_en.orR
  val old_coh = Reg(new ClientMetadata)
  val miss_coh = ClientMetadata.onReset
  val reply_coh = Mux(tag_matches, old_coh, miss_coh)
  val (is_dirty, report_param, new_coh) = reply_coh.onProbe(req.param)

  io.req.ready := state === s_invalid
  io.rep.valid := state === s_release
  io.rep.bits := edge.ProbeAck(req, report_param)

  assert(!io.rep.valid || !edge.hasData(io.rep.bits),
    "ProbeUnit should not send ProbeAcks with data, WritebackUnit should handle it")

  io.meta_read.valid := state === s_meta_read
  io.meta_read.bits.idx := req_idx
  io.meta_read.bits.tag := req_tag
  io.meta_read.bits.way_en := ~(0.U(nWays.W))

  io.meta_write.valid := state === s_meta_write
  io.meta_write.bits.way_en := way_en
  io.meta_write.bits.idx := req_idx
  io.meta_write.bits.tag := req_tag
  io.meta_write.bits.data.tag := req_tag
  io.meta_write.bits.data.coh := new_coh

  io.wb_req.valid := state === s_writeback_req
  io.wb_req.bits.source := req.source
  io.wb_req.bits.idx := req_idx
  io.wb_req.bits.tag := req_tag
  io.wb_req.bits.param := report_param
  io.wb_req.bits.way_en := way_en
  io.wb_req.bits.voluntary := false.B

  // state === s_invalid
  when (io.req.fire) {
    state := s_meta_read
    req := io.req.bits
  }

  // state === s_meta_read
  when (io.meta_read.fire) {
    state := s_meta_resp
  }

  // we need to wait one cycle for the metadata to be read from the array
  when (state === s_meta_resp) {
    state := s_mshr_req
  }

  when (state === s_mshr_req) {
    old_coh := io.block_state
    way_en := io.way_en
    // if the read didn't go through, we need to retry
    state := Mux(io.mshr_rdy, s_mshr_resp, s_meta_read)
  }

  when (state === s_mshr_resp) {
    state := Mux(tag_matches && is_dirty, s_writeback_req, s_release)
  }

  when (state === s_release && io.rep.ready) {
    state := Mux(tag_matches, s_meta_write, s_invalid)
  }

  // state === s_writeback_req
  when (io.wb_req.fire) {
    state := s_writeback_resp
  }

  // wait for the writeback request to finish before updating the metadata
  when (state === s_writeback_resp && io.wb_req.ready) {
    state := s_meta_write
  }

  when (io.meta_write.fire) {
    state := s_invalid
  }
}

class DataArray(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = IO(new Bundle {
    val read = Flipped(Decoupled(new L1DataReadReq))
    val write = Flipped(Decoupled(new L1DataWriteReq))
    val resp = Output(Vec(nWays, Bits(encRowBits.W)))
  })

  val waddr = io.write.bits.addr >> rowOffBits
  val raddr = io.read.bits.addr >> rowOffBits

  if (doNarrowRead) {
    for (w <- 0 until nWays by rowWords) {
      val wway_en = io.write.bits.way_en(w+rowWords-1,w)
      val rway_en = io.read.bits.way_en(w+rowWords-1,w)
      val resp = Wire(Vec(rowWords, Bits(encRowBits.W)))
      val r_raddr = RegEnable(io.read.bits.addr, io.read.valid)
      for (i <- 0 until resp.size) {
        val array  = DescribedSRAM(
          name = s"array_${w}_${i}",
          desc = "Non-blocking DCache Data Array",
          size = nSets * refillCycles,
          data = Vec(rowWords, Bits(encDataBits.W))
        )
        when (wway_en.orR && io.write.valid && io.write.bits.wmask(i)) {
          val data = VecInit.fill(rowWords)(io.write.bits.data(encDataBits*(i+1)-1,encDataBits*i))
          array.write(waddr, data, wway_en.asBools)
        }
        resp(i) := array.read(raddr, rway_en.orR && io.read.valid).asUInt
      }
      for (dw <- 0 until rowWords) {
        val r = VecInit(resp.map(_(encDataBits*(dw+1)-1,encDataBits*dw)))
        val resp_mux =
          if (r.size == 1) r
          else VecInit(r(r_raddr(rowOffBits-1,wordOffBits)), r.tail:_*)
        io.resp(w+dw) := resp_mux.asUInt
      }
    }
  } else {
    for (w <- 0 until nWays) {
      val array  = DescribedSRAM(
        name = s"array_${w}",
        desc = "Non-blocking DCache Data Array",
        size = nSets * refillCycles,
        data = Vec(rowWords, Bits(encDataBits.W))
      )
      when (io.write.bits.way_en(w) && io.write.valid) {
        val data = VecInit.tabulate(rowWords)(i => io.write.bits.data(encDataBits*(i+1)-1,encDataBits*i))
        array.write(waddr, data, io.write.bits.wmask.asBools)
      }
      io.resp(w) := array.read(raddr, io.read.bits.way_en(w) && io.read.valid).asUInt
    }
  }

  io.read.ready := true.B
  io.write.ready := true.B
}

class NonBlockingDCache(staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends HellaCache(staticIdForMetadataUseOnly)(p) {
  override lazy val module = new NonBlockingDCacheModule(this)
}

class NonBlockingDCacheModule(outer: NonBlockingDCache) extends HellaCacheModule(outer) {

  require(isPow2(nWays)) // TODO: relax this
  require(dataScratchpadSize == 0)
  require(!usingVM || untagBits <= pgIdxBits, s"untagBits($untagBits) > pgIdxBits($pgIdxBits)")
  require(!cacheParams.separateUncachedResp)

  // ECC is only supported on the data array
  require(cacheParams.tagCode.isInstanceOf[IdentityCode])
  val dECC = cacheParams.dataCode

  io.cpu := DontCare
  io.errors := DontCare

  val wb = Module(new WritebackUnit)
  val prober = Module(new ProbeUnit)
  val mshrs = Module(new MSHRFile)

  io.cpu.req.ready := true.B
  val s1_valid = RegNext(io.cpu.req.fire, false.B)
  val s1_req = Reg(new HellaCacheReq)
  val s1_valid_masked = s1_valid && !io.cpu.s1_kill
  val s1_replay = RegInit(false.B)
  val s1_clk_en = Reg(Bool())
  val s1_sfence = s1_req.cmd === M_SFENCE

  val s2_valid = RegNext(s1_valid_masked && !s1_sfence, false.B) && !io.cpu.s2_xcpt.asUInt.orR
  val s2_req = Reg(new HellaCacheReq)
  val s2_replay = RegNext(s1_replay, false.B) && s2_req.cmd =/= M_FLUSH_ALL
  val s2_recycle = Wire(Bool())
  val s2_valid_masked = Wire(Bool())

  val s3_valid = RegInit(false.B)
  val s3_req = Reg(new HellaCacheReq)
  val s3_way = Reg(Bits())

  val s1_recycled = RegEnable(s2_recycle, false.B, s1_clk_en)
  val s1_read  = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)
  val s1_readwrite = s1_read || s1_write || isPrefetch(s1_req.cmd)
  // check for unsupported operations
  assert(!s1_valid || !s1_req.cmd.isOneOf(M_PWR))

  val dtlb = Module(new TLB(false, log2Ceil(coreDataBytes), TLBConfig(nTLBSets, nTLBWays)))
  io.ptw <> dtlb.io.ptw
  dtlb.io.kill := io.cpu.s2_kill
  dtlb.io.req.valid := s1_valid && !io.cpu.s1_kill && s1_readwrite
  dtlb.io.req.bits.passthrough := s1_req.phys
  dtlb.io.req.bits.vaddr := s1_req.addr
  dtlb.io.req.bits.size := s1_req.size
  dtlb.io.req.bits.cmd := s1_req.cmd
  dtlb.io.req.bits.prv := s1_req.dprv
  dtlb.io.req.bits.v := s1_req.dv
  when (!dtlb.io.req.ready && !io.cpu.req.bits.phys) { io.cpu.req.ready := false.B }

  dtlb.io.sfence.valid := s1_valid && !io.cpu.s1_kill && s1_sfence
  dtlb.io.sfence.bits.rs1 := s1_req.size(0)
  dtlb.io.sfence.bits.rs2 := s1_req.size(1)
  dtlb.io.sfence.bits.addr := s1_req.addr
  dtlb.io.sfence.bits.asid := io.cpu.s1_data.data
  dtlb.io.sfence.bits.hv := s1_req.cmd === M_HFENCEV
  dtlb.io.sfence.bits.hg := s1_req.cmd === M_HFENCEG

  when (io.cpu.req.valid) {
    s1_req := io.cpu.req.bits
  }
  when (wb.io.meta_read.valid) {
    s1_req.addr := Cat(wb.io.meta_read.bits.tag, wb.io.meta_read.bits.idx) << blockOffBits
    s1_req.phys := true.B
  }
  when (prober.io.meta_read.valid) {
    s1_req.addr := Cat(prober.io.meta_read.bits.tag, prober.io.meta_read.bits.idx) << blockOffBits
    s1_req.phys := true.B
  }
  when (mshrs.io.replay.valid) {
    s1_req := mshrs.io.replay.bits
  }
  when (s2_recycle) {
    s1_req := s2_req
  }
  val s1_addr = dtlb.io.resp.paddr

  when (s1_clk_en) {
    s2_req.size := s1_req.size
    s2_req.signed := s1_req.signed
    s2_req.phys := s1_req.phys
    s2_req.addr := s1_addr
    when (s1_write) {
      s2_req.data := Mux(s1_replay, mshrs.io.replay.bits.data, io.cpu.s1_data.data)
    }
    when (s1_recycled) { s2_req.data := s1_req.data }
    s2_req.tag := s1_req.tag
    s2_req.cmd := s1_req.cmd
  }

  // tags
  def onReset = L1Metadata(0.U, ClientMetadata.onReset)
  val meta = Module(new L1MetadataArray(() => onReset ))
  val metaReadArb = Module(new Arbiter(new L1MetaReadReq, 5))
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 2))
  meta.io.read <> metaReadArb.io.out
  meta.io.write <> metaWriteArb.io.out

  // data
  val data = Module(new DataArray)
  val readArb = Module(new Arbiter(new L1DataReadReq, 4))
  val writeArb = Module(new Arbiter(new L1DataWriteReq, 2))
  data.io.write.valid := writeArb.io.out.valid
  writeArb.io.out.ready := data.io.write.ready
  data.io.write.bits := writeArb.io.out.bits
  val wdata_encoded = (0 until rowWords).map(i => dECC.encode(writeArb.io.out.bits.data(coreDataBits*(i+1)-1,coreDataBits*i)))
  data.io.write.bits.data := wdata_encoded.asUInt

  // tag read for new requests
  metaReadArb.io.in(4).valid := io.cpu.req.valid
  metaReadArb.io.in(4).bits.idx := io.cpu.req.bits.addr >> blockOffBits
  metaReadArb.io.in(4).bits.tag := io.cpu.req.bits.addr >> untagBits
  metaReadArb.io.in(4).bits.way_en := ~0.U(nWays.W)
  when (!metaReadArb.io.in(4).ready) { io.cpu.req.ready := false.B }

  // data read for new requests
  readArb.io.in(3).valid := io.cpu.req.valid
  readArb.io.in(3).bits.addr := io.cpu.req.bits.addr
  readArb.io.in(3).bits.way_en := ~0.U(nWays.W)
  when (!readArb.io.in(3).ready) { io.cpu.req.ready := false.B }

  // recycled requests
  metaReadArb.io.in(0).valid := s2_recycle
  metaReadArb.io.in(0).bits.idx := s2_req.addr >> blockOffBits
  metaReadArb.io.in(0).bits.way_en := ~0.U(nWays.W)
  metaReadArb.io.in(0).bits.tag := s2_req.tag
  readArb.io.in(0).valid := s2_recycle
  readArb.io.in(0).bits.addr := s2_req.addr
  readArb.io.in(0).bits.way_en := ~0.U(nWays.W)

  // tag check and way muxing
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === (s1_addr >> untagBits)).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta.io.resp(w).coh.isValid()).asUInt
  s1_clk_en := metaReadArb.io.out.valid //TODO: should be metaReadArb.io.out.fire, but triggers Verilog backend bug
  val s1_writeback = s1_clk_en && !s1_valid && !s1_replay
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_clk_en)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_state = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegEnable(meta.io.resp(w).coh, s1_clk_en)))
  val (s2_has_permission, _, s2_new_hit_state) = s2_hit_state.onAccess(s2_req.cmd)
  val s2_hit = s2_tag_match && s2_has_permission && s2_hit_state === s2_new_hit_state

  // load-reserved/store-conditional
  val lrsc_count = RegInit(0.U)
  val lrsc_valid = lrsc_count > lrscBackoff.U
  val lrsc_addr = Reg(UInt())
  val (s2_lr, s2_sc) = (s2_req.cmd === M_XLR, s2_req.cmd === M_XSC)
  val s2_lrsc_addr_match = lrsc_valid && lrsc_addr === (s2_req.addr >> blockOffBits)
  val s2_sc_fail = s2_sc && !s2_lrsc_addr_match
  when (lrsc_count > 0.U) { lrsc_count := lrsc_count - 1.U }
  when (s2_valid_masked && s2_hit || s2_replay) {
    when (s2_lr) {
      lrsc_count := lrscCycles.U - 1.U
      lrsc_addr := s2_req.addr >> blockOffBits
    }
    when (lrsc_count > 0.U) {
      lrsc_count := 0.U
    }
  }
  when (s2_valid_masked && !(s2_tag_match && s2_has_permission) && s2_lrsc_addr_match) {
    lrsc_count := 0.U
  }

  val s2_data = Wire(Vec(nWays, Bits(encRowBits.W)))
  for (w <- 0 until nWays) {
    val regs = Reg(Vec(rowWords, Bits(encDataBits.W)))
    val en1 = s1_clk_en && s1_tag_eq_way(w)
    for (i <- 0 until regs.size) {
      val en = en1 && (((i == 0).B || !doNarrowRead.B) || s1_writeback)
      when (en) { regs(i) := data.io.resp(w) >> encDataBits*i }
    }
    s2_data(w) := regs.asUInt
  }
  val s2_data_muxed = Mux1H(s2_tag_match_way, s2_data)
  val s2_data_decoded = (0 until rowWords).map(i => dECC.decode(s2_data_muxed(encDataBits*(i+1)-1,encDataBits*i)))
  val s2_data_corrected = s2_data_decoded.map(_.corrected).asUInt
  val s2_data_uncorrected = s2_data_decoded.map(_.uncorrected).asUInt
  val s2_word_idx = if(doNarrowRead) 0.U else s2_req.addr(log2Up(rowWords*coreDataBytes)-1,log2Up(wordBytes))
  val s2_data_correctable = s2_data_decoded.map(_.correctable).asUInt(s2_word_idx)

  // store/amo hits
  s3_valid := (s2_valid_masked && s2_hit || s2_replay) && !s2_sc_fail && isWrite(s2_req.cmd)
  val amoalu = Module(new AMOALU(xLen))
  when ((s2_valid || s2_replay) && (isWrite(s2_req.cmd) || s2_data_correctable)) {
    s3_req := s2_req
    s3_req.data := Mux(s2_data_correctable, s2_data_corrected, amoalu.io.out)
    s3_way := s2_tag_match_way
  }

  writeArb.io.in(0).bits.addr := s3_req.addr
  writeArb.io.in(0).bits.wmask := UIntToOH(s3_req.addr.extract(rowOffBits-1,offsetlsb))
  writeArb.io.in(0).bits.data := Fill(rowWords, s3_req.data)
  writeArb.io.in(0).valid := s3_valid
  writeArb.io.in(0).bits.way_en :=  s3_way

  // replacement policy
  val replacer = cacheParams.replacement
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegEnable(replacer.way, s1_clk_en))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegEnable(meta.io.resp(w), s1_clk_en && s1_replaced_way_en(w))).toSeq)

  // miss handling
  mshrs.io.req.valid := s2_valid_masked && !s2_hit && (isPrefetch(s2_req.cmd) || isRead(s2_req.cmd) || isWrite(s2_req.cmd))
  mshrs.io.req.bits.viewAsSupertype(new Replay) := s2_req.viewAsSupertype(new HellaCacheReq)
  mshrs.io.req.bits.tag_match := s2_tag_match
  mshrs.io.req.bits.old_meta := Mux(s2_tag_match, L1Metadata(s2_repl_meta.tag, s2_hit_state), s2_repl_meta)
  mshrs.io.req.bits.way_en := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
  mshrs.io.req.bits.data := s2_req.data
  when (mshrs.io.req.fire) { replacer.miss }
  tl_out.a <> mshrs.io.mem_acquire

  // replays
  readArb.io.in(1).valid := mshrs.io.replay.valid
  readArb.io.in(1).bits.addr := mshrs.io.replay.bits.addr
  readArb.io.in(1).bits.way_en := ~0.U(nWays.W)
  mshrs.io.replay.ready := readArb.io.in(1).ready
  s1_replay := mshrs.io.replay.valid && readArb.io.in(1).ready
  metaReadArb.io.in(1) <> mshrs.io.meta_read
  metaWriteArb.io.in(0) <> mshrs.io.meta_write

  // probes and releases
  prober.io.req.valid := tl_out.b.valid && !lrsc_valid
  tl_out.b.ready := prober.io.req.ready && !lrsc_valid
  prober.io.req.bits := tl_out.b.bits
  prober.io.way_en := s2_tag_match_way
  prober.io.block_state := s2_hit_state
  metaReadArb.io.in(2) <> prober.io.meta_read
  metaWriteArb.io.in(1) <> prober.io.meta_write
  prober.io.mshr_rdy := mshrs.io.probe_rdy

  // refills
  val grant_has_data = edge.hasData(tl_out.d.bits)
  mshrs.io.mem_grant.valid := tl_out.d.fire
  mshrs.io.mem_grant.bits := tl_out.d.bits
  tl_out.d.ready := writeArb.io.in(1).ready || !grant_has_data
  /* The last clause here is necessary in order to prevent the responses for
   * the IOMSHRs from being written into the data array. It works because the
   * IOMSHR ids start right the ones for the regular MSHRs. */
  writeArb.io.in(1).valid := tl_out.d.valid && grant_has_data &&
                               tl_out.d.bits.source < cfg.nMSHRs.U
  writeArb.io.in(1).bits.addr := mshrs.io.refill.addr
  writeArb.io.in(1).bits.way_en := mshrs.io.refill.way_en
  writeArb.io.in(1).bits.wmask := ~0.U(rowWords.W)
  writeArb.io.in(1).bits.data := tl_out.d.bits.data(encRowBits-1,0)
  data.io.read <> readArb.io.out
  readArb.io.out.ready := !tl_out.d.valid || tl_out.d.ready // insert bubble if refill gets blocked
  tl_out.e <> mshrs.io.mem_finish

  // writebacks
  val wbArb = Module(new Arbiter(new WritebackReq(edge.bundle), 2))
  wbArb.io.in(0) <> prober.io.wb_req
  wbArb.io.in(1) <> mshrs.io.wb_req
  wb.io.req <> wbArb.io.out
  metaReadArb.io.in(3) <> wb.io.meta_read
  readArb.io.in(2) <> wb.io.data_req
  wb.io.data_resp := s2_data_corrected
  TLArbiter.lowest(edge, tl_out.c, wb.io.release, prober.io.rep)

  // store->load bypassing
  val s4_valid = RegNext(s3_valid, false.B)
  val s4_req = RegEnable(s3_req, s3_valid && metaReadArb.io.out.valid)
  val bypasses = List(
    ((s2_valid_masked || s2_replay) && !s2_sc_fail, s2_req, amoalu.io.out),
    (s3_valid, s3_req, s3_req.data),
    (s4_valid, s4_req, s4_req.data)
  ).map(r => (r._1 && (s1_addr >> wordOffBits === r._2.addr >> wordOffBits) && isWrite(r._2.cmd), r._3))
  val s2_store_bypass_data = Reg(Bits(coreDataBits.W))
  val s2_store_bypass = Reg(Bool())
  when (s1_clk_en) {
    s2_store_bypass := false.B
    when (bypasses.map(_._1).reduce(_||_)) {
      s2_store_bypass_data := PriorityMux(bypasses)
      s2_store_bypass := true.B
    }
  }

  // load data subword mux/sign extension
  val s2_data_word_prebypass = s2_data_uncorrected >> Cat(s2_word_idx, 0.U(log2Up(coreDataBits).W))
  val s2_data_word = Mux(s2_store_bypass, s2_store_bypass_data, s2_data_word_prebypass)
  val loadgen = new LoadGen(s2_req.size, s2_req.signed, s2_req.addr, s2_data_word, s2_sc, wordBytes)

  amoalu.io.mask := new StoreGen(s2_req.size, s2_req.addr, 0.U, xLen/8).mask
  amoalu.io.cmd := s2_req.cmd
  amoalu.io.lhs := s2_data_word
  amoalu.io.rhs := s2_req.data

  // nack it like it's hot
  val s1_nack = dtlb.io.req.valid && dtlb.io.resp.miss || io.cpu.s2_nack ||
                s1_req.addr(idxMSB,idxLSB) === prober.io.meta_write.bits.idx && !prober.io.req.ready
  val s2_nack_hit = RegEnable(s1_nack, s1_valid || s1_replay)
  when (s2_nack_hit) { mshrs.io.req.valid := false.B }
  val s2_nack_victim = s2_hit && mshrs.io.secondary_miss
  val s2_nack_miss = !s2_hit && !mshrs.io.req.ready
  val s2_nack = s2_nack_hit || s2_nack_victim || s2_nack_miss
  s2_valid_masked := s2_valid && !s2_nack && !io.cpu.s2_kill

  val s2_recycle_ecc = (s2_valid || s2_replay) && s2_hit && s2_data_correctable
  val s2_recycle_next = RegInit(false.B)
  when (s1_valid || s1_replay) { s2_recycle_next := s2_recycle_ecc }
  s2_recycle := s2_recycle_ecc || s2_recycle_next

  // after a nack, block until nack condition resolves to save energy
  val block_miss = RegInit(false.B)
  block_miss := (s2_valid || block_miss) && s2_nack_miss
  when (block_miss || s1_nack) {
    io.cpu.req.ready := false.B
  }

  val cache_resp = Wire(Valid(new HellaCacheResp))
  cache_resp.valid := (s2_replay || s2_valid_masked && s2_hit) && !s2_data_correctable
  cache_resp.bits.addr := s2_req.addr
  cache_resp.bits.idx.foreach(_ := s2_req.idx.get)
  cache_resp.bits.tag := s2_req.tag
  cache_resp.bits.cmd := s2_req.cmd
  cache_resp.bits.size := s2_req.size
  cache_resp.bits.signed := s2_req.signed
  cache_resp.bits.dprv := s2_req.dprv
  cache_resp.bits.dv := s2_req.dv
  cache_resp.bits.data_word_bypass := loadgen.wordData
  cache_resp.bits.data_raw := s2_data_word
  cache_resp.bits.mask := s2_req.mask
  cache_resp.bits.has_data := isRead(s2_req.cmd)
  cache_resp.bits.data := loadgen.data | s2_sc_fail
  cache_resp.bits.store_data := s2_req.data
  cache_resp.bits.replay := s2_replay

  val uncache_resp = Wire(Valid(new HellaCacheResp))
  uncache_resp.bits := mshrs.io.resp.bits
  uncache_resp.valid := mshrs.io.resp.valid
  mshrs.io.resp.ready := RegNext(!(s1_valid || s1_replay))

  io.cpu.s2_nack := s2_valid && s2_nack
  io.cpu.resp := Mux(mshrs.io.resp.ready, uncache_resp, cache_resp)
  io.cpu.resp.bits.data_word_bypass := loadgen.wordData
  io.cpu.resp.bits.data_raw := s2_data_word
  io.cpu.ordered := mshrs.io.fence_rdy && !s1_valid && !s2_valid
  io.cpu.replay_next := (s1_replay && s1_read) || mshrs.io.replay_next

  val s1_xcpt_valid = dtlb.io.req.valid && !s1_nack
  val s1_xcpt = dtlb.io.resp
  io.cpu.s2_xcpt := Mux(RegNext(s1_xcpt_valid), RegEnable(s1_xcpt, s1_clk_en), 0.U.asTypeOf(s1_xcpt))
  io.cpu.s2_uncached := false.B
  io.cpu.s2_paddr := s2_req.addr

  // performance events
  io.cpu.perf.acquire := edge.done(tl_out.a)
  io.cpu.perf.release := edge.done(tl_out.c)
  io.cpu.perf.tlbMiss := io.ptw.req.fire

  // no clock-gating support
  io.cpu.clock_enabled := true.B
}

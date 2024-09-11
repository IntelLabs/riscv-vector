// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package gpc.core

import chisel3._
import chisel3.util.{Cat, Decoupled, Mux1H, OHToUInt, RegEnable, Valid, isPow2, log2Ceil, log2Up, PopCount}
import freechips.rocketchip.amba._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{DescribedSRAM, _}
import freechips.rocketchip.util.property
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.dontTouch
import chisel3.util.random.LFSR

import freechips.rocketchip.rocket.ICacheParams

/** Parameter of [[ICache]].
  *
  * @param nSets number of sets.
  * @param nWays number of ways.
  * @param rowBits L1Cache parameter
  * @param nTLBSets TLB sets
  * @param nTLBWays TLB ways
  * @param nTLBBasePageSectors TLB BasePageSectors
  * @param nTLBSuperpages TLB Superpages
  * @param tagECC tag ECC, will be parsed to [[freechips.rocketchip.util.Code]].
  * @param dataECC data ECC, will be parsed to [[freechips.rocketchip.util.Code]].
  * @param itimAddr optional base ITIM address,
  *                 if None, ITIM won't be generated,
  *                 if Some, ITIM will be generated, with itimAddr as ITIM base address.
  * @param prefetch if set, will send next-line[[TLEdgeOut.Hint]] to manger.
  * @param blockBytes size of a cacheline, calculates in byte.
  * @param latency latency of a instruction fetch, 1 or 2 are available
  * @param fetchBytes byte size fetched by CPU for each cycle.
  */
// case class ICacheParams(
//     nSets: Int = 128, 
//     nWays: Int = 4,
//     rowBits: Int = 128,
//     nTLBSets: Int = 1,
//     nTLBWays: Int = 32,
//     nTLBBasePageSectors: Int = 4,
//     nTLBSuperpages: Int = 4,
//     cacheIdBits: Int = 0,
//     tagECC: Option[String] = None,
//     dataECC: Option[String] = None,
//     itimAddr: Option[BigInt] = None,
//     prefetch: Boolean = false,
//     blockBytes: Int = 64,
//     latency: Int = 2,
//     fetchBytes: Int = 8) extends L1CacheParams {
//   def tagCode: Code = Code.fromString(tagECC)
//   def dataCode: Code = Code.fromString(dataECC)
//   def replacement = new RandomReplacement(nWays)
// }

object ICacheParamsGpc {
  val ICacheParams_gpc = ICacheParams().copy(
    nSets = 128,
    fetchBytes = 8
  )
}

trait HasL1ICacheParameters extends HasL1CacheParameters with HasCoreParameters {
  // val cacheParams = tileParams.icache.get
  val cacheParams = ICacheParamsGpc.ICacheParams_gpc
}

class ICacheReq(implicit p: Parameters) extends CoreBundle()(p) with HasL1ICacheParameters {
  val addr = UInt(vaddrBits.W)
}

class ICacheErrors(implicit p: Parameters) extends CoreBundle()(p)
    with HasL1ICacheParameters
    with CanHaveErrors {
  val correctable = (cacheParams.tagCode.canDetect || cacheParams.dataCode.canDetect).option(Valid(UInt(paddrBits.W)))
  val uncorrectable = (cacheParams.itimAddr.nonEmpty && cacheParams.dataCode.canDetect).option(Valid(UInt(paddrBits.W)))
  val bus = Valid(UInt(paddrBits.W))
}

/** [[ICache]] is a set associated cache I$(Instruction Cache) of Rocket.
 * {{{
  * Keywords: Set-associated
  *           3 stage pipeline
  *           Virtually-Indexed Physically-Tagged (VIPT)
  *           Parallel access to tag and data SRAM
  *           Random replacement algorithm
  * Optional Features:
  *           ECC}}}
  *{{{
  * PipeLine:
  *   Stage 0 : access data and tag SRAM in parallel
  *   Stage 1 : receive paddr from CPU
  *             compare tag and paddr when the entry is valid
  *             if hit : pick up the target instruction
  *             if miss : start refilling in stage 2
  *   Stage 2 : respond to CPU or start a refill}}}
  *{{{
  * Note: Page size = 4KB thus paddr[11:0] = vaddr[11:0]
  *       considering sets = 128, cachelineBytes =64
  *       use vaddr[12:6] to access tag_array
  *       use vaddr[12:2] to access data_array}}}
  *{{{
  * @param icacheParams parameter to this I$.
  * @param staticIdForMetadataUseOnly metadata used for hart id.
  */
class ICache(val icacheParams: ICacheParams, val staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends LazyModule {
  lazy val module = new ICacheModule(this)

  /** Rocket configuration has virtual memory.
    *
    * This only affect [[masterNode]] AMBA ports only:
    * AMBA privileged, secure will be set as true while others set as false.
    * see [[freechips.rocketchip.amba.AMBAProt]] for more informations.
    */
  val useVM = p(TileKey).core.useVM

  /** [[TLClientNode]] of I$.
    *
    * source Id range:
    * 0: use [[TLEdgeOut.Get]] to get instruction.
    * 1: use [[TLEdgeOut.Hint]] to hint next level memory device fetching next cache line, if configured [[icacheParams.prefetch]].
    *
    * @todo why if no [[useVM]], will have AMBAProtField in requestFields?
    */
  val masterNode = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      sourceId = IdRange(0, 1 + icacheParams.prefetch.toInt), // 0=refill, 1=hint
      name = s"Core ${staticIdForMetadataUseOnly} ICache")),
    requestFields = useVM.option(Seq()).getOrElse(Seq(AMBAProtField())))))

  /** size of [[ICache]], count in byte. */
  val size = icacheParams.nSets * icacheParams.nWays * icacheParams.blockBytes
  /** @todo why [[wordBytes]] is defined by [[icacheParams.fetchBytes]], rather than 32 directly? */
  private val wordBytes = icacheParams.fetchBytes
}

class ICacheResp(outer: ICache) extends Bundle {
  /** data to CPU. */
  val data = UInt((outer.icacheParams.fetchBytes*8).W)
  /** ask CPU to replay fetch when tag or data ECC error happened. */
  val replay = Bool()
  /** access exception:
    * indicate CPU an tag ECC error happened.
    * if [[outer.icacheParams.latency]] is 1, tie 0.
    */
  val ae = Bool()

}

class ICachePerfEvents extends Bundle {
  val acquire = Bool()
}

/** IO from CPU to ICache. */
class ICacheBundle(val outer: ICache) extends CoreBundle()(outer.p) {
  /** first cycle requested from CPU. */
  val req = Flipped(Decoupled(new ICacheReq))
  val s1_paddr = Input(UInt(paddrBits.W)) // delayed one cycle w.r.t. req
  val s2_vaddr = Input(UInt(vaddrBits.W)) // delayed two cycles w.r.t. req
  val s1_kill = Input(Bool()) // delayed one cycle w.r.t. req
  val s2_kill = Input(Bool()) // delayed two cycles; prevents I$ miss emission
  val s2_cacheable = Input(Bool()) // should L2 cache line on a miss?
  val s2_prefetch = Input(Bool()) // should I$ prefetch next line on a miss?
  /** response to CPU. */
  val resp = Valid(new ICacheResp(outer))

  /** flush L1 cache from CPU.
    * TODO: IIRC, SFENCE.I
    */
  val invalidate = Input(Bool())

  /** I$ has error, notify to bus.
    * TODO: send to BPU.
    */
  val errors = new ICacheErrors

  /** for performance counting. */
  val perf = Output(new ICachePerfEvents())

  /** enable clock. */
  val clock_enabled = Input(Bool())

  /** I$ miss or ITIM access will still enable clock even [[ICache]] is asked to be gated. */
  val keep_clock_enabled = Output(Bool())
}

class ICacheModule(outer: ICache) extends LazyModuleImp(outer)
    with HasL1ICacheParameters {
  override val cacheParams = outer.icacheParams // Use the local parameters

  /** IO between Core and ICache. */
  val io = IO(new ICacheBundle(outer))

  /** TileLink port to memory. */
  val (tl_out, edge_out) = outer.masterNode.out(0)

  val tECC = cacheParams.tagCode
  val dECC = cacheParams.dataCode

  require(isPow2(nSets) && isPow2(nWays))

  /** valid signal for CPU accessing cache in stage 0. */
  val s0_valid = io.req.fire
  /** virtual address from CPU in stage 0. */
  val s0_vaddr = io.req.bits.addr
  /** valid signal for stage 1, drived by s0_valid.*/
  val s1_valid = RegInit(false.B)
  /** virtual address from CPU in stage 1. */
  val s1_vaddr = RegEnable(s0_vaddr, s0_valid)
  /** tag hit vector to indicate hit which way. */
  val s1_tag_hit = Wire(Vec(nWays, Bool()))
  /** CPU I$ Hit in stage 1.*/
  val s1_hit = s1_tag_hit.reduce(_||_)
  val s2_valid = RegNext(s1_valid && !io.s1_kill, false.B)
  val s2_hit = RegNext(s1_hit)

  /** status register to indicate a cache flush. */
  val invalidated = Reg(Bool())
  val refill_valid = RegInit(false.B)
  /** register to indicate [[tl_out]] is performing a hint.
   *  prefetch only happens after refilling
   * */
  val send_hint = RegInit(false.B)
  /** indicate [[tl_out]] is performing a refill. */
  val refill_fire = tl_out.a.fire && !send_hint
  /** register to indicate there is a outstanding hint. */
  val hint_outstanding = RegInit(false.B)
  /** [[io]] access L1 I$ miss. */
  val s2_miss = s2_valid && !s2_hit && !io.s2_kill
  /** forward signal to stage 1, permit stage 1 refill. */
  val s1_can_request_refill = !(s2_miss || refill_valid)
  /** real refill signal, stage 2 miss, and was permit to refill in stage 1.
    * Since a miss will trigger burst.
    * miss under miss won't trigger another burst.
    */
  val s2_request_refill = s2_miss && RegNext(s1_can_request_refill)
  val refill_paddr = RegEnable(io.s1_paddr, s1_valid && s1_can_request_refill)
  val refill_vaddr = RegEnable(s1_vaddr, s1_valid && s1_can_request_refill)
  val refill_tag = refill_paddr >> pgUntagBits
  val refill_idx = index(refill_vaddr, refill_paddr)
  /** AccessAckData, is refilling I$, it will block request from CPU. */
  val refill_one_beat = tl_out.d.fire && edge_out.hasData(tl_out.d.bits)

  /** block request from CPU when refill or scratch pad access. */
  io.req.ready := !(refill_one_beat)
  s1_valid := s0_valid

  val (_, _, d_done, refill_cnt) = edge_out.count(tl_out.d)
  /** at last beat of `tl_out.d.fire`, finish refill. */
  val refill_done = refill_one_beat && d_done
  tl_out.d.ready := true.B
  require (edge_out.manager.minLatency > 0)

  /** way to be replaced, implemented with a hardcoded random replacement algorithm */
  val repl_way = if (isDM) 0.U else {
    // pick a way that is not used by the scratchpad
    LFSR(16, refill_fire)(log2Up(nWays)-1,0)
  }

/**  Tag SRAM, indexed with virtual memory,
 *   content with `refillError ## tag[19:0]` after ECC
 * */
  val tag_array  = DescribedSRAM(
    name = "tag_array",
    desc = "ICache Tag Array",
    size = nSets,
    data = Vec(nWays, UInt(tECC.width(1 + tagBits).W))
  )
  val tag_rdata = tag_array.read(s0_vaddr(untagBits-1,blockOffBits), !refill_done && s0_valid)
  /** register indicates the ongoing GetAckData transaction is corrupted. */
  val accruedRefillError = Reg(Bool())
  /** wire indicates the ongoing GetAckData transaction is corrupted. */
  val refillError = tl_out.d.bits.corrupt || (refill_cnt > 0.U && accruedRefillError)
  when (refill_done) {
    // For AccessAckData, denied => corrupt
    /** data written to [[tag_array]].
     *  ECC encoded `refillError ## refill_tag`*/
    val enc_tag = tECC.encode(Cat(refillError, refill_tag))
    tag_array.write(refill_idx, VecInit(Seq.fill(nWays){enc_tag}), Seq.tabulate(nWays)(repl_way === _.U))

    ccover(refillError, "D_CORRUPT", "I$ D-channel corrupt")
  }
  // notify CPU, I$ has corrupt.
  io.errors.bus.valid := tl_out.d.fire && (tl_out.d.bits.denied || tl_out.d.bits.corrupt)
  io.errors.bus.bits  := (refill_paddr >> blockOffBits) << blockOffBits

  /** true indicate this cacheline is valid,
    * indexed by (wayIndex ## setIndex)
    * after refill_done and not FENCE.I, (repl_way ## refill_idx) set to true.
    */
  val vb_array = RegInit(0.U((nSets*nWays).W))
  when (refill_one_beat) {
    accruedRefillError := refillError
    // clear bit when refill starts so hit-under-miss doesn't fetch bad data
    vb_array := vb_array.bitSet(Cat(repl_way, refill_idx), refill_done && !invalidated)
  }

  /** flush cache when invalidate is true. */
  val invalidate = WireDefault(io.invalidate)
  when (invalidate) {
    vb_array := 0.U
    invalidated := true.B
  }

  /** wire indicates that tag is correctable or uncorrectable.
    * will trigger CPU to replay and I$ invalidating, if correctable.
    */
  val s1_tag_disparity = Wire(Vec(nWays, Bool()))
  /** wire indicates that bus has an uncorrectable error.
    * respond to CPU [[io.resp.bits.ae]], cause [[Causes.fetch_access]].
    */
  val s1_tl_error = Wire(Vec(nWays, Bool()))
  /** how many bits will be fetched by CPU for each fetch. */
  val wordBits = outer.icacheParams.fetchBytes*8
  /** a set of raw data read from [[data_arrays]]. */
  val s1_dout = Wire(Vec(nWays, UInt(dECC.width(wordBits).W)))
  s1_dout := DontCare

  for (i <- 0 until nWays) {
    val s1_idx = index(s1_vaddr, io.s1_paddr)
    val s1_tag = io.s1_paddr >> pgUntagBits
    val s1_vb = vb_array(Cat(i.U, s1_idx)) 
    val enc_tag = tECC.decode(tag_rdata(i))
    /** [[tl_error]] ECC error bit.
      * [[tag]] of [[tag_array]] access.
      */
    val (tl_error, tag) = Split(enc_tag.uncorrected, tagBits)
    val tagMatch = s1_vb && tag === s1_tag
    /** tag error happens. */
    s1_tag_disparity(i) := s1_vb && enc_tag.error
    /** if tag matched but ecc checking failed, this access will trigger [[Causes.fetch_access]] exception.*/
    s1_tl_error(i) := tagMatch && tl_error.asBool
    s1_tag_hit(i) := tagMatch 
  }
  assert(!(s1_valid) || PopCount(s1_tag_hit zip s1_tag_disparity map { case (h, d) => h && !d }) <= 1.U)

  require(tl_out.d.bits.data.getWidth % wordBits == 0)

  /** Data SRAM
    *
    * banked with TileLink beat bytes / CPU fetch bytes,
    * indexed with [[index]] and multi-beats cycle,
    * content with `eccError ## wordBits` after ECC.
    * {{{
    * │                          │xx│xxxxxx│xxx│x│xx│
    *                                            ↑word
    *                                          ↑bank
    *                            ↑way
    *                               └─set──┴─offset─┘
    *                               └────row───┘
    *}}}
    * Note:
    *  Data SRAM is indexed with virtual memory(vaddr[11:2]),
    *  - vaddr[11:3]->row,
    *  - vaddr[2]->bank=i
    *  - Cache line size = refillCycels(8) * bank(2) * datasize(4 bytes) = 64 bytes
    *  - data width = 32
    *
    *  read:
    *      read happens in stage 0
    *
    *  write:
    *    It takes 8 beats to refill 16 instruction in each refilling cycle.
    *    Data_array receives data[63:0](2 instructions) at once,they will be allocated in deferent bank according to vaddr[2]
    */
  val data_arrays = Seq.tabulate(tl_out.d.bits.data.getWidth / wordBits) {
    i =>
      DescribedSRAM(
        name = s"data_arrays_${i}",
        desc = "ICache Data Array",
        size = nSets * refillCycles,
        data = Vec(nWays, UInt(dECC.width(wordBits).W))
      )
  }

  for ((data_array , i) <- data_arrays.zipWithIndex) {
    /**  bank match (vaddr[2]) */
    def wordMatch(addr: UInt) = addr.extract(log2Ceil(tl_out.d.bits.data.getWidth/8)-1, log2Ceil(wordBits/8)) === i.U
    def row(addr: UInt) = addr(untagBits-1, blockOffBits-log2Ceil(refillCycles))
    /** read_enable signal*/
    val s0_ren = s0_valid && wordMatch(s0_vaddr)
    /** write_enable signal */
    val wen = (refill_one_beat && !invalidated)
    /** index to access [[data_array]]. */
    val mem_idx =
      // I$ refill. refill_idx[2:0] is the beats
      Mux(refill_one_beat, (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
      // CPU read.
                  row(s0_vaddr))
    when (wen) {
      //wr_data
      val data =  tl_out.d.bits.data(wordBits*(i+1)-1, wordBits*i)
      //the way to be replaced/written
      val way = repl_way
      data_array.write(mem_idx, VecInit(Seq.fill(nWays){dECC.encode(data)}), (0 until nWays).map(way === _.U))
    }
    // write access
    /** data read from [[data_array]]. */
    val dout = data_array.read(mem_idx, !wen && s0_ren)
    // Mux to select a way to [[s1_dout]]
    when (wordMatch( io.s1_paddr)) {
      s1_dout := dout
    }
  }

  /** clock gate signal for [[s2_tag_hit]], [[s2_dout]], [[s2_tag_disparity]], [[s2_tl_error]]. */
  val s1_clk_en = s1_valid 
  val s2_tag_hit = RegEnable(s1_tag_hit, s1_clk_en)
  /** way index to access [[data_arrays]]. */
  val s2_hit_way = OHToUInt(s2_tag_hit)
  val s2_dout = RegEnable(s1_dout, s1_clk_en)
  val s2_way_mux = Mux1H(s2_tag_hit, s2_dout)
  val s2_tag_disparity = RegEnable(s1_tag_disparity, s1_clk_en).asUInt.orR
  val s2_tl_error = RegEnable(s1_tl_error.asUInt.orR, s1_clk_en)
  /** ECC decode result for [[data_arrays]]. */
  val s2_data_decoded = dECC.decode(s2_way_mux)
  /** ECC error happened, correctable or uncorrectable, ask CPU to replay. */
  val s2_disparity = s2_tag_disparity || s2_data_decoded.error
  require(outer.icacheParams.latency == 2)
  // output signals
      // when some sort of memory bit error have occurred
      // @todo why so aggressive to invalidate all when ecc corrupted.
      when (s2_valid && s2_disparity) { invalidate := true.B }

      // reply data to CPU at stage 2.
      io.resp.bits.data := s2_data_decoded.uncorrected
      io.resp.bits.ae := s2_tl_error
      io.resp.bits.replay := s2_disparity
      io.resp.valid := s2_valid && s2_hit

  tl_out.a.valid := s2_request_refill
  tl_out.a.bits := edge_out.Get(
                    fromSource = 0.U,
                    toAddress = (refill_paddr >> blockOffBits) << blockOffBits,
                    lgSize = lgCacheBlockBytes.U)._2

  // prefetch when next-line access does not cross a page
  if (cacheParams.prefetch) {
    /** [[crosses_page]]  indicate if there is a crosses page access
      * [[next_block]] : the address to be prefetched.
      */
    val (crosses_page, next_block) = Split(refill_paddr(pgIdxBits-1, blockOffBits) +& 1.U, pgIdxBits-blockOffBits)

    when (tl_out.a.fire) {
      send_hint := !hint_outstanding && io.s2_prefetch && !crosses_page
      when (send_hint) {
        send_hint := false.B
        hint_outstanding := true.B
      }
    }

    // @todo why refill_done will kill hint at this cycle?
    when (refill_done) {
      send_hint := false.B
    }

    // D channel reply with HintAck.
    when (tl_out.d.fire && !refill_one_beat) {
      hint_outstanding := false.B
    }

    when (send_hint) {
      tl_out.a.valid := true.B
      tl_out.a.bits := edge_out.Hint(
                        fromSource = 1.U,
                        toAddress = Cat(refill_paddr >> pgIdxBits, next_block) << blockOffBits,
                        lgSize = lgCacheBlockBytes.U,
                        param = TLHints.PREFETCH_READ)._2
    }

    ccover(send_hint && !tl_out.a.ready, "PREFETCH_A_STALL", "I$ prefetch blocked by A-channel")
    ccover(refill_valid && (tl_out.d.fire && !refill_one_beat), "PREFETCH_D_BEFORE_MISS_D", "I$ prefetch resolves before miss")
    ccover(!refill_valid && (tl_out.d.fire && !refill_one_beat), "PREFETCH_D_AFTER_MISS_D", "I$ prefetch resolves after miss")
    ccover(tl_out.a.fire && hint_outstanding, "PREFETCH_D_AFTER_MISS_A", "I$ prefetch resolves after second miss")
  }
  // Drive APROT information
  tl_out.a.bits.user.lift(AMBAProt).foreach { x =>
    // Rocket caches all fetch requests, and it's difficult to differentiate privileged/unprivileged on
    // cached data, so mark as privileged
    x.fetch       := true.B
    x.secure      := true.B
    x.privileged  := true.B
    x.bufferable  := true.B
    x.modifiable  := true.B
    x.readalloc   := io.s2_cacheable
    x.writealloc  := io.s2_cacheable
  }
  tl_out.b.ready := true.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B

  // if there is an outstanding refill, cannot flush I$.
  when (!refill_valid) { invalidated := false.B }
  when (refill_fire) { refill_valid := true.B }
  when (refill_done) { refill_valid := false.B}

  io.perf.acquire := refill_fire
  // don't gate I$ clock since there are outstanding transcations.
  io.keep_clock_enabled :=
    s1_valid || s2_valid || refill_valid || send_hint || hint_outstanding // I$

  /** index to access [[data_arrays]] and [[tag_array]].
    * @note
    * if [[untagBits]] > [[pgIdxBits]] in
    * {{{
    *                        ┌──idxBits──┐
    *                        ↓           ↓
    * │          tag         │    set    │offset│
    * │              pageTag     │     pageIndex│
    *                        ↑   ↑       ↑      │
    *                   untagBits│  blockOffBits│
    *                       pgIdxBits    │
    *                        └msb┴──lsb──┘
    *                        vaddr paddr
    * }}}
    *
    * else use paddr directly.
    * Note: if [[untagBits]] > [[pgIdxBits]], there will be a alias issue which isn't addressend by the icache yet.
    */
  def index(vaddr: UInt, paddr: UInt) = {
    /** [[paddr]] as LSB to be used for VIPT. */
    val lsbs = paddr(pgUntagBits-1, blockOffBits)
    /** if [[untagBits]] > [[pgIdxBits]], append [[vaddr]] to higher bits of index as [[msbs]]. */
    val msbs = (idxBits+blockOffBits > pgUntagBits).option(vaddr(idxBits+blockOffBits-1, pgUntagBits))
    msbs ## lsbs
  }

  ccover(!send_hint && (tl_out.a.valid && !tl_out.a.ready), "MISS_A_STALL", "I$ miss blocked by A-channel")
  ccover(invalidate && refill_valid, "FLUSH_DURING_MISS", "I$ flushed during miss")

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    property.cover(cond, s"ICACHE_$label", "MemorySystem;;" + desc)

  val mem_active_valid = Seq(property.CoverBoolean(s2_valid, Seq("mem_active")))
  val data_error = Seq(
    property.CoverBoolean(!s2_data_decoded.correctable, Seq("no_data_error")),
    property.CoverBoolean(s2_data_decoded.correctable, Seq("data_correctable_error")),
  )
  val tag_error = Seq(
    property.CoverBoolean(!s2_tag_disparity, Seq("no_tag_error")),
    property.CoverBoolean(s2_tag_disparity, Seq("tag_error"))
  )

  val error_cross_covers = new property.CrossProperty(
    Seq(mem_active_valid, data_error, tag_error),
    Seq(
      // tag error cannot occur in ITIM mode
      Seq("tag_error", "ITIM_mode"),
      // Can only respond to TL in ITIM mode
      Seq("from_TL", "cache_mode")
    ),
    "MemorySystem;;Memory Bit Flip Cross Covers")

  property.cover(error_cross_covers)
}

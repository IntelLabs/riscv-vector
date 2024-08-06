package grapecoveDCache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import utility._
import AddrDecoder._
import MemoryOpConstants._

class GPCDCache()(
    implicit p: Parameters
) extends BaseDCache {
  override lazy val module = new GPCDCacheImp(this)
}

class GPCDCacheImp(outer: BaseDCache) extends BaseDCacheImp(outer) {

  dontTouch(io)

  // * Modules Begin
  def onReset     = Metadata(0.U, ClientMetadata.onReset.state)
  val metaArray   = Module(new MetaArray[Metadata](() => onReset))
  val dataArray   = Module(new DataArray)
  val amoalu      = Module(new AMOALU(XLEN))
  val mshrs       = Module(new MSHRWrapper)
  val wbQueue     = Module(new WritebackQueue)
  val probeQueue  = Module(new ProbeQueue)
  val refillQueue = Module(new RefillQueueWrapper)
  // * Modules End

  // * Signal Define Begin
  // Store -> Load Bypassing
  class BypassStore extends Bundle {
    val data  = UInt(dataWidth.W)
    val tag   = UInt(tagBits.W)
    val coh   = UInt(cohBits.W)
    val wayEn = UInt(nWays.W)
  }
  // Define a function to create and configure a BypassStore
  def createBypassStore(data: UInt, tag: UInt, coh: UInt, wayEn: UInt): BypassStore = {
    val bypassStoreCandidate = Wire(new BypassStore)
    bypassStoreCandidate.data  := data
    bypassStoreCandidate.tag   := tag
    bypassStoreCandidate.coh   := coh
    bypassStoreCandidate.wayEn := wayEn
    bypassStoreCandidate
  }

  val s1_bypassStore        = Reg(Valid(new BypassStore))
  val s1_bypassReplaceValid = RegInit(false.B)

  // replace victim way
  val victimWay = WireInit(0.U(log2Up(nWays).W))

  val wbPipeReq = WireInit(0.U.asTypeOf(Decoupled(new WritebackReq(edge.bundle))))

  // * Signal Define End

  // * pipeline stage 0 Begin
  // req arbiter
  val mainReqArb = Module(new Arbiter(new MainPipeReq, 3))

  // TODO: priority
  // source 0: probe
  mainReqArb.io.in(0) <> probeQueue.io.mainPipeReq
  // source 1: replace
  mainReqArb.io.in(1).valid := mshrs.io.toReplace.valid
  mainReqArb.io.in(1).bits  := MainPipeReqConverter(mshrs.io.toReplace.bits, victimWay)
  mshrs.io.toReplace.ready  := mainReqArb.io.in(1).ready
  // source 2: req
  mainReqArb.io.in(2).valid := io.req.valid
  mainReqArb.io.in(2).bits  := MainPipeReqConverter(io.req.bits)
  io.req.ready              := mainReqArb.io.in(2).ready

  // FIXME: store ready ?
  mainReqArb.io.out.ready := Mux(
    mainReqArb.io.out.bits.isFromCore,
    !(io.nextCycleWb || (io.resp.valid && io.resp.bits.status === CacheRespStatus.replay)),
    true.B,
  )

  // get s0 req
  val s0_req   = mainReqArb.io.out.bits
  val s0_valid = mainReqArb.io.out.fire & ~(io.s0_kill && s0_req.isFromCore)
  val s0_cacheable = edge.manager.supportsAcquireBFast( // determine if the request is cacheable or not
    s0_req.paddr,
    log2Up(blockBytes).U,
  )

  // read tag array
  metaArray.io.read.valid       := s0_valid
  metaArray.io.read.bits.setIdx := getSetIdx(s0_req.paddr)
  metaArray.io.read.bits.wayEn  := Mux(s0_req.isRefill, UIntToOH(s0_req.refillWay), Fill(nWays, true.B))

  // read data array
  dataArray.io.read.valid       := s0_valid
  dataArray.io.read.bits.setIdx := getSetIdx(s0_req.paddr)
  dataArray.io.read.bits.bankEn := UIntToOH(getBankIdx(s0_req.paddr)) // useless now
  dataArray.io.read.bits.wayEn  := Mux(s0_req.isRefill, UIntToOH(s0_req.refillWay), Fill(nWays, true.B))
  // * pipeline stage 0 End

  // * pipeline stage 1 Begin

  val s1_req           = RegEnable(s0_req, s0_valid)
  val s1_valid         = RegNext(s0_valid) & ~(io.s1_kill && s1_req.isFromCore)
  val s1_cacheable     = RegEnable(s0_cacheable, s0_valid)
  val s1_validFromCore = s1_valid && s1_req.isFromCore
  val s1_validProbe    = s1_valid && s1_req.isProbe
  val s1_validRefill   = s1_valid && s1_req.isRefill

  assert(!s1_validProbe || (s1_validProbe && s1_cacheable))
  assert(!s1_validRefill || (s1_validRefill && s1_cacheable))

  // meta & data resp
  val s1_dataArrayResp = dataArray.io.resp // nways nbanks data
  val s1_metaArrayResp = metaArray.io.resp // nways meta

  // obtain tag & coh
  val s1_tagMatchWayPreBypassVec = VecInit(
    (0 until nWays).map(w =>
      s1_metaArrayResp(w).tag === getTag(s1_req.paddr) && s1_metaArrayResp(w).coh > 0.U
    )
  )

  val s1_tagMatchWayVec = MuxCase(
    s1_tagMatchWayPreBypassVec,
    Seq(
      s1_bypassReplaceValid
        -> VecInit(Seq.fill(nWays)(false.B)),
      s1_bypassStore.valid
        -> VecInit((0 until nWays).map(w =>
          s1_bypassStore.bits.wayEn(w) && s1_bypassStore.bits.coh > 0.U
        )),
    ),
  )

  val s1_isTagMatch = s1_tagMatchWayVec.asUInt.orR

  val s1_metaPreBypass = Mux(
    s1_req.isRefill,
    s1_metaArrayResp(s1_req.refillWay),
    Mux1H(s1_tagMatchWayPreBypassVec, s1_metaArrayResp),
  )

  val s1_meta = MuxCase(
    s1_metaPreBypass,
    Seq(
      s1_bypassReplaceValid -> Metadata(0.U, 0.U),
      s1_bypassStore.valid  -> Metadata(s1_bypassStore.bits.tag, s1_bypassStore.bits.coh),
    ),
  )

  val s1_tag = s1_meta.tag
  val s1_coh = ClientMetadata(s1_meta.coh) // obtain coh for load/store/probe/replace

  // obtain data
  val s1_dataPreBypass = Mux(
    s1_req.isRefill,
    s1_dataArrayResp(s1_req.refillWay),                 // get replace data
    Mux1H(s1_tagMatchWayPreBypassVec, s1_dataArrayResp),// select hit way data
  ).asUInt

  val s1_data = Mux(s1_bypassStore.valid, s1_bypassStore.bits.data, s1_dataPreBypass)

  // * load/store
  val s1_hasPerm         = s1_coh.onAccess(s1_req.cmd)._1
  val s1_newHitCoh       = s1_coh.onAccess(s1_req.cmd)._3
  val s1_hit             = s1_isTagMatch && s1_hasPerm
  val s1_upgradePermHit  = s1_hit && s1_newHitCoh =/= s1_coh // e.g. T->Dirty hit
  val s1_noDataMiss      = !s1_isTagMatch                    // e.g. N->B or N->T miss
  val s1_upgradePermMiss = s1_isTagMatch && !s1_hasPerm      // e.g. B->T miss

  // organize load data
  val loadGen = new LoadGen(s1_req.size, s1_req.signed, s1_req.paddr, s1_data, false.B, dataBytes)

  // organize store data
  val s1_storeGenMask = new StoreGen(s1_req.size, s1_req.paddr, 0.U, dataBytes).mask
  val s1_maskInBytes = Mux(
    s1_req.isRefill,
    Fill(dataBytes, 1.U),   // refill don't need mask
    Mux(                    //
      s1_req.cmd === M_PWR, // condition: is partial write?
      s1_req.wmask,         // partial store mask
      s1_storeGenMask,      // store gen mask
    ),
  )
  val s1_mask           = FillInterleaved(8, s1_maskInBytes)
  val s1_mergeStoreData = s1_req.wdata & s1_mask | s1_data & ~s1_mask

  assert(
    !(s1_validFromCore && s1_req.cmd === M_PWR && s1_req.size =/= 6.U),
    "Only support partial write 64 bytes",
  )
  // TODO: PWR assertion
  // TODO: Merge Store & AMO Store

  // * cpu amo store data
  // NOTE: operate in 8Bytes
  val s1_blockOffset  = getBlockOffset(s1_req.paddr)
  val s1_amoStoreData = (amoalu.io.out << (s1_blockOffset << 3) & s1_mask) | s1_data & ~s1_mask

  amoalu.io.mask := new StoreGen(s1_req.size, s1_req.paddr, 0.U, XLEN).mask
  amoalu.io.cmd  := s1_req.cmd
  amoalu.io.lhs  := (s1_data >> (s1_blockOffset << 3))(XLEN - 1, 0)
  amoalu.io.rhs  := s1_req.wdata

  // * cpu lrsc
  // NOTE: when lrscValid -> block probe & replace to lrscAddr
  val lrscCount = RegInit(0.U)
  val lrscValid = lrscCount > lrscBackoff.U // FIXME lrscBackOff  probe interval

  val s1_lr = s1_validFromCore && (s1_req.cmd === M_XLR)
  val s1_sc = s1_validFromCore && (s1_req.cmd === M_XSC)

  val lrscAddr         = RegEnable(s1_req.paddr >> blockOffBits, s1_lr)
  val s1_lrscAddrMatch = lrscValid && (s1_req.paddr >> blockOffBits === lrscAddr)
  val s1_lrFail        = s1_lr && !s1_hit
  val s1_scFail        = s1_sc && (!s1_hit || !s1_lrscAddrMatch)

  lrscCount := MuxCase(
    lrscCount,
    Seq(
      // (lr | sc | other cmd) after lr hit
      (s1_validFromCore & (lrscCount > 0.U)) -> 0.U,
      // lr hit
      (s1_hit && s1_lr) -> (lrscCycles - 1).U,
      // no cmd after lr hit
      (lrscCount > 0.U) -> (lrscCount - 1.U),
    ),
  )

  val s1_storeUpdateMeta = s1_validFromCore && s1_cacheable && !s1_scFail &&
    (s1_upgradePermHit || (s1_upgradePermMiss && mshrs.io.req.ready))
  val s1_storeUpdateData = s1_validFromCore && s1_cacheable && s1_hit &&
    isWrite(s1_req.cmd) && !s1_scFail

  // TODO: flush & flush all

  // * probe
  // FIXME
  val s1_canProbe = wbPipeReq.ready // writeback queue ready
  val s1_probeCoh = Mux(s1_isTagMatch, s1_coh, ClientMetadata(0.U))

  val s1_probeDirty       = s1_probeCoh.onProbe(s1_req.probePerm)._1
  val s1_probeReportParam = s1_probeCoh.onProbe(s1_req.probePerm)._2
  val s1_newProbeCoh      = s1_probeCoh.onProbe(s1_req.probePerm)._3

  val s1_probeUpdateMeta = s1_validProbe && s1_isTagMatch && (s1_probeCoh =/= s1_newProbeCoh)
  val s1_probeWb         = s1_validProbe                  // probe should send probeAck
  val s1_probeWbData     = s1_isTagMatch && s1_probeDirty // probe has data

  // * replace
  val s1_repLineAddr    = Cat(s1_tag, getSetIdx(s1_req.paddr))
  val s1_repCoh         = s1_coh
  val s1_repDirty       = s1_repCoh.onCacheControl(M_FLUSH)._1
  val s1_repShrinkParam = s1_repCoh.onCacheControl(M_FLUSH)._2
  val s1_replaceWb      = s1_validRefill && (s1_repCoh.state > ClientStates.Branch) // Trunk or Dirty
  val s1_replaceWbData  = s1_repDirty

  val s1_refillUpdateMeta = s1_validRefill
  val s1_refillUpdateData = s1_validRefill
  val s1_refillData       = s1_req.wdata

  val s1_canDoRefill = (!s1_replaceWb) || wbPipeReq.ready

  // prepare for s2
  val s1_updateMeta = s1_storeUpdateMeta ||
    (s1_probeUpdateMeta && s1_canProbe) ||
    (s1_refillUpdateMeta && s1_canDoRefill)
  val s1_updateData = s1_storeUpdateData ||
    (s1_refillUpdateData && s1_canDoRefill)

  // FIXME mask
  // data array store data
  val s1_storeData = Mux(
    s1_req.isRefill,
    s1_req.wdata,
    Mux(isAMO(s1_req.cmd), s1_amoStoreData, s1_mergeStoreData),
  )

  val s1_newCoh = MuxCase(
    0.U,
    Seq(
      s1_validProbe      -> s1_newProbeCoh.state,
      s1_validRefill     -> s1_req.refillCoh,
      s1_hit             -> s1_newHitCoh.state,
      s1_upgradePermMiss -> 0.U,// invalid
    ),
  )

  val s1_wayEn = MuxCase(
    s1_tagMatchWayVec,
    Seq(
      s1_validProbe    -> s1_tagMatchWayVec,
      s1_validRefill   -> VecInit(UIntToOH(s1_req.refillWay).asBools),
      s1_validFromCore -> s1_tagMatchWayVec,
    ),
  ).asUInt

  // * pipeline stage 1 End

  // * pipeline stage 2 Begin
  val s2_valid       = RegNext(s1_updateMeta || s1_updateData)
  val s2_req         = RegInit(0.U.asTypeOf(new MainPipeReq))
  val s2_wayEn       = RegInit(0.U(nWays.W))
  val s2_newCoh      = RegInit(0.U(cohBits.W))
  val s2_updateMeta  = RegNext(s1_updateMeta)
  val s2_updateData  = RegNext(s1_updateData)
  val s2_validRefill = RegNext(s1_validRefill)

  when(s1_updateMeta || s1_updateData) {
    s2_req       := s1_req
    s2_newCoh    := s1_newCoh
    s2_wayEn     := s1_wayEn
    s2_req.wdata := s1_storeData
  }

  // meta write
  metaArray.io.write.valid         := s2_updateMeta
  metaArray.io.write.bits.setIdx   := getSetIdx(s2_req.paddr)
  metaArray.io.write.bits.wayEn    := s2_wayEn
  metaArray.io.write.bits.data.tag := getTag(s2_req.paddr)
  metaArray.io.write.bits.data.coh := s2_newCoh

  // data write
  dataArray.io.write.valid       := s2_updateData
  dataArray.io.write.bits.setIdx := getSetIdx(s2_req.paddr)
  dataArray.io.write.bits.bankEn := Fill(nBanks, true.B).asUInt
  dataArray.io.write.bits.wayEn  := s2_wayEn
  dataArray.io.write.bits.mask   := VecInit(Seq.fill(nBanks)(0.U)) // TODO
  dataArray.io.write.bits.data := VecInit(
    (0 until nBanks).map(i =>
      s2_req.wdata((i + 1) * rowBits - 1, i * rowBits)
    )
  )

  // * pipeline stage 2 End

  // * Store -> Load Bypassing Begin

  val s1_bypassStoreCandidate = createBypassStore(s1_storeData, getTag(s1_req.paddr), s1_newCoh, s1_wayEn)
  val s2_bypassStoreCandidate = createBypassStore(s2_req.wdata, getTag(s2_req.paddr), s2_newCoh, s2_wayEn)

  val bypassList = List(
    (s1_updateMeta || s1_updateData, s1_req, s1_bypassStoreCandidate, s1_tag),
    (s2_valid, s2_req, s2_bypassStoreCandidate, RegNext(s1_tag)),
  ).map(r =>
    (
      // bypass store cond: valid & same line addr
      r._1 && getLineAddr(s0_req.paddr) === getLineAddr(r._2.paddr), // FIXME
      // bypass replace cond: valid & prev isRefill & req line addr = replace line addr
      r._1 && r._2.isRefill && (getLineAddr(s0_req.paddr) === Cat(r._4, getSetIdx(r._2.paddr))), // TODO: redundant
      r._3,
    )
  )

  val bypassEntry = PriorityMux(bypassList.map(d => (d._1 || d._2, Cat(d._3.asUInt, d._2, d._1))))
  s1_bypassReplaceValid := bypassEntry(1)
  s1_bypassStore.valid  := bypassEntry(0)
  s1_bypassStore.bits   := (bypassEntry >> 2.U).asTypeOf(new BypassStore)

  // * Store -> Load Bypassing End

  // * Replacer Begin

  val replacer = ReplacementPolicy.fromString(replacementPolicy, nWays, nSets)

  // pipeline stage 1 -> hit update replacer
  val s1_updateReplacer = (s1_validFromCore && s1_hit) ||
    (s1_validRefill && s1_canDoRefill)

  val touchWay = OHToUInt(s1_wayEn)
  val touchSet = getSetIdx(s1_req.paddr)

  when(s1_updateReplacer) {
    replacer.access(touchSet, touchWay)
  }

  // get victim way
  val replSet = getSetIdx(mshrs.io.toReplace.bits.lineAddr << blockOffBits)
  victimWay := replacer.way(replSet) // FIXME

  // * Replacer End

  // * MSHR Begin

  dontTouch(mshrs.io)

  // pipeline miss -> mshr
  val s1_mshrAlloc     = s1_validFromCore && ~s1_hit && ~s1_scFail
  val s1_mshrAllocFail = s1_mshrAlloc && !mshrs.io.req.ready

  // mshr store data
  val s1_mshrStoreData        = Mux(s1_upgradePermMiss, s1_mergeStoreData, s1_req.wdata)
  val s1_mshrStoreMaskInBytes = Mux(s1_upgradePermMiss, Fill(dataBytes, 1.U), s1_maskInBytes)

  val mshrReq = WireDefault(s1_req)
  mshrReq.wdata := s1_mshrStoreData
  mshrReq.wmask := s1_mshrStoreMaskInBytes

  mshrs.io.req.valid := s1_mshrAlloc
  mshrs.io.req.bits  := mshrReq
  mshrs.io.isUpgrade := s1_upgradePermMiss
  mshrs.io.cacheable := s1_cacheable

  // mshr acquire block or perm from L2
  tlBus.a <> mshrs.io.l2Req

  // refillQueue -> mshr
  mshrs.io.fromRefill <> refillQueue.io.refillResp
  mshrs.io.probeRefill <> refillQueue.io.probeCheck

  // mshr send replace req to pipeline
  mshrs.io.replaceStatus := Mux(
    !s1_validRefill,
    ReplaceStatus.replace_invalid,
    Mux(
      s1_canDoRefill,
      ReplaceStatus.replace_finish,
      ReplaceStatus.replace_replay,
    ),
  )

  // probe -> mshr
  mshrs.io.probeCheck <> probeQueue.io.probeCheck

  // * MSHR End

  // * Refill Begin

  refillQueue.io.memFinish <> tlBus.e

  // NOTE: set to default; actual connect see tl-d
  refillQueue.io.memGrant.valid := false.B
  refillQueue.io.memGrant.bits  := DontCare

  // * Refill End

  // * Writeback Begin

  // wb in pipeline stage 1
  val wbArbiter = Module(new Arbiter(new WritebackReq(edge.bundle), 2))

  wbPipeReq.valid          := s1_probeWb || s1_replaceWb
  wbPipeReq.bits.voluntary := s1_req.isRefill
  wbPipeReq.bits.data      := s1_data
  wbPipeReq.bits.perm      := Mux(s1_probeWb, s1_probeReportParam, s1_repShrinkParam)
  wbPipeReq.bits.lineAddr  := Mux(s1_probeWb, getLineAddr(s1_req.paddr), s1_repLineAddr)
  wbPipeReq.bits.hasData   := Mux(s1_probeWb, s1_probeWbData, s1_replaceWbData)

  // source 0: main pipeline
  wbArbiter.io.in(0) <> wbPipeReq
  // source 1: probe
  wbArbiter.io.in(1) <> probeQueue.io.wbReq

  // wbq req
  wbQueue.io.req <> wbArbiter.io.out
  // miss check
  wbQueue.io.missCheck.valid    := s1_validFromCore && s1_cacheable && ~s1_hit
  wbQueue.io.missCheck.lineAddr := getLineAddr(s1_req.paddr)
  val s1_wbqBlockMiss = wbQueue.io.missCheck.blockMiss
  // wbq release
  wbQueue.io.release <> tlBus.c
  // wbq grant: set default value; see TL-D connection
  wbQueue.io.grant.valid := false.B
  wbQueue.io.grant.bits  := DontCare

  assert(!(s1_probeWb && s1_replaceWb))

  // * Writeback End

  // * Probe Begin
  probeQueue.io.memProbe <> tlBus.b

  probeQueue.io.lrscAddr.valid := lrscValid
  probeQueue.io.lrscAddr.bits  := lrscAddr

  // in MSHR: probeQueue.io.probeCheck <> mshrs.io.probeCheck

  probeQueue.io.probeResp := Mux(
    !s1_probeWb,
    ProbeRespStatus.probe_invalid,
    Mux(s1_canProbe, ProbeRespStatus.probe_finish, ProbeRespStatus.probe_replay),
  )
  // * Probe End

  // * Resp Begin
  val s1_cacheResp = Wire(Valid(new DataExchangeResp))
  val mshrsResp    = Wire(Valid(new DataExchangeResp))

  //  s1 resp
  s1_cacheResp.valid        := s1_validFromCore && !isPrefetch(s1_req.cmd)
  s1_cacheResp.bits.source  := s1_req.source
  s1_cacheResp.bits.dest    := s1_req.dest
  s1_cacheResp.bits.data    := Mux(s1_sc, s1_scFail, loadGen.data)
  s1_cacheResp.bits.hasData := s1_hit && isRead(s1_req.cmd)
  s1_cacheResp.bits.status := MuxCase(
    CacheRespStatus.miss,
    Seq(
      s1_hit           -> CacheRespStatus.hit,
      s1_wbqBlockMiss  -> CacheRespStatus.replay,
      s1_mshrAllocFail -> CacheRespStatus.replay,
    ),
  )

  // mshr resp
  mshrsResp <> mshrs.io.resp

  // return resp
  io.nextCycleWb := mshrs.io.nextCycleWb
  io.resp.valid  := s1_cacheResp.valid || mshrsResp.valid
  io.resp.bits   := Mux(s1_cacheResp.valid, s1_cacheResp.bits, mshrsResp.bits)

  // when mshr wants to wb,
  // it will send nextCycleWb to kill other cache request
  assert(
    ~(s1_cacheResp.valid && mshrsResp.valid),
    "MSHR & Cache Resp cannot exist simultaneously",
  )

  // * Resp End

  // tilelink d
  tlBus.d.ready := false.B
  when(tlBus.d.bits.opcode === TLMessages.ReleaseAck) {
    tlBus.d <> wbQueue.io.grant
  }.otherwise {
    tlBus.d <> refillQueue.io.memGrant
  }

  dontTouch(tlBus)
}

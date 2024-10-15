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
  val s1_mshrAlloc = WireInit(false.B)
  // replace victim way
  val victimWay = WireInit(0.U(log2Up(nWays).W))

  val wbPipeReq       = WireInit(0.U.asTypeOf(Decoupled(new WritebackReq(edge.bundle))))
  val s1_wbqBlockMiss = wbQueue.io.missCheck.blockMiss

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

  val s1_bypassStore        = Wire(Valid(new BypassStore))
  val s1_bypassReplaceValid = WireInit(false.B)

  // * Signal Define End

  // * pipeline stage 0 Begin
  // req arbiter
  val mainReqArb = Module(new Arbiter(new MainPipeReq(edge.bundle), 3))

  // TODO: priority
  // source 0: probe
  mainReqArb.io.in(0) <> probeQueue.io.mainPipeReq
  // source 1: replace
  mainReqArb.io.in(1).valid := mshrs.io.toReplace.valid
  mainReqArb.io.in(1).bits  := MainPipeReqConverter(mshrs.io.toReplace.bits, edge.bundle)
  mshrs.io.toReplace.ready  := mainReqArb.io.in(1).ready
  // source 2: req
  mainReqArb.io.in(2).valid := io.req.valid
  mainReqArb.io.in(2).bits  := MainPipeReqConverter(io.req.bits, edge.bundle)
  io.req.ready              := mainReqArb.io.in(2).ready

  // FIXME: store ready ?
  mainReqArb.io.out.ready := Mux(
    mainReqArb.io.out.bits.isFromCore,
    !(io.nextCycleWb || (io.resp.valid && io.resp.bits.status === CacheRespStatus.replay)),
    true.B,
  )

  // get s0 req
  val s0_req   = mainReqArb.io.out.bits
  val s0_valid = mainReqArb.io.out.fire
  val s0_cacheable = edge.manager.supportsAcquireBFast( // determine if the request is cacheable or not
    s0_req.paddr,
    log2Up(blockBytes).U,
  )

  assert(!(s0_req.isFromCore && s0_req.size > 6.U), "Request size should <= 6")

  // read tag array
  metaArray.io.read.valid       := s0_valid
  metaArray.io.read.bits.setIdx := getSetIdx(s0_req.paddr)
  metaArray.io.read.bits.wayEn  := Fill(nWays, true.B)

  // read data array
  dataArray.io.read.valid       := s0_valid
  dataArray.io.read.bits.setIdx := getSetIdx(s0_req.paddr)
  dataArray.io.read.bits.bankEn := UIntToOH(getBankIdx(s0_req.paddr)) // useless now
  dataArray.io.read.bits.wayEn  := Fill(nWays, true.B)
  // * pipeline stage 0 End

  // * pipeline stage 1 Begin

  val s1_req           = RegEnable(s0_req, s0_valid)
  val s1_valid         = RegNext(s0_valid) & ~(io.s1_kill && s1_req.isFromCore)
  val s1_cacheable     = RegEnable(s0_cacheable, s0_valid)
  val s1_validFromCore = s1_valid && s1_req.isFromCore
  val s1_validProbe    = s1_valid && s1_req.isProbe
  val s1_validRefill   = s1_valid && s1_req.isRefill
  val s1_refillWay     = victimWay

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
    s1_metaArrayResp(s1_refillWay),
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
  val s1_dataHitWay = Mux1H(s1_tagMatchWayPreBypassVec, s1_dataArrayResp)
  val s1_dataPreBypass = Mux(
    s1_req.isRefill,
    s1_dataArrayResp(s1_refillWay), // get replace data
    s1_dataHitWay,                  // select hit way data
  ).asUInt

  val s1_data = Mux(s1_bypassStore.valid, s1_bypassStore.bits.data, s1_dataPreBypass)

  // * load/store
  val s1_hasPerm         = s1_coh.onAccess(s1_req.cmd)._1
  val s1_newHitCoh       = s1_coh.onAccess(s1_req.cmd)._3
  val s1_hit             = s1_isTagMatch && (s1_hasPerm || isPrefetch(s1_req.cmd))
  val s1_upgradePermHit  = s1_hit && s1_newHitCoh =/= s1_coh                       // e.g. T->Dirty hit
  val s1_noDataMiss      = !s1_isTagMatch                                          // e.g. N->B or N->T miss
  val s1_upgradePermMiss = s1_isTagMatch && !s1_hasPerm && !isPrefetch(s1_req.cmd) // e.g. B->T miss

  val storeGen        = new StoreGen(s1_req.size, s1_req.paddr, s1_req.wdata, dataBytes)
  val s1_newStoreData = storeGen.data
  val s1_newStoreMask = Mux(
    s1_req.cmd === M_PWR,
    s1_req.wmask,
    storeGen.mask,
  )

  assert(
    !(s1_validFromCore && s1_req.cmd === M_PWR && s1_req.size =/= 6.U),
    "Only support partial write 64 bytes",
  )
  // TODO: PWR assertion
  // TODO: Merge Store & AMO Store

  // * amo no perm miss
  val s1_amoNoPermMiss   = s1_validFromCore && isAMO(s1_req.cmd) && s1_upgradePermMiss
  val s1_amoNoPermWb     = s1_amoNoPermMiss
  val s1_amoNoPermWbData = false.B
  val s1_amoShrinkParam  = TLPermissions.BtoN

  // * cpu lrsc
  // NOTE: when lrscValid -> block probe & replace to lrscAddr
  val lrscCount = RegInit(0.U)
  val lrscValid = lrscCount > lrscBackoff.U // FIXME lrscBackOff  probe interval

  val s1_lr = s1_validFromCore && (s1_req.cmd === M_XLR)
  val s1_sc = s1_validFromCore && (s1_req.cmd === M_XSC)

  val lrscAddr         = RegEnable(getLineAddr(s1_req.paddr), s1_lr)
  val s1_lrscAddrMatch = lrscValid && (getLineAddr(s1_req.paddr) === lrscAddr)
  val s1_lrFail        = s1_lr && !s1_hit
  val s1_scFail        = s1_sc && (!s1_hit || !s1_lrscAddrMatch)

  lrscCount := MuxCase(
    lrscCount,
    Seq(
      // lr hit
      (s1_hit && s1_lr) -> (lrscCycles - 1).U,
      // (sc | other cmd) after lr hit
      (s1_validFromCore & (lrscCount > 0.U)) -> 0.U,
      // no cmd after lr hit
      (lrscCount > 0.U) -> (lrscCount - 1.U),
    ),
  )

  val s1_storeHitUpdateMeta = s1_validFromCore && s1_cacheable && s1_upgradePermHit && !s1_scFail
  val s1_storeMissUpdateMeta = s1_validFromCore && s1_cacheable && (
    (s1_amoNoPermMiss && wbPipeReq.ready) ||
      (s1_upgradePermMiss && !isAMO(s1_req.cmd) && mshrs.io.req.ready)
  )

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
  val s1_replaceWb      = s1_validRefill && (s1_repCoh.state > ClientStates.Nothing)
  val s1_replaceWbData  = s1_repDirty

  // * refill
  val s1_refillUpdateMeta = s1_validRefill
  val s1_refillUpdateData = s1_validRefill
  val s1_refillData       = s1_req.wdata

  val s1_canDoRefill = (!s1_replaceWb) || wbPipeReq.ready

  // prepare for s2
  val s1_updateMeta = s1_storeHitUpdateMeta ||
    s1_storeMissUpdateMeta ||
    (s1_probeUpdateMeta && s1_canProbe) ||
    (s1_refillUpdateMeta && s1_canDoRefill)
  val s1_updateData = s1_storeUpdateData ||
    (s1_refillUpdateData && s1_canDoRefill)

  val s1_needUpdate = s1_updateMeta || s1_updateData

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
      s1_validRefill   -> VecInit(UIntToOH(s1_refillWay).asBools),
      s1_validFromCore -> s1_tagMatchWayVec,
    ),
  ).asUInt

  // * pipeline stage 1 End

  // * pipeline stage 2 Begin
  // organize store data
  val s2_valid      = RegNext(s1_needUpdate)
  val s2_wayEn      = RegEnable(s1_wayEn, s1_needUpdate)
  val s2_newCoh     = RegEnable(s1_newCoh, s1_needUpdate)
  val s2_repCoh     = RegEnable(s1_repCoh, s1_needUpdate)
  val s2_updateMeta = RegNext(s1_updateMeta)
  val s2_updateData = RegNext(s1_updateData)
  val s2_tag        = RegEnable(s1_tag, s1_needUpdate)
  val s2_data       = RegEnable(s1_data, s1_needUpdate)
  val s2_req        = RegInit(0.U.asTypeOf(new MainPipeReq(edge.bundle)))
  // need to calculate hit store data & miss store data
  when(s1_needUpdate | (s1_mshrAlloc && isWrite(s1_req.cmd))) {
    s2_req       := s1_req
    s2_req.wdata := s1_newStoreData
    s2_req.wmask := s1_newStoreMask
  }

  // * cpu store data
  val s2_mask           = FillInterleaved(8, s2_req.wmask)
  val s2_mergeStoreData = s2_req.wdata & s2_mask | s2_data & ~s2_mask

  // * cpu amo store data
  // NOTE: operate in 8Bytes
  val s2_bankOffset = getBankIdx(s2_req.paddr)
  val s2_dataVec = VecInit(
    (0 until nBanks).map(i => s2_data((i + 1) * rowBits - 1, i * rowBits))
  )
  val s2_amoStoreDataVec = VecInit(
    (0 until nBanks).map(i => s2_data((i + 1) * rowBits - 1, i * rowBits))
  )
  s2_amoStoreDataVec(s2_bankOffset) := amoalu.io.out

  val s2_amoStoreData = (s2_amoStoreDataVec.asUInt & s2_mask) | s2_data & ~s2_mask

  amoalu.io.mask := new StoreGen(s2_req.size, s2_req.paddr, 0.U, XLEN).mask // XLEN mask
  amoalu.io.cmd  := s2_req.cmd
  amoalu.io.lhs  := s2_dataVec(s2_bankOffset)
  amoalu.io.rhs  := s2_req.wdata

  val s2_storeData = Mux(isAMO(s2_req.cmd), s2_amoStoreData, s2_mergeStoreData)
  // * pipeline stage 2 End

  // * pipeline stage 3 Begin
  val s3_valid      = RegNext(s2_valid)
  val s3_req        = RegEnable(s2_req, s2_valid)
  val s3_newCoh     = RegEnable(s2_newCoh, s2_valid)
  val s3_repCoh     = RegEnable(s2_repCoh, s2_valid)
  val s3_wayEn      = RegEnable(s2_wayEn, s2_valid)
  val s3_tag        = RegEnable(s2_tag, s2_valid)
  val s3_updateMeta = RegNext(s2_updateMeta)
  val s3_updateData = RegNext(s2_updateData)

  s3_req.wdata := s2_storeData

  // meta write
  metaArray.io.write.valid         := s3_updateMeta
  metaArray.io.write.bits.setIdx   := getSetIdx(s3_req.paddr)
  metaArray.io.write.bits.wayEn    := s3_wayEn
  metaArray.io.write.bits.data.tag := getTag(s3_req.paddr)
  metaArray.io.write.bits.data.coh := s3_newCoh

  // data write
  dataArray.io.write.valid       := s3_updateData
  dataArray.io.write.bits.setIdx := getSetIdx(s3_req.paddr)
  dataArray.io.write.bits.bankEn := Fill(nBanks, true.B).asUInt
  dataArray.io.write.bits.wayEn  := s3_wayEn
  dataArray.io.write.bits.mask   := VecInit(Seq.fill(nBanks)(0.U)) // TODO
  dataArray.io.write.bits.data := VecInit(
    (0 until nBanks).map(i =>
      s3_req.wdata((i + 1) * rowBits - 1, i * rowBits)
    )
  )

  // * pipeline stage 3 End

  // * pipeline stage 4 Begin
  // used for forwarding
  val s4_valid  = RegNext(s3_valid)
  val s4_req    = RegEnable(s3_req, s3_valid)
  val s4_newCoh = RegEnable(s3_newCoh, s3_valid)
  val s4_repCoh = RegEnable(s3_repCoh, s3_valid)
  val s4_wayEn  = RegEnable(s3_wayEn, s3_valid)
  val s4_tag    = RegEnable(s3_tag, s3_valid)
  // * pipeline stage 4 End

  // * Store -> Load Bypassing Begin

  // s2/s3/s4 -> s1
  val s2_bypassStoreCandidate = createBypassStore(s2_storeData, getTag(s2_req.paddr), s2_newCoh, s2_wayEn)
  val s3_bypassStoreCandidate = createBypassStore(s3_req.wdata, getTag(s3_req.paddr), s3_newCoh, s3_wayEn)
  val s4_bypassStoreCandidate = createBypassStore(s4_req.wdata, getTag(s4_req.paddr), s4_newCoh, s4_wayEn)

  val bypassList = List(
    (s2_valid, s2_req, s2_bypassStoreCandidate, s2_tag, s2_repCoh),
    (s3_valid, s3_req, s3_bypassStoreCandidate, s3_tag, s3_repCoh),
    (s4_valid, s4_req, s4_bypassStoreCandidate, s4_tag, s4_repCoh),
  ).map(r =>
    (
      r._1 &&
        // s1 access/refill addr = s2/s3/s4 access/refill addr
        ((getLineAddr(s1_req.paddr) === getLineAddr(r._2.paddr)) ||
          // s1 replace way = s2/s3/s4 access/refill way; same set same way
          (getSetIdx(r._2.paddr) === getSetIdx(s1_req.paddr) &&
            (s1_refillWay === OHToUInt(r._3.wayEn) && s1_req.isRefill))),
      r._1 && r._2.isRefill && r._5.state > ClientStates.Nothing &&
        (getLineAddr(s1_req.paddr) === Cat(r._4, getSetIdx(r._2.paddr))), // s1 access addr = s2/s3/s4 replace addr
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
  val s1_updateReplacer = (s1_validFromCore && s1_hit) || (s1_validRefill && s1_canDoRefill)
  val touchWay          = OHToUInt(s1_wayEn)
  val touchSet          = getSetIdx(s1_req.paddr)

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
  s1_mshrAlloc := s1_validFromCore && ~s1_scFail && (~s1_hit && ~s1_amoNoPermMiss) && ~s1_wbqBlockMiss
  val s1_mshrAllocFail = s1_mshrAlloc && !mshrs.io.req.ready

  // mshr get store data in s2
  val s2_upgradePermMiss      = RegNext(s1_upgradePermMiss)
  val s2_mshrStoreData        = Mux(s2_upgradePermMiss, s2_mergeStoreData, s2_req.wdata)
  val s2_mshrStoreMaskInBytes = Mux(s2_upgradePermMiss, Fill(dataBytes, 1.U), s2_req.wmask)

  val mshrReq = WireDefault(s1_req)
  mshrReq.wdata := s2_mshrStoreData
  mshrReq.wmask := s2_mshrStoreMaskInBytes

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

  wbPipeReq.valid          := s1_probeWb | s1_replaceWb | s1_amoNoPermWb
  wbPipeReq.bits.voluntary := !s1_req.isProbe
  wbPipeReq.bits.data      := s1_data
  wbPipeReq.bits.lineAddr  := Mux(s1_replaceWb, s1_repLineAddr, getLineAddr(s1_req.paddr))
  wbPipeReq.bits.source    := s1_req.source
  wbPipeReq.bits.perm := Mux(
    s1_probeWb,
    s1_probeReportParam,
    Mux(s1_amoNoPermWb, s1_amoShrinkParam, s1_repShrinkParam),
  )
  wbPipeReq.bits.hasData := Mux(
    s1_probeWb,
    s1_probeWbData,
    Mux(s1_amoNoPermWb, s1_amoNoPermWbData, s1_replaceWbData),
  )

  // source 0: main pipeline
  wbArbiter.io.in(0) <> wbPipeReq
  // source 1: probe
  wbArbiter.io.in(1) <> probeQueue.io.wbReq

  // wbq req
  wbQueue.io.req <> wbArbiter.io.out
  // miss check
  wbQueue.io.missCheck.valid    := s1_validFromCore && s1_cacheable && ~s1_hit
  wbQueue.io.missCheck.lineAddr := getLineAddr(s1_req.paddr)
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
    Mux(
      s1_canProbe,
      ProbeRespStatus.probe_finish,
      ProbeRespStatus.probe_replay,
    ),
  )
  // * Probe End

  // * Resp Begin
  val s1_cacheResp = Wire(Valid(new DataExchangeResp))
  val mshrsResp    = Wire(Valid(new DataExchangeResp))

  //  s1 resp
  val loadGenAcc = new LoadGenAcc(
    s1_req.size,
    s1_req.signed,
    s1_req.paddr,
    s1_data,
    s1_sc,
    dataBytes,
    log2Up(rowBytes),
  )

  s1_cacheResp.valid        := s1_validFromCore && !isPrefetch(s1_req.cmd)
  s1_cacheResp.bits.source  := s1_req.source
  s1_cacheResp.bits.dest    := s1_req.dest
  s1_cacheResp.bits.hasData := (s1_hit && isRead(s1_req.cmd)) | s1_sc // FIXME SC
  s1_cacheResp.bits.data    := Mux(s1_sc, s1_scFail, loadGenAcc.genData())
  s1_cacheResp.bits.status := MuxCase(
    CacheRespStatus.miss,
    Seq(
      s1_hit
        -> CacheRespStatus.hit,
      (s1_wbqBlockMiss |   // miss addr exist in wbq
        s1_mshrAllocFail | // miss but mshr full
        s1_amoNoPermMiss | // amo no perm miss release B first
        s1_lrFail)         // lr failed
        -> CacheRespStatus.replay,
    ),
  )

  // mshr resp
  mshrsResp <> mshrs.io.resp

  // return resp
  io.nextCycleWb := mshrs.io.nextCycleWb
  io.resp.valid  := s1_cacheResp.valid | mshrsResp.valid
  io.resp.bits   := Mux(s1_cacheResp.valid, s1_cacheResp.bits, mshrsResp.bits)

  // when mshr wants to wb,
  // it will send nextCycleWb to kill other cache request
  assert(
    ~(s1_cacheResp.valid && mshrsResp.valid),
    "MSHR & Cache Resp cannot exist simultaneously",
  )

  io.fenceRdy := mshrs.io.fenceRdy && !s1_valid && !s2_valid && !s3_valid

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

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

  def onReset     = Metadata(0.U, ClientMetadata.onReset.state)
  val metaArray   = Module(new MetaArray[Metadata](() => onReset))
  val dataArray   = Module(new DataArray)
  val mshrs       = Module(new MSHRFile)
  val wbQueue     = Module(new WritebackQueue)
  val probeQueue  = Module(new ProbeQueue)
  val refillQueue = Module(new RefillQueueWrapper)

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

  val s1_bypassStore   = Reg(Valid(new BypassStore))
  val s1_bypassReplace = RegInit(false.B)

  val s1_cacheResp = Wire(Valid(new DataExchangeResp))
  val mshrsResp    = Wire(Valid(new DataExchangeResp))

  val victimWay = WireInit(0.U(log2Up(nWays).W))

  val s1_wbqBlockMiss = WireInit(false.B)

  val blockReq = io.nextCycleWb || (io.resp.valid && io.resp.bits.status === CacheRespStatus.replay)
  // * Signal Define End

  // * pipeline stage 0 Begin

  // req arbiter
  val mainReqArb = Module(new Arbiter(new MainPipeReq, 3))

  mainReqArb.io.in(0) <> probeQueue.io.mainPipeReq
  mainReqArb.io.in(1).valid := mshrs.io.toReplace.valid
  mainReqArb.io.in(1).bits  := MainPipeReqConverter(mshrs.io.toReplace.bits, victimWay)
  mainReqArb.io.in(2).valid := io.req.valid & !blockReq
  mainReqArb.io.in(2).bits  := MainPipeReqConverter(io.req.bits)
  mainReqArb.io.out.ready   := true.B

  probeQueue.io.mainPipeReq.ready := mainReqArb.io.in(0).ready
  mshrs.io.toReplace.ready        := mainReqArb.io.in(1).ready
  io.req.ready                    := mainReqArb.io.in(2).ready & !blockReq

  // get s0 req
  val s0_req   = mainReqArb.io.out.bits
  val s0_valid = mainReqArb.io.out.valid & ~(io.s0_kill && s0_req.isFromCore)

  // read tag array
  metaArray.io.read.valid       := s0_valid
  metaArray.io.read.bits.setIdx := getSetIdx(s0_req.paddr)
  metaArray.io.read.bits.wayEn  := Mux(s0_req.isRefill, UIntToOH(s0_req.refillWay), Fill(nWays, true.B))

  // read data array
  dataArray.io.read.valid       := s0_valid
  dataArray.io.read.bits.setIdx := getSetIdx(s0_req.paddr)
  dataArray.io.read.bits.bankEn := UIntToOH(getBankIdx(s0_req.paddr))
  dataArray.io.read.bits.wayEn  := Mux(s0_req.isRefill, UIntToOH(s0_req.refillWay), Fill(nWays, true.B))
  // * pipeline stage 0 End

  // * pipeline stage 1 Begin
  // 1. Obtain meta & data
  // 2. Organize Data
  // 3. Return Resp

  val s1_req   = RegEnable(s0_req, s0_valid)
  val s1_valid = RegNext(s0_valid) & ~(io.s1_kill && s1_req.isFromCore)

  val s1_validFromCore = s1_valid && s1_req.isFromCore
  val s1_validProbe    = s1_valid && s1_req.isProbe
  val s1_validRefill   = s1_valid && s1_req.isRefill

  // meta & data resp
  val s1_dataArrayResp = dataArray.io.resp // nways nbanks data
  val s1_metaArrayResp = metaArray.io.resp // nways meta

  // obtain tag for load/store/probe

  val s1_tagMatchWayPreBypassVec = VecInit((0 until nWays).map(w =>
    s1_metaArrayResp(w).tag === getTag(s1_req.paddr) && s1_metaArrayResp(w).coh > 0.U
  ))

  val s1_tagMatchWayVec = MuxCase(
    s1_tagMatchWayPreBypassVec,
    Seq(
      s1_bypassReplace -> VecInit(Seq.fill(nWays)(false.B)),
      s1_bypassStore.valid -> VecInit(
        (0 until nWays).map(w =>
          s1_bypassStore.bits.wayEn(w) && s1_bypassStore.bits.coh > 0.U
        )
      ),
    ),
  )

  val s1_tagMatch = s1_tagMatchWayVec.asUInt.orR

  val s1_metaPreBypass = Mux(
    s1_req.isRefill,
    s1_metaArrayResp(s1_req.refillWay),
    Mux1H(s1_tagMatchWayPreBypassVec, s1_metaArrayResp),
  )

  val s1_meta = MuxCase(
    s1_metaPreBypass,
    Seq(
      s1_bypassReplace     -> Metadata(0.U, 0.U),
      s1_bypassStore.valid -> Metadata(s1_bypassStore.bits.tag, s1_bypassStore.bits.coh),
    ),
  )

  val s1_tag     = s1_meta.tag
  val s1_cohMeta = ClientMetadata(s1_meta.coh) // obtain coh for load/store/probe/replace

  // hit/miss for load/store
  val s1_hasPerm         = s1_cohMeta.onAccess(s1_req.cmd)._1
  val s1_newHitCohMeta   = s1_cohMeta.onAccess(s1_req.cmd)._3
  val s1_hit             = s1_validFromCore && s1_tagMatch && s1_hasPerm
  val s1_upgradePermHit  = s1_hit && s1_newHitCohMeta =/= s1_cohMeta      // e.g. T->Dirty hit
  val s1_noDataMiss      = s1_validFromCore && !s1_tagMatch               // e.g. N->B or N->T miss
  val s1_upgradePermMiss = s1_validFromCore && s1_tagMatch && !s1_hasPerm // e.g. B->T miss

  // organize read data
  val s1_dataPreBypass =
    Mux(s1_req.isRefill, s1_dataArrayResp(s1_req.refillWay), Mux1H(s1_tagMatchWayPreBypassVec, s1_dataArrayResp)).asUInt
  val s1_data = Mux(s1_bypassStore.valid, s1_bypassStore.bits.data, s1_dataPreBypass)
  val loadGen = new LoadGen(s1_req.size, s1_req.signed, s1_req.paddr, s1_data, false.B, dataBytes)

  // organize store data
  val s1_storeGenMask = new StoreGen(s1_req.size, s1_req.paddr, 0.U, dataBytes).mask
  val s1_maskInBytes = Mux(
    s1_req.isRefill,
    Fill(dataBytes, 1.U),
    Mux(s1_req.cmd === M_PWR, s1_req.wmask, s1_storeGenMask), // FIXME wmask
  )
  val s1_mask           = FillInterleaved(8, s1_maskInBytes)
  val s1_mergeStoreData = s1_req.wdata & s1_mask | s1_data & ~s1_mask // FIXME wdata incorrect
  // TODO: PWR assertion
  // TODO: Merge Store & AMO Store

  // amo store data
  val amoalu          = Module(new AMOALU(XLEN))
  val s1_blockOffset  = getBlockOffset(s1_req.paddr)
  val s1_amoStoreData = (amoalu.io.out << (s1_blockOffset << 3) & s1_mask) | s1_data & ~s1_mask

  amoalu.io.mask := new StoreGen(s1_req.size, s1_req.paddr, 0.U, XLEN).mask
  amoalu.io.cmd  := s1_req.cmd
  amoalu.io.lhs  := (s1_data >> (s1_blockOffset << 3))(XLEN - 1, 0)
  amoalu.io.rhs  := s1_req.wdata

  // lrsc
  val lrscCount = RegInit(0.U)
  val lrscValid = lrscCount > lrscBackoff.U

  val s1_lr = s1_validFromCore && (s1_req.cmd === M_XLR)
  val s1_sc = s1_validFromCore && (s1_req.cmd === M_XSC)

  val lrscAddr         = RegEnable(s1_req.paddr >> blockOffBits, s1_lr)
  val s1_lrscAddrMatch = lrscValid && (s1_req.paddr >> blockOffBits === lrscAddr)
  val s1_scFail        = s1_sc && !s1_lrscAddrMatch // FIXME: s1 sc miss?

  lrscCount := MuxCase(
    lrscCount,
    Seq(
      // (lr | sc | other cmd) after lr hit
      (s1_validFromCore & lrscCount > 0.U) -> 0.U,
      // lr hit
      (s1_hit && s1_lr) -> (lrscCycles - 1).U,
      // no cmd after lr hit
      (lrscCount > 0.U) -> (lrscCount - 1.U),
    ),
  )

  val s1_storeUpdateMeta = (s1_upgradePermHit || s1_upgradePermMiss) && !s1_scFail
  val s1_storeUpdateData = (s1_hit && isWrite(s1_req.cmd)) && !s1_scFail

  // probe
  val s1_probeDirty       = s1_cohMeta.onProbe(s1_req.probePerm)._1
  val s1_probeReportParam = s1_cohMeta.onProbe(s1_req.probePerm)._2
  val s1_newProbeCoh      = s1_cohMeta.onProbe(s1_req.probePerm)._3
  val s1_probeUpdateMeta  = s1_validProbe && s1_tagMatch                  // probe should update meta
  val s1_probeWb          = s1_validProbe                                 // probe should send probeAck
  val s1_probeWbData      = s1_validProbe && s1_tagMatch && s1_probeDirty // probe has data

  // replace
  val s1_repLineAddr    = Cat(s1_tag, getSetIdx(s1_req.paddr)) // FIXME
  val s1_repMeta        = s1_cohMeta
  val s1_repDirty       = s1_repMeta.onCacheControl(M_FLUSH)._1
  val s1_repShrinkParam = s1_repMeta.onCacheControl(M_FLUSH)._2

  val s1_refillUpdateMeta = s1_validRefill
  val s1_refillUpdateData = s1_validRefill
  val s1_refillData       = s1_req.wdata
  val s1_replaceWb        = s1_validRefill && (s1_repMeta.state > ClientStates.Branch)
  val s1_replaceWbData    = s1_repDirty

  // prepare for s2
  val s1_updateMeta = s1_storeUpdateMeta || s1_probeUpdateMeta || s1_refillUpdateMeta
  val s1_updateData = s1_storeUpdateData || s1_refillUpdateData

  // FIXME mask
  val s1_storeData =
    Mux(s1_req.isRefill, s1_req.wdata, Mux(isAMO(s1_req.cmd), s1_amoStoreData, s1_mergeStoreData))

  val s1_newCoh = MuxCase(
    s1_req.cmd,
    Seq(
      s1_validProbe      -> s1_newProbeCoh.state,
      s1_validRefill     -> s1_req.refillCoh,
      s1_hit             -> s1_newHitCohMeta.state,
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

  val s1_updateReplacer = s1_hit || s1_validRefill

  // * pipeline stage 1 End

  // * pipeline stage 2 Begin
  val s2_valid       = RegNext(s1_updateMeta || s1_updateData)
  val s2_req         = Reg(new MainPipeReq)
  val s2_wayEn       = RegInit(0.U(nWays.W))
  val s2_newCoh      = RegInit(0.U(cohBits.W))
  val s2_updateMeta  = RegNext(s1_updateMeta)
  val s2_updateData  = RegNext(s1_updateData)
  val s2_validRefill = RegNext(s1_validRefill)
  val s2_tag         = RegEnable(s1_tag, s1_valid)

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
  dataArray.io.write.bits.data   := VecInit((0 until nBanks).map(i => s2_req.wdata((i + 1) * rowBits - 1, i * rowBits)))
  dataArray.io.write.bits.mask   := VecInit(Seq.fill(nBanks)(0.U)) // TODO

  // * pipeline stage 2 End

  // *  Store -> Load Bypassing Begin

  // store/probe/refill -> load/store bypassing
  // bypass list (valid, req, data, (coh, wayEn))

  val s1_bypassStoreCandidate = createBypassStore(s1_storeData, getTag(s1_req.paddr), s1_newCoh, s1_wayEn)
  val s2_bypassStoreCandidate = createBypassStore(s2_req.wdata, getTag(s2_req.paddr), s2_newCoh, s2_wayEn)

  val bypassStoreList = List(
    (s1_updateMeta || s1_updateData, s1_req, s1_bypassStoreCandidate),
    (s2_valid, s2_req, s2_bypassStoreCandidate),
  ).map(r =>
    (
      r._1 && isWrite(r._2.cmd) && getLineAddr(s0_req.paddr) === getLineAddr(r._2.paddr),
      r._3,
    )
  )

  s1_bypassStore.valid := bypassStoreList.map(_._1).reduce(_ || _)
  s1_bypassStore.bits  := PriorityMux(bypassStoreList.map(d => (d._1, d._2)))

  // replace -> load/store bypassing
  // list (valid, req, refillWay, replace tag)
  val bypassReplaceList = List(
    (s1_validRefill, s1_req, s1_req.refillWay, s1_tag),
    (s2_validRefill, s2_req, s2_req.refillWay, s2_tag),
  )

  s1_bypassReplace := bypassReplaceList.map(r =>
    r._1 && (getLineAddr(s0_req.paddr) === Cat(r._4, getSetIdx(r._2.paddr)))
  ).reduce(_ || _)

  // * Store -> Load Bypassing End

  // * Replacer Begin

  val replacer = ReplacementPolicy.fromString(replacementPolicy, nWays, nSets)

  val touchWay = OHToUInt(s1_wayEn)
  val touchSet = getSetIdx(s1_req.paddr)

  // pipeline stage 1 -> hit update replacer
  when(s1_updateReplacer) {
    replacer.access(touchSet, touchWay)
  }

  val replSet = getSetIdx(mshrs.io.toReplace.bits.lineAddr << blockOffBits)
  victimWay := replacer.way(replSet) // FIXME

  // * Replacer End

  // * MSHR Begin

  dontTouch(mshrs.io)

  // pipeline miss -> mshr
  val s1_mshrAlloc     = s1_validFromCore && ~s1_hit && ~s1_scFail
  val s1_mshrAllocFail = s1_mshrAlloc && !mshrs.io.pipelineReq.ready

  mshrs.io.pipelineReq.valid              := s1_mshrAlloc
  mshrs.io.pipelineReq.bits.isUpgrade     := s1_upgradePermMiss
  mshrs.io.pipelineReq.bits.data          := s1_storeData
  mshrs.io.pipelineReq.bits.mask          := s1_req.wmask
  mshrs.io.pipelineReq.bits.lineAddr      := getLineAddr(s1_req.paddr)
  mshrs.io.pipelineReq.bits.meta.sourceId := s1_req.source
  mshrs.io.pipelineReq.bits.meta.offset   := getBlockOffset(s1_req.paddr)
  mshrs.io.pipelineReq.bits.meta.rwType   := isWrite(s1_req.cmd)
  mshrs.io.pipelineReq.bits.meta.regIdx   := s1_req.dest
  mshrs.io.pipelineReq.bits.meta.size     := s1_req.size
  mshrs.io.pipelineReq.bits.meta.signed   := s1_req.signed

  // mshr acquire block or perm from L2
  mshrs.io.toL2Req.ready := tl_out.a.ready
  tl_out.a.valid         := mshrs.io.toL2Req.valid
  tl_out.a.bits := edge.AcquireBlock(
    fromSource = mshrs.io.toL2Req.bits.entryId,
    toAddress = mshrs.io.toL2Req.bits.lineAddr << blockOffBits,
    lgSize = log2Ceil(blockBytes).U,
    growPermissions = mshrs.io.toL2Req.bits.perm,
  )._2

  // refillQueue -> mshr
  mshrs.io.fromRefill <> refillQueue.io.refillResp
  mshrs.io.probeRefill <> refillQueue.io.probeCheck

  // mshr send replace req to pipeline
  mshrs.io.toReplace.ready := true.B
  mshrs.io.replaceFinish   := s2_validRefill

  // probe -> mshr
  mshrs.io.probeCheck <> probeQueue.io.probeCheck

  mshrs.io.toPipeline.ready := true.B // FIXME

  // * MSHR End

  // * Refill Begin

  refillQueue.io.memFinish <> tl_out.e

  // set to default
  // actual connect see tl-d
  refillQueue.io.memGrant.valid := false.B
  refillQueue.io.memGrant.bits  := DontCare

  // TODO: add tl-e GrantAck

  // * Refill End

  // * Writeback Begin

  // wb in pipeline stage 1

  // source 0: main pipeline
  // source 1: probe
  val wbArbiter = Module(new Arbiter(new WritebackReq(edge.bundle), 2))

  val wbPipeReq = WireInit(0.U.asTypeOf(Decoupled(new WritebackReq(edge.bundle))))
  wbPipeReq.valid          := s1_probeWb || s1_replaceWb
  wbPipeReq.bits.voluntary := s1_req.isRefill
  wbPipeReq.bits.data      := s1_data
  wbPipeReq.bits.perm      := Mux(s1_probeWb, s1_probeReportParam, s1_repShrinkParam)
  wbPipeReq.bits.lineAddr  := Mux(s1_probeWb, getLineAddr(s1_req.paddr), s1_repLineAddr)
  wbPipeReq.bits.hasData   := Mux(s1_probeWb, s1_probeWbData, s1_replaceWbData)

  wbArbiter.io.in(0) <> wbPipeReq
  wbArbiter.io.in(1) <> probeQueue.io.wbReq

  wbQueue.io.req <> wbArbiter.io.out

  wbQueue.io.missCheck.valid    := s1_validFromCore && ~s1_hit
  wbQueue.io.missCheck.lineAddr := getLineAddr(s1_req.paddr)
  s1_wbqBlockMiss               := wbQueue.io.missCheck.blockMiss

  wbQueue.io.release <> tl_out.c
  // default value
  wbQueue.io.grant.valid := false.B
  wbQueue.io.grant.bits  := DontCare

  assert(!(s1_probeWbData && s1_replaceWbData))

  // * Writeback End

  // * Probe Begin
  probeQueue.io.memProbe <> tl_out.b

  probeQueue.io.lrscAddr.valid := lrscValid
  probeQueue.io.lrscAddr.bits  := lrscAddr

  // in MSHR: probeQueue.io.probeCheck <> mshrs.io.probeCheck

  probeQueue.io.probeResp := Mux(
    !s1_probeWb,
    ProbeRespStatus.probe_invalid,
    Mux(wbPipeReq.fire, ProbeRespStatus.probe_finish, ProbeRespStatus.probe_replay),
  )
  // * Probe End

  // * Resp Begin

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
  mshrsResp.valid        := mshrs.io.toPipeline.valid
  mshrsResp.bits.status  := CacheRespStatus.refill
  mshrsResp.bits.source  := mshrs.io.toPipeline.bits.sourceId
  mshrsResp.bits.dest    := mshrs.io.toPipeline.bits.regIdx
  mshrsResp.bits.data    := mshrs.io.toPipeline.bits.regData
  mshrsResp.bits.hasData := mshrs.io.toPipeline.valid

  // return resp
  io.nextCycleWb := mshrs.io.toPipeline.bits.nextCycleWb // FIXME
  io.resp.valid  := s1_cacheResp.valid || mshrsResp.valid
  io.resp.bits   := Mux(s1_cacheResp.valid, s1_cacheResp.bits, mshrsResp.bits)

  // when mshr wants to wb,
  // it will send nextCycleWb to kill other cache request
  assert(~(s1_cacheResp.valid && mshrsResp.valid))

  // * Resp End

  // FIXME
  // test tilelink
  tl_out.e.valid := false.B

  tl_out.d.ready := false.B
  when(tl_out.d.bits.opcode === TLMessages.ReleaseAck) {
    tl_out.d <> wbQueue.io.grant
  }.elsewhen(tl_out.d.bits.opcode === TLMessages.Grant || tl_out.d.bits.opcode === TLMessages.GrantData) {
    tl_out.d <> refillQueue.io.memGrant
  }.otherwise {
    assert(!tl_out.d.fire)
  }
  dontTouch(tl_out)
}

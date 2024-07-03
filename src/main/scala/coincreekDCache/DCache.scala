package coincreekDCache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import utility._

class CCDCache()(
    implicit p: Parameters
) extends BaseDCache {
  override lazy val module = new CCDCacheImp(this)
}

class CCDCacheImp(outer: BaseDCache) extends BaseDCacheImp(outer) {

  def onReset    = Metadata(0.U, ClientMetadata.onReset.state)
  val metaArray  = Module(new MetaArray[Metadata](() => onReset))
  val dataArray  = Module(new DataArray)
  val mshrs      = Module(new MSHRFile)
  val wbQueue    = Module(new WriteBackQueue)
  val probeQueue = Module(new ProbeQueue)
  val mainReqArb = Module(new Arbiter(new MainPipeReq, 3))

  probeQueue.io.mainPipeReq.ready := mainReqArb.io.in(0).ready
  mshrs.io.toReplace.ready        := mainReqArb.io.in(1).ready
  io.req.ready                    := mainReqArb.io.in(2).ready

  // * Signal Define Begin
  // Store -> Load Bypassing
  val s1_storeBypass     = RegInit(false.B)
  val s1_storeBypassData = RegInit(0.U(dataWidth.W))
  val s1_cacheResp       = Wire(Valid(new DataExchangeResp))
  val mshrsResp          = Wire(Valid(new DataExchangeResp))
  val victimWay          = WireInit(0.U(log2Up(nWays).W))
  // * Signal Define End

  // * pipeline stage 0 Begin

  // req arbiter
  mainReqArb.io.in(0) <> probeQueue.io.mainPipeReq
  mainReqArb.io.in(1).valid := mshrs.io.toReplace.valid
  mainReqArb.io.in(1).bits  := MainPipeReqConverter(mshrs.io.toReplace.bits, victimWay)
  mainReqArb.io.in(2).valid := io.req.valid
  mainReqArb.io.in(2).bits  := MainPipeReqConverter(io.req.bits)
  mainReqArb.io.out.ready   := true.B

  // get s0 req
  val s0_req   = mainReqArb.io.out.bits
  val s0_valid = mainReqArb.io.out.valid & ~(io.s0_kill && s0_req.isFromCore)

  // read tag array
  metaArray.io.read.valid       := s0_valid
  metaArray.io.read.bits.setIdx := AddrDecoder.getSetIdx(s0_req.paddr)
  metaArray.io.read.bits.wayEn  := Mux(s0_req.isRefill, UIntToOH(s0_req.refillWay), Fill(nWays, true.B))

  // read data array
  dataArray.io.read.valid       := s0_valid
  dataArray.io.read.bits.setIdx := AddrDecoder.getSetIdx(s0_req.paddr)
  dataArray.io.read.bits.bankEn := UIntToOH(AddrDecoder.getBankIdx(s0_req.paddr))
  dataArray.io.read.bits.wayEn  := Mux(s0_req.isRefill, UIntToOH(s0_req.refillWay), Fill(nWays, true.B))
  // * pipeline stage 0 End

  // * pipeline stage 1 Begin
  // 1. Obtain meta & data
  // 2. Organize Data
  // 3. Return Resp

  val s1_valid = RegNext(s0_valid) & ~io.s1_kill
  val s1_req   = RegNext(s0_req)

  val s1_validFromCore = s1_valid && s1_req.isFromCore
  val s1_validProbe    = s1_valid && s1_req.isProbe
  val s1_validRefill   = s1_valid && s1_req.isRefill

  // meta & data resp
  val s1_dataArrayResp = dataArray.io.resp // nways nbanks data
  val s1_metaArrayResp = metaArray.io.resp // nways meta

  // tag & coh match
  val s1_tagMatchWay = VecInit((0 until nWays).map(w =>
    s1_metaArrayResp(w).tag === AddrDecoder.getTag(s1_req.paddr) && s1_metaArrayResp(w).coh > 0.U
  ))
  val s1_tagMatch = s1_tagMatchWay.asUInt.orR

  val s1_cohMeta                        = ClientMetadata(Mux1H(s1_tagMatchWay, s1_metaArrayResp.map(_.coh)))
  val (s1_hasPerm, _, s1_newHitCohMeta) = s1_cohMeta.onAccess(s1_req.cmd)
  val s1_hit                            = s1_valid && s1_tagMatch && s1_hasPerm
  val s1_upgradePermMiss                = s1_valid && s1_tagMatch && !s1_hasPerm

  // organize read data
  val s1_dataPreBypass = Mux1H(s1_tagMatchWay, s1_dataArrayResp).asUInt
  val s1_data          = Mux(s1_storeBypass, s1_storeBypassData, s1_dataPreBypass)
  val loadGen          = new LoadGen(s1_req.size, s1_req.signed, s1_req.paddr, s1_data, false.B, dataBytes)

  // organize store data
  val s1_storeGenMask = new StoreGen(s1_req.size, s1_req.paddr, 0.U, dataBytes).mask
  val s1_maskInBytes = Mux(
    s1_req.isRefill,
    Fill(dataBytes, 1.U),
    Mux(s1_req.cmd === MemoryOpConstants.M_PWR, s1_req.wmask, s1_storeGenMask),
  )
  val s1_mask           = FillInterleaved(8, s1_maskInBytes)
  val s1_mergeStoreData = s1_req.wdata & s1_mask | s1_data & ~s1_mask
  // TODO: PWR assertion
  // TODO: Merge Store & AMO Store

  // amo store data
  val amoalu          = Module(new AMOALU(dataWidth))
  val s1_amoStoreData = amoalu.io.out

  amoalu.io.mask := s1_maskInBytes
  amoalu.io.cmd  := s1_req.cmd
  amoalu.io.lhs  := s1_data
  amoalu.io.rhs  := s1_req.wdata

  val s1_storeData = Mux(MemoryOpConstants.isAMO(s1_req.cmd), s1_amoStoreData, s1_mergeStoreData)

  // lrsc
  val lrscCount = RegInit(0.U)
  val lrscValid = lrscCount > lrscBackoff.U

  val s1_lr = s1_valid && (s1_req.cmd === MemoryOpConstants.M_XLR)
  val s1_sc = s1_valid && (s1_req.cmd === MemoryOpConstants.M_XSC)

  val lrscAddr         = RegEnable(s1_req.paddr >> blockOffBits, s1_lr)
  val s1_lrscAddrMatch = lrscValid && (s1_req.paddr >> blockOffBits === lrscAddr)
  val s1_scFail        = s1_sc && !s1_lrscAddrMatch // FIXME: s1 sc miss?

  lrscCount := MuxCase(
    lrscCount,
    Seq(
      // (lr | sc | other cmd) after lr hit
      (s1_valid & lrscCount > 0.U) -> 0.U,
      // lr hit
      (s1_hit && s1_lr) -> (lrscCycles - 1).U,
      // no cmd after lr hit
      (lrscCount > 0.U) -> (lrscCount - 1.U),
    ),
  )

  val s1_storeUpdateMeta =
    s1_hit && (s1_cohMeta =/= s1_newHitCohMeta) && MemoryOpConstants.isWrite(s1_req.cmd) // hit but need to upgrade perm
  val s1_storeClearMeta  = s1_upgradePermMiss
  val s1_storeUpdateData = (s1_hit && MemoryOpConstants.isWrite(s1_req.cmd)) || (s1_sc && ~s1_scFail)

  // probe
  val s1_pbDirty         = s1_cohMeta.onProbe(s1_req.probePerm)._1
  val s1_newProbeCoh     = s1_cohMeta.onProbe(s1_req.probePerm)._3
  val s1_probeUpdateMeta = s1_validProbe && s1_tagMatch               // probe should update meta
  val s1_probeWb         = s1_validProbe
  val s1_probeWbData     = s1_validProbe && s1_tagMatch && s1_pbDirty // probe has data

  // replace
  val s1_repLineAddr      = Cat(s1_metaArrayResp(s1_req.refillWay).tag, AddrDecoder.getSetIdx(s1_req.paddr))
  val s1_repData          = s1_dataArrayResp(s1_req.refillWay).asUInt
  val s1_repMeta          = ClientMetadata(s1_metaArrayResp(s1_req.refillWay).coh)
  val (s1_repDirty, _, _) = s1_repMeta.onCacheControl(MemoryOpConstants.M_FLUSH)

  val s1_refillUpdateMeta = s1_validRefill
  val s1_refillUpdateData = s1_validRefill
  val s1_replaceWb        = s1_validRefill && s1_repMeta.state > 0.U
  val s1_replaceWbData    = s1_validRefill && s1_repDirty

  val s1_updateMeta = s1_storeUpdateMeta || s1_storeClearMeta || s1_probeUpdateMeta || s1_refillUpdateMeta
  val s1_updateData = s1_storeUpdateData || s1_refillUpdateData

  // * pipeline stage 1 End

  // * pipeline stage 2 Begin
  val s2_valid          = RegNext(s1_updateMeta || s1_updateData)
  val s2_req            = Reg(new MainPipeReq)
  val s2_wayEn          = RegInit(0.U(nWays.W))
  val s2_newCoh         = RegInit(0.U(cohBits.W))
  val s2_updateMeta     = RegNext(s1_updateMeta)
  val s2_updateData     = RegNext(s1_updateData)
  val s2_updateReplacer = RegNext(s1_hit || s1_validRefill)
  val s2_refillFinish   = RegNext(s1_validRefill)

  when(s1_updateMeta || s1_updateData || s2_updateReplacer) {
    s2_req := s1_req
    s2_newCoh := MuxCase(
      s1_req.cmd,
      Seq(
        s1_validProbe                            -> s1_newProbeCoh.state,
        s1_validRefill                           -> s1_req.refillCoh,
        (s1_validFromCore && s1_hit)             -> s1_newHitCohMeta.state,
        (s1_validFromCore && s1_upgradePermMiss) -> 0.U,// invalid
      ),
    )
    s2_wayEn := MuxCase(
      s1_tagMatchWay,
      Seq(
        s1_validProbe    -> s1_tagMatchWay,
        s1_validRefill   -> VecInit(UIntToOH(s1_req.refillWay).asBools),
        s1_validFromCore -> s1_tagMatchWay,
      ),
    ).asUInt
    s2_req.wdata := s1_storeData
  }

  // meta write
  metaArray.io.write.valid         := s2_updateMeta
  metaArray.io.write.bits.setIdx   := AddrDecoder.getSetIdx(s2_req.paddr)
  metaArray.io.write.bits.wayEn    := s2_wayEn
  metaArray.io.write.bits.data.tag := AddrDecoder.getTag(s2_req.paddr)
  metaArray.io.write.bits.data.coh := s2_newCoh

  // data write
  dataArray.io.write.valid       := s2_updateData
  dataArray.io.write.bits.setIdx := AddrDecoder.getSetIdx(s2_req.paddr)
  dataArray.io.write.bits.bankEn := Fill(nBanks, true.B).asUInt
  dataArray.io.write.bits.wayEn  := s2_wayEn
  dataArray.io.write.bits.data   := VecInit((0 until nBanks).map(i => s2_req.wdata((i + 1) * rowBits - 1, i * rowBits)))
  dataArray.io.write.bits.mask   := VecInit(Seq.fill(nBanks)(0.U)) // TODO

  // * pipeline stage 2 End

  // * pipeline stage 3 Begin
  val s3_valid = RegNext(s2_valid, false.B)
  val s3_req   = RegNext(s2_req)
  // * pipeline stage 3 End

  // *  Store -> Load Bypassing Begin
  // bypass list (valid, req, data)
  val bypassDataList = List(
    (s1_valid && !s1_scFail, s1_req, s1_storeData),
    (s2_valid, s2_req, s2_req.wdata),
    (s3_valid, s3_req, s3_req.wdata),
  ).map(r =>
    (r._1 && s1_req.paddr >> blockOffBits === r._2.paddr >> blockOffBits && MemoryOpConstants.isWrite(r._2.cmd), r._3)
  )

  s1_storeBypass     := bypassDataList.map(_._1).reduce(_ || _)
  s1_storeBypassData := PriorityMux(bypassDataList)
  // * Store -> Load Bypassing End

  // * Replacer Begin

  val replacer = ReplacementPolicy.fromString(replacementPolicy, nWays, nSets)

  val touchWay = OHToUInt(s2_wayEn)
  val touchSet = AddrDecoder.getSetIdx(s2_req.paddr)

  // pipeline stage 2 -> hit update replacer
  when(s2_updateReplacer) {
    replacer.access(touchSet, touchWay)
  }

  victimWay := replacer.way(AddrDecoder.getSetIdx(mshrs.io.toReplace.bits.lineAddr << blockOffBits)) // FIXME

  dontTouch(victimWay)

  // * Replacer End

  // * MSHR Begin

  dontTouch(mshrs.io)

  // pipeline miss -> mshr
  mshrs.io.pipelineReq.valid              := s1_validFromCore && ~s1_hit // FIXME
  mshrs.io.pipelineReq.bits.isUpgrade     := s1_upgradePermMiss
  mshrs.io.pipelineReq.bits.data          := s1_storeData
  mshrs.io.pipelineReq.bits.mask          := s1_req.wmask
  mshrs.io.pipelineReq.bits.lineAddr      := AddrDecoder.getLineAddr(s1_req.paddr)
  mshrs.io.pipelineReq.bits.meta.sourceId := s1_req.source
  mshrs.io.pipelineReq.bits.meta.offset   := AddrDecoder.getBlockOffset(s1_req.paddr)
  mshrs.io.pipelineReq.bits.meta.rwType   := MemoryOpConstants.isWrite(s1_req.cmd)
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

  // L2 send data to mshr
  mshrs.io.fromRefill.valid        := tl_out.d.valid
  mshrs.io.fromRefill.bits.data    := tl_out.d.bits.data
  mshrs.io.fromRefill.bits.entryId := tl_out.d.bits.source

  // mshr send replace req to pipeline
  mshrs.io.toReplace.ready := true.B
  mshrs.io.replaceFinish   := s2_refillFinish

  // probe -> mshr
  mshrs.io.fromProbe := DontCare

  mshrs.io.toPipeline.ready := true.B // FIXME

  // * MSHR End

  // * Writeback Begin

  // wb in pipeline stage 1

  wbQueue.io.req.valid          := s1_probeWb || s1_replaceWb
  wbQueue.io.req.bits           := DontCare
  wbQueue.io.req.bits.source    := 0.U // FIXME
  wbQueue.io.req.bits.voluntary := s1_req.isRefill
  wbQueue.io.req.bits.lineAddr  := Mux(s1_probeWb, AddrDecoder.getLineAddr(s1_req.paddr), s1_repLineAddr)
  wbQueue.io.req.bits.hasData   := Mux(s1_probeWb, s1_probeWbData, s1_replaceWbData)
  wbQueue.io.req.bits.data      := Mux(s1_probeWb, s1_dataPreBypass, s1_repData)

  wbQueue.io.missCheck.valid    := false.B
  wbQueue.io.missCheck.lineAddr := DontCare

  wbQueue.io.release <> tl_out.c
  // default value
  wbQueue.io.grant.valid := false.B
  wbQueue.io.grant.bits  := DontCare

  assert(!(s1_probeWbData && s1_replaceWbData))

  // * Writeback End

  // * Probe Begin
  probeQueue.io.memProbe <> tl_out.b

  probeQueue.io.lsrcValid             := false.B
  probeQueue.io.lsrcLineAddr          := DontCare
  probeQueue.io.probeCheck.blockProbe := false.B
  probeQueue.io.wbReady               := true.B
  // * Probe End

  // * Resp Begin

  //  s1 resp
  s1_cacheResp.valid        := s1_valid && s1_req.isFromCore
  s1_cacheResp.bits.status  := Mux(s1_hit, CacheRespStatus.hit, CacheRespStatus.miss)
  s1_cacheResp.bits.source  := s1_req.source
  s1_cacheResp.bits.dest    := s1_req.dest
  s1_cacheResp.bits.data    := Mux(s1_sc, s1_scFail, loadGen.data)
  s1_cacheResp.bits.hasData := MemoryOpConstants.isRead(s1_req.cmd)

  mshrsResp.valid        := mshrs.io.toPipeline.valid
  mshrsResp.bits.status  := CacheRespStatus.refill
  mshrsResp.bits.source  := mshrs.io.toPipeline.bits.sID
  mshrsResp.bits.dest    := mshrs.io.toPipeline.bits.regIdx
  mshrsResp.bits.data    := mshrs.io.toPipeline.bits.regData
  mshrsResp.bits.hasData := true.B

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
  tl_out.b.ready := false.B
  tl_out.e.valid := false.B

  tl_out.d.ready := false.B
  when(tl_out.d.bits.opcode === TLMessages.ReleaseAck) {
    tl_out.d <> wbQueue.io.grant
  }.elsewhen(tl_out.d.bits.opcode === TLMessages.Grant || tl_out.d.bits.opcode === TLMessages.GrantData) {
    tl_out.d.ready := true.B
  }.otherwise {
    assert(!tl_out.d.fire)
  }
  dontTouch(tl_out)
}

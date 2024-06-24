package coincreekDCache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class CCDCache()(
    implicit p: Parameters
) extends BaseDCache {
  override lazy val module = new CCDCacheImp(this)
}

class CCDCacheImp(outer: BaseDCache) extends BaseDCacheImp(outer) {

  def onReset   = Metadata(0.U, ClientMetadata.onReset.state)
  val metaArray = Module(new MetaArray[Metadata](() => onReset))
  val dataArray = Module(new DataArray())
  val wbQueue   = Module(new WriteBackQueue)

  // * Signal Define Begin
  // Store -> Load Bypassing
  val s1_storeBypass     = RegInit(false.B)
  val s1_storeBypassData = RegInit(0.U(dataWidth.W))
  // * Signal Define End

  // * pipeline stage 0 Begin
  val s0_valid = io.req.valid & ~io.s0_kill
  val s0_req   = io.req.bits
  // read tag array
  metaArray.io.read.valid       := s0_valid
  metaArray.io.read.bits.setIdx := AddrDecoder.getSetIdx(s0_req.paddr)
  metaArray.io.read.bits.wayEn  := Mux(s0_req.specifyValid, UIntToOH(s0_req.specifyWay), Fill(nWays, true.B))

  // read data array
  dataArray.io.read.valid       := s0_valid
  dataArray.io.read.bits.setIdx := AddrDecoder.getSetIdx(s0_req.paddr)
  dataArray.io.read.bits.bankEn := UIntToOH(AddrDecoder.getBankIdx(s0_req.paddr))
  dataArray.io.read.bits.wayEn  := Mux(s0_req.specifyValid, UIntToOH(s0_req.specifyWay), Fill(nWays, true.B))
  // * pipeline stage 0 End

  // * pipeline stage 1 Begin
  // 1. Judge Tag & Meta
  // 2. Organize Data
  // 3. Return Resp
  val s1_valid = RegNext(s0_valid) & !io.s1_kill
  val s1_req   = RegNext(s0_req)

  // meta & data resp
  val s1_dataArrayResp = dataArray.io.resp // nways nbanks data
  val s1_metaArrayResp = metaArray.io.resp // nways meta

  // tag & coh match
  val s1_tagMatchWay = VecInit((0 until nWays).map(w =>
    // tag match & coh valid
    s1_metaArrayResp(w).tag === AddrDecoder.getTag(s1_req.paddr) && s1_metaArrayResp(w).coh > 0.U
  ))
  val s1_tagMatch = s1_tagMatchWay.asUInt.orR

  val s1_cohMeta                        = ClientMetadata(Mux1H(s1_tagMatchWay, s1_metaArrayResp.map(_.coh)))
  val (s1_hasPerm, _, s1_newHitCohMeta) = s1_cohMeta.onAccess(s1_req.cmd)
  // special case: trunk -> dirty, has perm but not hit
  val s1_hit = s1_valid && s1_tagMatch && s1_hasPerm && (s1_cohMeta === s1_newHitCohMeta)

  // organize read data
  val s1_dataPreBypass = Mux1H(s1_tagMatchWay, s1_dataArrayResp).asUInt
  val s1_data          = Mux(s1_storeBypass, s1_storeBypassData, s1_dataPreBypass)
  val loadGen          = new LoadGen(s1_req.size, s1_req.signed, s1_req.paddr, s1_data, false.B, dataBytes)

  // organize store data
  val s1_storeGenMask   = new StoreGen(s1_req.size, s1_req.paddr, 0.U, dataBytes).mask
  val s1_maskInBytes    = Mux(s1_req.cmd === MemoryOpConstants.M_PWR, s1_req.wmask, s1_storeGenMask)
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

  // FIXME: s1 sc miss?
  val s1_scFail = s1_sc && !s1_lrscAddrMatch

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

  //  s1 resp
  val s1_cacheResp = Wire(Valid(new DataExchangeResp))

  s1_cacheResp.valid            := s1_valid
  s1_cacheResp.bits.hit         := s1_hit
  s1_cacheResp.bits.source      := RegNext(s1_req.source)
  s1_cacheResp.bits.data        := Mux(s1_sc, s1_scFail, loadGen.data)
  s1_cacheResp.bits.hasData     := MemoryOpConstants.isRead(s1_req.cmd)
  s1_cacheResp.bits.replay      := false.B
  s1_cacheResp.bits.nextCycleWb := false.B

  val s1_needWrite =
    s1_hit & MemoryOpConstants.isWrite(s1_req.cmd) || (s1_valid & s1_req.cmd === MemoryOpConstants.M_FILL) // TODO

  // * pipeline stage 1 End

  // * pipeline stage 2 Begin
  val s2_valid = RegNext(s1_valid & !s1_scFail & s1_needWrite)
  val s2_req   = Reg(new DataExchangeReq)
  val s2_wayEn = RegInit(0.U(nWays.W))

  when(s1_valid && s1_needWrite) {
    s2_req       := s1_req
    s2_wayEn     := Mux(s1_hit, s1_tagMatchWay, VecInit(UIntToOH(s1_req.specifyWay).asBools)).asUInt
    s2_req.wdata := s1_storeData
  }

  // meta write
  // FIXME
  metaArray.io.write.valid := s2_valid &&
    (s2_req.cmd === MemoryOpConstants.M_FILL || s2_req.cmd === MemoryOpConstants.M_REP)
  metaArray.io.write.bits.setIdx   := AddrDecoder.getSetIdx(s2_req.paddr)
  metaArray.io.write.bits.wayEn    := s2_wayEn
  metaArray.io.write.bits.data.tag := AddrDecoder.getTag(s2_req.paddr)
  metaArray.io.write.bits.data.coh := Mux(
    s2_req.cmd === MemoryOpConstants.M_REP,
    ClientStates.Nothing,
    ClientStates.Dirty,
  )

  // data write
  dataArray.io.write.valid       := s2_valid
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

  // return resp
  io.resp <> s1_cacheResp

  // * Writeback Begin
  wbQueue.io.req.valid          := s1_valid && (s1_req.cmd === MemoryOpConstants.M_REP)
  wbQueue.io.req.bits           := DontCare
  wbQueue.io.req.bits.addr      := s1_req.paddr
  wbQueue.io.req.bits.source    := 1.U
  wbQueue.io.req.bits.voluntary := true.B
  wbQueue.io.req.bits.hasData   := false.B
  wbQueue.io.missCheck.valid    := false.B
  wbQueue.io.missCheck.addr     := DontCare

  wbQueue.io.release <> tl_out.c
  // default value
  wbQueue.io.grant.valid := false.B
  wbQueue.io.grant.bits  := DontCare

  // * Writeback End

  // FIXME
  // test tilelink
  tl_out.a.valid := false.B
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

  io.req.ready := true.B
}

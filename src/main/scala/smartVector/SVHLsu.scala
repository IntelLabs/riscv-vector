package smartVector
import chisel3._
import chisel3.util._
import darecreek.VDecode
import utils._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import xiangshan.MicroOp
import SmartParam._
import AddrUtil._

class HLdstQueuePtr extends CircularQueuePtr[HLdstQueuePtr](nHLsuQueueEntries)

class HLSUMetaPtr extends CircularQueuePtr[HLSUMetaPtr](nHLsuMetaEntries)

class AddrInfo extends Bundle {
  val addr        = UInt(addrWidth.W)
  val startElem   = UInt(vlenbWidth.W)
  val elemCnt     = UInt((log2Ceil(dataBytes) + 1).W)
  val curSplitIdx = UInt(vlenbWidth.W)
  val curVl       = UInt(log2Up(vlenb * 8).W)
  val metaPtr     = UInt(nHLsuMetaWidth.W)
}

// horizontal LSU
class SVHLsu(
    implicit p: Parameters
) extends Module with HasCircularQueuePtrHelper {
  val io = IO(new LSUIO)

  // split fsm states
  val uop_idle :: uop_split :: Nil = Enum(2)

  val uopState  = RegInit(uop_idle)
  val stopSplit = WireInit(false.B)

  // Split info
  val splitCount  = RegInit(0.U(vlenbWidth.W))
  val curSplitIdx = RegInit(0.U(vlenbWidth.W))
  val splitStart  = RegInit(0.U(vlenbWidth.W))

  // addr
  val addrQueue = Module(new Queue(new AddrInfo, 1))
  val addrReg   = RegInit(0.U(addrWidth.W))

  // hlsu meta queue
  val metaQueue      = RegInit(VecInit(Seq.fill(nHLsuMetaEntries)(0.U.asTypeOf(new HLSUMeta))))
  val metaEnqPtr     = RegInit(0.U.asTypeOf(new HLSUMetaPtr))
  val metaLastEnqPtr = RegInit(0.U.asTypeOf(new HLSUMetaPtr))
  val metaDeqPtr     = RegInit(0.U.asTypeOf(new HLSUMetaPtr))
  val enqMeta        = metaQueue(metaLastEnqPtr.value)
  val deqMeta        = metaQueue(metaDeqPtr.value)

  val deqLsCtrl   = deqMeta.ldstCtrl
  val deqMuopInfo = deqMeta.muopInfo

  val deqMetaByteLevelMask = MaskGen.genByteLevelMask(
    deqMeta.elementMask.asUInt,
    deqLsCtrl.log2Memwb,
  )

  // ldQueue
  val ldstUopQueue = RegInit(VecInit(Seq.fill(nHLsuQueueEntries)(0.U.asTypeOf(new LdstUop))))
  val uopEnqPtr    = RegInit(0.U.asTypeOf(new HLdstQueuePtr))
  val uopIssuePtr  = RegInit(0.U.asTypeOf(new HLdstQueuePtr))
  val uopDeqPtr    = RegInit(0.U.asTypeOf(new HLdstQueuePtr))
  val uopRespPtr   = WireInit(0.U.asTypeOf(new HLdstQueuePtr))
  val issueUop     = ldstUopQueue(uopIssuePtr.value)
  val respUop      = ldstUopQueue(uopRespPtr.value)
  val deqUop       = ldstUopQueue(uopDeqPtr.value)

  // val hasXcptHappened
  // assert
  val memXcpt = io.dataExchange.xcpt.asUInt.orR
  val hasXcpt = RegInit(false.B)

  io.lsuReq.ready := (uopState === uop_idle) && !isFull(metaEnqPtr, metaDeqPtr)

  // * BEGIN
  // * Split LdstUop
  val (vstart, vl)     = (io.lsuReq.bits.vstart, io.lsuReq.bits.vl)
  val (uopIdx, uopEnd) = (io.lsuReq.bits.uopIdx, io.lsuReq.bits.uopEnd)

  val ldstCtrl = io.lsuReq.bits.ldstCtrl
  val mUopInfo = io.lsuReq.bits.muopInfo

  // * SPLIT FSM -- decide next state
  switch(uopState) {
    is(uop_idle) {
      when(io.lsuReq.fire)(uopState := uop_split)
    }
    is(uop_split) {
      when(stopSplit)(uopState := uop_idle)
    }
  }

  // if exception occurs or split finished, stop split
  stopSplit := hasXcpt || (curSplitIdx >= splitCount) || (splitCount === 0.U)

  // decide micro vl
  val totalVl = Mux(ldstCtrl.unitSMop === UnitStrideMop.mask, (vl + 7.U) >> 3.U, vl) // ceil(vl/8)
  val doneVl  = uopIdx << ldstCtrl.log2MinLen
  val leftVl = Mux(
    ldstCtrl.unitSMop === UnitStrideMop.whole_register,
    ldstCtrl.mlen,
    Mux(totalVl > doneVl, totalVl - doneVl, 0.U),
  )
  val curUopElemCnt   = ldstCtrl.minLen min leftVl
  val curUopElemStart = Mux(vstart < doneVl, 0.U, ldstCtrl.minLen min (vstart - doneVl))

  val doneElemCnt = RegInit(0.U(vlenbWidth.W))

  when(io.lsuReq.fire) {
    addrReg := Mux(
      uopIdx === 0.U,
      mUopInfo.rs1Val + (mUopInfo.segIdx << ldstCtrl.log2Memwb),
      addrReg,
    )

    doneElemCnt := Mux(uopIdx === 0.U, 0.U, doneElemCnt + curUopElemCnt)
    // Set split info
    curSplitIdx := 0.U
    splitCount  := curUopElemCnt
    splitStart  := curUopElemStart
    // set hlsu info
    when(io.lsuReq.bits.muopInfo.destVRegStart) {
      metaQueue(metaEnqPtr.value).valid     := true.B
      metaQueue(metaEnqPtr.value).ldstCtrl  := ldstCtrl
      metaQueue(metaEnqPtr.value).muopInfo  := mUopInfo
      metaQueue(metaEnqPtr.value).canCommit := (curUopElemCnt === 0.U) | (vstart > (leftVl + doneVl)) // FIXME
      (0 until vlenb).foreach { i =>
        metaQueue(metaEnqPtr.value).vregDataVec(i) :=
          mUopInfo.old_vd(8 * i + 7, 8 * i)
      }

      metaQueue(metaEnqPtr.value).elementMask :=
        MaskGen.genElementLevelMask(
          ldstCtrl.vm,
          mUopInfo.mask,
          doneElemCnt,
          curUopElemStart,
          curUopElemCnt,
        )

      metaLastEnqPtr := metaEnqPtr
      metaEnqPtr     := metaEnqPtr + 1.U
    }
  }

  // * BEGIN
  // * Calculate Addr
  // pipeline stage 0 --> calc addr
  val enqLsCtrl   = enqMeta.ldstCtrl
  val enqMuopInfo = enqMeta.muopInfo
  val addr        = WireInit(0.U(addrWidth.W))
  val baseAddr    = enqMuopInfo.rs1Val
  val curVl       = (enqMuopInfo.uopIdx << enqLsCtrl.log2MinLen) + curSplitIdx
  val isValidAddr = uopState === uop_split && !hasXcpt && (curSplitIdx < splitCount)

  val mlenMask = WireInit(0.U(vlenbWidth.W))
  val elenMask = WireInit(0.U(vlenbWidth.W))
  mlenMask := ((1.U << enqLsCtrl.log2Mlen) - 1.U)
  elenMask := ((1.U << enqLsCtrl.log2Elen) - 1.U)

  // * indexed addr
  val indexData   = WireInit(0.U(XLEN.W))
  val indexOffset = (curVl & elenMask) << (enqLsCtrl.log2Eewb)
  val loadGen     = new LoadGen(enqLsCtrl.log2Eewb, true.B, indexOffset, indexData, false.B, XLEN / 8)
  val curAddrIdx  = loadGen.data.asSInt

  elenMask := ((1.U << enqLsCtrl.log2Elen) - 1.U)
  (0 until VLEN / XLEN).foreach { i =>
    when((indexOffset >> log2Up(XLEN / 8)) === i.U) {
      indexData := enqMuopInfo.vs2(XLEN * (i + 1) - 1, XLEN * i)
    }
  }

  // * stride addr
  val stride     = WireInit(0.S(XLEN.W))
  val zeroStride = stride === 0.S
  val negStride  = stride < 0.S
  val strideAbs  = stride.abs().asUInt

  stride := MuxLookup(
    enqLsCtrl.ldstType,
    11111.U, // illegal value
    Seq(
      Mop.unit_stride     -> enqLsCtrl.memwb,
      Mop.constant_stride -> enqMuopInfo.rs2Val,
    ),
  ).asSInt

  val accelStrideList     = Seq.tabulate(log2Up(maxAccelerateStride) + 1)(i => 1.U << i)
  val accelLog2StrideList = Seq.tabulate(log2Up(maxAccelerateStride) + 1)(i => i.U)
  val accelStride         = Cat(accelStrideList.reverseMap(strideAbs === _))
  val canAccel            = (accelStride =/= 0.U || zeroStride) && ~isAddrMisalign(strideAbs, enqLsCtrl.log2Memwb)
  val log2Stride          = Mux(canAccel, Mux1H(accelStride, accelLog2StrideList), 0.U)

  val curStrideAddr = Mux(
    curSplitIdx === 0.U && enqMuopInfo.uopIdx === 0.U,
    addrReg,
    (addrReg.asSInt + stride.asSInt).asUInt,
  )

  enqMeta.zeroStride := zeroStride
  enqMeta.negStride  := negStride
  enqMeta.log2Stride := log2Stride

  // * addr
  addr := Mux(
    canAccel,
    addrReg,
    Mux1H(
      enqLsCtrl.ldstType,
      Seq(
        curStrideAddr,                                       // unit stride
        (Cat(false.B, baseAddr).asSInt + curAddrIdx).asUInt, // index_unodered
        curStrideAddr,                                       // strided
        (Cat(false.B, baseAddr).asSInt + curAddrIdx).asUInt, // index_odered
      ),
    ),
  )

  // * elem pos & cnt
  val addrOffset    = getAlignedOffset(addr)
  val startElem     = curVl & mlenMask
  val strideElemCnt = Mux(negStride, (addrOffset >> log2Stride) + 1.U, (dataBytes.U - addrOffset) >> log2Stride)
  val elemCnt       = Mux(canAccel, (splitCount - curSplitIdx) min strideElemCnt, 1.U)

  when(addrQueue.io.enq.fire) {
    curSplitIdx := curSplitIdx + elemCnt
    addrReg := Mux(
      canAccel && ~zeroStride,
      Mux(negStride, addr - (elemCnt << log2Stride), addr + (elemCnt << log2Stride)),
      addr,
    )
  }

  // * Calculate Addr
  // * END

  addrQueue.io.enq.valid            := isValidAddr
  addrQueue.io.enq.bits.addr        := addr
  addrQueue.io.enq.bits.startElem   := startElem
  addrQueue.io.enq.bits.elemCnt     := elemCnt
  addrQueue.io.enq.bits.curSplitIdx := curSplitIdx
  addrQueue.io.enq.bits.curVl       := curVl
  addrQueue.io.enq.bits.metaPtr     := metaLastEnqPtr.value
  addrQueue.io.deq.ready            := !isFull(uopEnqPtr, uopDeqPtr)

  val deqAddrInfoValid = addrQueue.io.deq.valid
  val deqAddrInfo      = addrQueue.io.deq.bits
  val accessMeta       = metaQueue(deqAddrInfo.metaPtr)
  val addrMisalign     = isAddrMisalign(deqAddrInfo.addr, accessMeta.ldstCtrl.log2Memwb)
  // val elemMaskVec      = VecInit(Seq.fill(vlenb)(false.B))
  val isNotMasked = true.B
  val canEnqueue =
    (accessMeta.ldstCtrl.vm || isNotMasked) && (deqAddrInfo.curSplitIdx + deqAddrInfo.elemCnt >= splitStart)
  val canWriteback =
    (deqAddrInfo.curSplitIdx + deqAddrInfo.elemCnt >= splitCount) && accessMeta.muopInfo.destVRegEnd

  when(addrQueue.io.deq.fire) {
    ldstUopQueue(uopEnqPtr.value).valid        := canEnqueue
    ldstUopQueue(uopEnqPtr.value).status       := Mux(addrMisalign, LdstUopStatus.ready, LdstUopStatus.notReady)
    ldstUopQueue(uopEnqPtr.value).addr         := deqAddrInfo.addr
    ldstUopQueue(uopEnqPtr.value).pos          := deqAddrInfo.curVl
    ldstUopQueue(uopEnqPtr.value).xcptValid    := addrMisalign
    ldstUopQueue(uopEnqPtr.value).addrMisalign := addrMisalign
    ldstUopQueue(uopEnqPtr.value).startElem    := deqAddrInfo.startElem
    ldstUopQueue(uopEnqPtr.value).elemCnt      := deqAddrInfo.elemCnt
    ldstUopQueue(uopEnqPtr.value).writeback    := canWriteback
    ldstUopQueue(uopEnqPtr.value).metaPtr      := deqAddrInfo.metaPtr

    uopEnqPtr := Mux(canEnqueue, uopEnqPtr + 1.U, uopEnqPtr)
  }

  // * Split LdstUop
  // * END

  // * BEGIN
  // * Issue LdstUop

  val isNoXcptUop = issueUop.valid & (~issueUop.xcptValid) &&
    !isFull(uopIssuePtr, uopDeqPtr) &&
    (issueUop.metaPtr === metaDeqPtr.value)

  uopRespPtr.value := io.dataExchange.resp.bits.idx(nLSUMaxQueueWidth - 1, 0)
  uopRespPtr.flag  := io.dataExchange.resp.bits.flag
  val respData = io.dataExchange.resp.bits.data

  // non store waiting code
  when(io.dataExchange.resp.bits.nack && (uopRespPtr < uopIssuePtr)) {
    uopIssuePtr := uopRespPtr
  }.elsewhen(isNoXcptUop && io.dataExchange.req.ready) {
    uopIssuePtr := uopIssuePtr + 1.U
  }

  val storeDataVec = VecInit(Seq.fill(dataBytes)(0.U(8.W)))
  val storeMaskVec = VecInit(Seq.fill(dataBytes)(0.U(1.W)))

  // * 1. vreg offset shift
  val stVRegOffset         = issueUop.startElem << deqLsCtrl.log2Memwb
  val stVRegOffShiftedData = WireInit(0.U(dataWidth.W))
  val stVRegOffShiftedMask = WireInit(0.U(dataBytes.W))
  val stPaddingMask        = VecInit(deqMeta.elementMask ++ VecInit(Seq.fill(vlenb)(false.B)))
  val stElemMask = (0 until vlenb).map(i =>
    (i.U < issueUop.elemCnt) & stPaddingMask(i.U + issueUop.startElem)
  )
  stVRegOffShiftedMask := MaskGen.genByteLevelMask(stElemMask.asUInt, deqLsCtrl.log2Memwb)
  stVRegOffShiftedData := DataGenUtil.groupShifter(true, 8)(deqMeta.vregDataVec.asUInt, stVRegOffset)

  // * 2. strided data selected
  val stStridedData = Mux(
    deqLsCtrl.ldstType === Mop.constant_stride,
    DataGen.genStridedStoreData(dataBytes, false)(
      stVRegOffShiftedData,
      deqLsCtrl.log2Memwb,
      deqMeta.log2Stride,
    ),
    stVRegOffShiftedData,
  )

  val stStridedMask = Mux(
    deqLsCtrl.ldstType === Mop.constant_stride,
    DataGen.genStridedStoreData(dataBytes, true)(
      stVRegOffShiftedMask,
      deqLsCtrl.log2Memwb,
      deqMeta.log2Stride,
    ),
    stVRegOffShiftedMask,
  )

  // * address offset shift
  val stAddrOffsetShiftedData = WireInit(0.U(dataWidth.W))
  val stAddrOffsetShiftedMask = WireInit(0.U(dataBytes.W))
  val stAddrOffset            = WireInit(0.U(log2Ceil(dataBytes).W))

  stAddrOffset := Mux(
    deqMeta.negStride,
    dataBytes.U - getAlignedOffset(issueUop.addr + deqLsCtrl.memwb),
    getAlignedOffset(issueUop.addr),
  )

  stAddrOffsetShiftedData := DataGenUtil.groupShifter(false, 8)(stStridedData, stAddrOffset)
  stAddrOffsetShiftedMask := stStridedMask << stAddrOffset

  // * store data reverse
  val stDataReversed = Mux(
    deqMeta.negStride,
    DataGen.reverseData(false)(stAddrOffsetShiftedData, deqLsCtrl.log2Memwb),
    stAddrOffsetShiftedData,
  )

  val stMaskReversed = Mux(
    deqMeta.negStride,
    DataGen.reverseData(true)(stAddrOffsetShiftedMask, deqLsCtrl.log2Memwb),
    stAddrOffsetShiftedMask,
  )

  // * gen store data & mask
  storeDataVec := VecInit(stDataReversed.asBools.grouped(8).toSeq.map(_.asUInt))
  storeMaskVec := VecInit(stMaskReversed.asBools)

  io.dataExchange.req.valid      := isNoXcptUop
  io.dataExchange.req.bits.addr  := getAlignedAddr(issueUop.addr)
  io.dataExchange.req.bits.cmd   := deqLsCtrl.isStore
  io.dataExchange.req.bits.srcId := 0.U
  io.dataExchange.req.bits.flag  := uopIssuePtr.flag
  io.dataExchange.req.bits.idx   := uopIssuePtr.value
  io.dataExchange.req.bits.data  := storeDataVec.asUInt
  io.dataExchange.req.bits.mask  := storeMaskVec.asUInt

  // * Issue LdstUop
  // * END

  // * BEGIN
  // * Recv Resp
  val respDataVec = VecInit(Seq.fill(dataBytes)(0.U(8.W)))
  (0 until dataBytes).foreach { i =>
    respDataVec(i) := respData(8 * i + 7, 8 * i)
  }

  when(io.dataExchange.resp.valid || memXcpt) {
    respUop.status    := LdstUopStatus.ready
    respUop.xcptValid := memXcpt
  }

  deqMeta.xcpt.xcptValid := memXcpt
  when(memXcpt && respUop.pos <= deqMeta.xcptVl) {
    deqMeta.xcptVl := respUop.pos
    deqMeta.xcpt.fromHellaXcpt(io.dataExchange.xcpt)
  }

  when(io.dataExchange.resp.valid && deqLsCtrl.isLoad) {
    // * 1. neg stride reverse
    val ldDataReversed = Mux(deqMeta.negStride, DataGen.reverseData(false)(respData, deqLsCtrl.log2Memwb), respData)

    // * 2. address offset shift
    val addrOffset = WireInit(0.U(log2Ceil(dataBytes).W))
    addrOffset := Mux(
      deqMeta.negStride,
      dataBytes.U - getAlignedOffset(respUop.addr + deqLsCtrl.memwb),
      getAlignedOffset(respUop.addr),
    )

    val addrOffsetShiftedData    = WireInit(0.U(dataWidth.W))
    val addrOffsetShiftedDataVec = (0 until dataBytes).map(i => addrOffsetShiftedData(8 * i + 7, 8 * i))
    addrOffsetShiftedData := DataGenUtil.groupShifter(true, 8)(ldDataReversed, addrOffset)

    // * 3. strided data selected
    val stridedData = DataGen.genStridedLoadData(dataBytes)(
      addrOffsetShiftedDataVec.asUInt,
      deqLsCtrl.log2Memwb,
      deqMeta.log2Stride,
    )

    val zeroStridedData = Mux1H(
      UIntToOH(deqLsCtrl.log2Memwb),
      Seq(8, 16, 32, 64).map(mew =>
        Fill(VLEN / mew, addrOffsetShiftedData(mew - 1, 0))
      ),
    )
    // * 4. shift to vreg offset
    val destVRegOffset = respUop.startElem << deqLsCtrl.log2Memwb
    val shiftedData2   = WireInit(0.U(VLEN.W))
    shiftedData2 := DataGenUtil.groupShifter(false, 8)(
      Mux(deqLsCtrl.ldstType === Mop.constant_stride, stridedData, addrOffsetShiftedData),
      destVRegOffset,
    )
    // * 5. vreg element merge mask gen
    val mergeElemMask =
      (0 until vlenb).map(i => i.U >= respUop.startElem && i.U < (respUop.startElem + respUop.elemCnt))
    val mergeByteMask = MaskGen.genByteLevelMask(mergeElemMask.asUInt, deqLsCtrl.log2Memwb)

    // * 6. vreg merge data gen
    deqMeta.vregDataVec := Mux(
      deqMeta.zeroStride,
      VecInit(zeroStridedData.asBools.grouped(8).toSeq.map(_.asUInt)),
      DataGen.mergeData(
        deqMeta.vregDataVec,
        VecInit(shiftedData2.asBools.grouped(8).toSeq.map(_.asUInt)),
        mergeByteMask,
      ),
    )

  }

  // * Recv Resp
  // * END

  // * BEGIN
  // * Commit to VRegInfo
  val canDeque = deqUop.valid && deqUop.status === LdstUopStatus.ready
  val deqXcpt  = canDeque && deqUop.xcptValid

  when(deqXcpt) {
    val misalignXcpt = WireInit(0.U.asTypeOf(new LdstXcpt))
    misalignXcpt.ma := deqUop.addrMisalign

    deqMeta.hasXcpt   := true.B
    deqMeta.canCommit := true.B
    deqMeta.xcptVl    := deqUop.pos
    deqMeta.xcptAddr  := deqUop.addr
    when(deqUop.addrMisalign) {
      deqMeta.xcpt := misalignXcpt
    }

    deqMeta.elementMask.zipWithIndex.foreach { case (elem, i) => elem := Mux(i.U >= deqUop.startElem, false.B, elem) }

  }.elsewhen(canDeque) {
    deqUop.valid := false.B
    uopDeqPtr    := uopDeqPtr + 1.U

    deqMeta.canCommit := deqUop.writeback
  }

  // * Commit to VRegIngo
  // * END

  // * BEGIN
  // * Writeback to uopQueue
  val fofValid = deqLsCtrl.unitSMop === UnitStrideMop.fault_only_first && deqMeta.xcptVl > 0.U

  io.lsuOut.valid                   := deqMeta.canCommit
  io.lsuOut.bits.muopEnd            := deqMeta.muopInfo.muopEnd
  io.lsuOut.bits.rfWriteEn          := deqMeta.muopInfo.rfWriteEn
  io.lsuOut.bits.rfWriteIdx         := deqMeta.muopInfo.ldest
  io.lsuOut.bits.rfWriteMask        := ~deqMetaByteLevelMask
  io.lsuOut.bits.regCount           := 1.U
  io.lsuOut.bits.regStartIdx        := deqMeta.muopInfo.ldest
  io.lsuOut.bits.isSegLoad          := false.B
  io.lsuOut.bits.data               := Cat(deqMeta.vregDataVec.reverseMap(entry => entry))
  io.lsuOut.bits.xcpt.exception_vld := deqMeta.hasXcpt & ~fofValid
  io.lsuOut.bits.xcpt.xcpt_cause := Mux(
    fofValid,
    0.U.asTypeOf(new HellaCacheExceptions),
    deqMeta.xcpt.generateHellaXcpt(deqLsCtrl.isStore),
  )
  io.lsuOut.bits.xcpt.xcpt_addr   := deqMeta.xcptAddr
  io.lsuOut.bits.xcpt.update_vl   := deqMeta.hasXcpt & fofValid
  io.lsuOut.bits.xcpt.update_data := deqMeta.xcptVl

  when(deqMeta.hasXcpt) {
    // clear ldstUopQueue
    ldstUopQueue.foreach(uop => uop.valid := false.B)
    metaQueue.foreach(info => info := 0.U.asTypeOf(new HLSUMeta))

    metaEnqPtr := 0.U.asTypeOf(new HLSUMetaPtr)
    metaDeqPtr := 0.U.asTypeOf(new HLSUMetaPtr)

    uopIssuePtr := 0.U.asTypeOf(new HLdstQueuePtr)
    uopDeqPtr   := 0.U.asTypeOf(new HLdstQueuePtr)
    uopEnqPtr   := 0.U.asTypeOf(new HLdstQueuePtr)
  }.elsewhen(deqMeta.canCommit) {
    deqMeta    := 0.U.asTypeOf(new HLSUMeta)
    metaDeqPtr := metaDeqPtr + 1.U
  }

  // * Writeback to uopQueue
  // * END
}

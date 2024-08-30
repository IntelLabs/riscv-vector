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

  when(io.lsuReq.fire) {
    addrReg := Mux(
      uopIdx === 0.U,
      mUopInfo.rs1Val + (mUopInfo.segIdx << ldstCtrl.log2Memwb),
      addrReg,
    )
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

      // val curVl = uopIdx << ldstCtrl.log2MinLen

      // (0 until vlenb).foreach { i =>
      //   val pos = i.U >> ldstCtrl.log2Memwb
      //   metaQueue(metaEnqPtr.value).vregDataValid(i) :=
      //     mUopInfo.mask(curVl + pos) && (i.U >= curVl) && (i.U < (curVl + curUopElemCnt))
      // }

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

  val accelStrideList     = Seq.tabulate(log2Up(dataBytes))(i => 1.U << i)
  val accelLog2StrideList = Seq.tabulate(log2Up(dataBytes))(i => i.U)
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
  val elemMaskVec      = VecInit(Seq.fill(vlenb)(false.B))
  val isNotMasked      = elemMaskVec.asUInt =/= 0.U
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

    (0 until vlenb).foreach { i =>
      val curElemPos = i.U >> accessMeta.ldstCtrl.log2Memwb
      val belong =
        curElemPos >= deqAddrInfo.startElem && curElemPos < (deqAddrInfo.startElem + deqAddrInfo.elemCnt)
      val maskIdx = curElemPos - deqAddrInfo.startElem + deqAddrInfo.curVl

      elemMaskVec(i) := Mux(
        belong,
        (accessMeta.ldstCtrl.vm || accessMeta.muopInfo.mask(maskIdx))
          && (curElemPos >= splitStart) && (curElemPos < splitCount),
        false.B,
      )

      when(belong) {
        accessMeta.vregDataValid(i) := elemMaskVec(i) && !addrMisalign
      }
    }
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

  (0 until vlenb).foreach { i =>
    val curElemPos = i.U >> deqMeta.ldstCtrl.log2Memwb
    val belong     = (curElemPos >= issueUop.startElem) && (curElemPos < (issueUop.startElem + issueUop.elemCnt))
    when(belong) {
      val elemInnerOffset =
        getAlignedOffset(issueUop.addr) + (i.U - (curElemPos << deqMeta.ldstCtrl.log2Memwb))
      val curElemOffset = Mux(
        deqMeta.zeroStride,
        0.U,
        Mux(
          deqMeta.negStride,
          -(curElemPos - issueUop.startElem) << deqMeta.log2Stride,
          (curElemPos - issueUop.startElem) << deqMeta.log2Stride,
        ),
      )

      storeDataVec(elemInnerOffset + curElemOffset) := deqMeta.vregDataVec(i)
      storeMaskVec(elemInnerOffset + curElemOffset) := deqMeta.vregDataValid(i)
    }
  }

  io.dataExchange.req.valid      := isNoXcptUop
  io.dataExchange.req.bits.addr  := getAlignedAddr(issueUop.addr)
  io.dataExchange.req.bits.cmd   := deqMeta.ldstCtrl.isStore
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

  when(io.dataExchange.resp.valid && deqMeta.ldstCtrl.isLoad) {
    (0 until vlenb).foreach { i =>
      val curElemPos = i.U >> deqMeta.ldstCtrl.log2Memwb
      val belong     = (curElemPos >= respUop.startElem) && (curElemPos < (respUop.startElem + respUop.elemCnt))
      when(belong) {
        val elemInnerOffset =
          getAlignedOffset(respUop.addr) + (i.U - (curElemPos << deqMeta.ldstCtrl.log2Memwb))
        val curElemOffset = Mux(
          deqMeta.zeroStride,
          0.U,
          Mux(
            deqMeta.negStride,
            -(curElemPos - respUop.startElem) << deqMeta.log2Stride,
            (curElemPos - respUop.startElem) << deqMeta.log2Stride,
          ),
        )

        deqMeta.vregDataVec(i) := Mux(
          deqMeta.vregDataValid(i),
          respDataVec(elemInnerOffset + curElemOffset),
          deqMeta.vregDataVec(i),
        )
      }
    }
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

  }.elsewhen(canDeque) {
    deqUop.valid := false.B
    uopDeqPtr    := uopDeqPtr + 1.U

    deqMeta.canCommit := deqUop.writeback
  }

  // * Commit to VRegIngo
  // * END

  // * BEGIN
  // * Writeback to uopQueue
  val fofValid = deqMeta.ldstCtrl.unitSMop === UnitStrideMop.fault_only_first && deqMeta.xcptVl > 0.U

  io.lsuOut.valid                   := deqMeta.canCommit
  io.lsuOut.bits.muopEnd            := deqMeta.muopInfo.muopEnd
  io.lsuOut.bits.rfWriteEn          := deqMeta.muopInfo.rfWriteEn
  io.lsuOut.bits.rfWriteIdx         := deqMeta.muopInfo.ldest
  io.lsuOut.bits.rfWriteMask        := "h0".U // FIXME xcpt mask
  io.lsuOut.bits.regCount           := 1.U
  io.lsuOut.bits.regStartIdx        := deqMeta.muopInfo.ldest
  io.lsuOut.bits.isSegLoad          := false.B
  io.lsuOut.bits.data               := Cat(deqMeta.vregDataVec.reverseMap(entry => entry))
  io.lsuOut.bits.xcpt.exception_vld := deqMeta.hasXcpt & ~fofValid
  io.lsuOut.bits.xcpt.xcpt_cause := Mux(
    fofValid,
    0.U.asTypeOf(new HellaCacheExceptions),
    deqMeta.xcpt.generateHellaXcpt(deqMeta.ldstCtrl.isStore),
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

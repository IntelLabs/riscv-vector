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

class HLdstQueuePtr extends CircularQueuePtr[HLdstQueuePtr](nHLsuQueueEntries)

class HLSUMetaPtr extends CircularQueuePtr[HLSUMetaPtr](nHLsuMetaEntries)

class AddrInfo extends Bundle {
  val addr           = UInt(addrWidth.W)
  val startElemPos   = UInt(vlenbWidth.W)
  val canLoadElemCnt = UInt((log2Ceil(dataBytes) + 1).W)
  val curSplitIdx    = UInt(vlenbWidth.W)
  val curVl          = UInt(log2Up(vlenb * 8).W)
}

// horizontal LSU
class SVHLsu(
    implicit p: Parameters
) extends Module with HasCircularQueuePtrHelper {
  val io = IO(new LSUIO)

  // split fsm states
  val uop_idle :: uop_split :: Nil = Enum(2)

  val uopState     = RegInit(uop_idle)
  val nextUopState = WireInit(uop_idle)
  val stopSplit    = WireInit(false.B)

  // Split info
  val vstartGeVl  = RegInit(false.B)
  val splitCount  = RegInit(0.U(vlenbWidth.W))
  val curSplitIdx = RegInit(0.U(vlenbWidth.W))
  val splitStart  = RegInit(0.U(vlenbWidth.W))

  // addr
  val addrQueue = Module(new Queue(new AddrInfo, 1))
  val addrReg   = RegInit(0.U(addrWidth.W))

  // hlsu meta queue
  val metaQueue     = RegInit(VecInit(Seq.fill(nHLsuMetaEntries)(0.U.asTypeOf(new HLSUMeta))))
  val metaEnqPtr    = RegInit(0.U.asTypeOf(new HLSUMetaPtr))
  val metaAccessPtr = RegInit(0.U.asTypeOf(new HLSUMetaPtr))
  val metaDeqPtr    = RegInit(0.U.asTypeOf(new HLSUMetaPtr))
  val enqMeta       = metaQueue(metaEnqPtr.value)
  val accessMeta    = metaQueue(metaAccessPtr.value)
  val deqMeta       = metaQueue(metaDeqPtr.value)

  // ldQueue
  val canEnqueue   = WireInit(false.B)
  val ldstUopQueue = RegInit(VecInit(Seq.fill(nHLsuQueueEntries)(0.U.asTypeOf(new LdstUop))))
  val uopEnqPtr    = RegInit(0.U.asTypeOf(new HLdstQueuePtr))
  val uopIssuePtr  = RegInit(0.U.asTypeOf(new HLdstQueuePtr))
  val uopDeqPtr    = RegInit(0.U.asTypeOf(new HLdstQueuePtr))
  val uopRespPtr   = WireInit(0.U.asTypeOf(new HLdstQueuePtr))
  val issueUop     = ldstUopQueue(uopIssuePtr.value)
  val respUop      = ldstUopQueue(uopRespPtr.value)
  val deqUop       = ldstUopQueue(uopDeqPtr.value)

  // xcpt info
  val xcptVlReg    = RegInit(0.U(bVL.W))
  val xcptAddrReg  = RegInit(0.U(addrWidth.W))
  val hellaXcptReg = RegInit(0.U.asTypeOf(new HellaCacheExceptions))

  // val hasXcptHappened
  // assert
  val memXcpt = io.dataExchange.xcpt.asUInt.orR
  val hasXcpt = RegInit(false.B)

  // * BEGIN
  // * Split LdstUop

  /*
                                                     splitId+1
                    +--------+                       +--------+
                    |        |                       |        |
                    |   +----+---+  mUop.Valid  +----+----+   |
                    |-> |uop_idle|--------------|uop_split| <-|
                        +---+----+              +----+----+
                            |                        |
                completeLdst|                        |stopSplit
                            |   +----------------+   |
                            |-> |uop_split_finish| <-|
                                +----------------+

   */
  val (vstart, vl)     = (io.lsuReq.bits.vstart, io.lsuReq.bits.vl)
  val (uopIdx, uopEnd) = (io.lsuReq.bits.uopIdx, io.lsuReq.bits.uopEnd)

  val ldstCtrl = io.lsuReq.bits.ldstCtrl
  val mUopInfo = io.lsuReq.bits.muopInfo

  io.lsuReq.ready := (uopState === uop_idle) && !isFull(metaEnqPtr, metaDeqPtr)
  // SPLIT FSM -- decide next state
  switch(uopState) {
    is(uop_idle) {
      when(io.lsuReq.valid) { // not segment
        nextUopState := uop_split
      }.otherwise {
        nextUopState := uop_idle
      }
    }
    is(uop_split) {
      when(stopSplit) {
        nextUopState := uop_idle
      }.otherwise {
        nextUopState := uop_split
      }
    }
  }
  // SPLIT FSM -- transition
  uopState := nextUopState

  // HLSU info queue update logic

  metaEnqPtr := Mux(
    ((uopState === uop_split) && stopSplit) && enqMeta.muopInfo.destVRegEnd,
    metaEnqPtr + 1.U,
    metaEnqPtr,
  )

  // if exception occurs or split finished, stop split
  stopSplit := hasXcpt || (curSplitIdx >= splitCount) || (splitCount === 0.U)

  // decide micro vl
  val actualVl = Mux(ldstCtrl.unitSMop === UnitStrideMop.mask, (vl + 7.U) >> 3.U, vl) // ceil(vl/8)
  val doneLen  = uopIdx << ldstCtrl.log2MinLen
  val leftLen = Mux(
    ldstCtrl.unitSMop === UnitStrideMop.whole_register,
    ldstCtrl.mlen,
    Mux(actualVl > doneLen, actualVl - doneLen, 0.U),
  )
  val microVl     = ldstCtrl.minLen min leftLen
  val microVstart = Mux(vstart < doneLen, 0.U, ldstCtrl.minLen min (vstart - doneLen))

  when(io.lsuReq.fire) {
    addrReg := Mux(
      uopIdx === 0.U,
      mUopInfo.rs1Val + (mUopInfo.segIdx << ldstCtrl.log2Memwb),
      addrReg,
    )
    // Set split info
    curSplitIdx := 0.U
    splitCount  := microVl
    splitStart  := microVstart
    vstartGeVl  := vstart > actualVl
    // set hlsu info
    when(io.lsuReq.bits.muopInfo.destVRegStart) {
      enqMeta.valid    := true.B
      enqMeta.ldstCtrl := ldstCtrl
      enqMeta.muopInfo := mUopInfo
      (0 until vlenb).foreach { i =>
        enqMeta.vregDataVec(i) := mUopInfo.old_vd(8 * i + 7, 8 * i)
      }
    }
  }

  // * BEGIN
  // * Calculate Addr
  // pipeline stage 0 --> calc addr
  val addr     = WireInit(0.U(addrWidth.W))
  val offset   = WireInit(0.U(log2Ceil(dataBytes).W))
  val baseAddr = enqMeta.muopInfo.rs1Val

  val curVl       = (enqMeta.muopInfo.uopIdx << enqMeta.ldstCtrl.log2MinLen) + curSplitIdx
  val isValidAddr = uopState === uop_split && !hasXcpt && (curSplitIdx < splitCount)

  // indexed addr
  val idxVal  = WireInit(0.U(XLEN.W))
  val idxMask = WireInit(0.U(XLEN.W))
  val eew     = enqMeta.ldstCtrl.eewb << 3.U
  // FIXME
  val beginIdx =
    (curVl - ((curVl >> enqMeta.ldstCtrl.log2Elen) << enqMeta.ldstCtrl.log2Elen)) << (enqMeta.ldstCtrl.log2Eewb +& 3.U) // 3.U: to bits
  idxMask := (("h1".asUInt(addrWidth.W) << eew) - 1.U)
  idxVal  := (enqMeta.muopInfo.vs2 >> beginIdx) & idxMask

  val stride    = WireInit(0.S(XLEN.W))
  val negStride = stride < 0.S
  val strideAbs = Mux(negStride, (-stride).asUInt, stride.asUInt)

  when(enqMeta.ldstCtrl.ldstType === Mop.unit_stride) {
    stride := (enqMeta.ldstCtrl.nfield << enqMeta.ldstCtrl.log2Memwb).asSInt
  }.elsewhen(enqMeta.ldstCtrl.ldstType === Mop.constant_stride) {
    stride := enqMeta.muopInfo.rs2Val.asSInt
  }.otherwise {
    stride := 11111.S
  }

  val accelerateStride = Cat(strideAbs === 8.U, strideAbs === 4.U, strideAbs === 2.U, strideAbs === 1.U)
  val canAccelerate =
    (accelerateStride =/= 0.U || strideAbs === 0.U) &&
      ~AddrUtil.isAddrMisalign(strideAbs, enqMeta.ldstCtrl.log2Memwb)
  val log2Stride = Mux(canAccelerate, Mux1H(accelerateStride, Seq(0.U, 1.U, 2.U, 3.U)), 0.U)

  val curStridedAddr = Mux(
    curSplitIdx === 0.U && enqMeta.muopInfo.uopIdx === 0.U,
    addrReg,
    (addrReg.asSInt + stride.asSInt).asUInt,
  )

  enqMeta.zeroStride := strideAbs === 0.U
  enqMeta.negStride  := negStride
  enqMeta.log2Stride := log2Stride

  when(canAccelerate) {
    addr := addrReg
  }.otherwise {
    addr := Mux1H(
      enqMeta.ldstCtrl.ldstType,
      Seq(
        // unit stride
        curStridedAddr,
        // index_unodered
        (Cat(false.B, baseAddr).asSInt + idxVal.asSInt).asUInt,
        // strided
        curStridedAddr,
        // index_odered
        (Cat(false.B, baseAddr).asSInt + idxVal.asSInt).asUInt,
      ),
    )
  }

  offset := AddrUtil.getAlignedOffset(addr)

  val startElemMask = WireInit(0.U.asTypeOf(curVl))
  startElemMask := ((1.U << enqMeta.ldstCtrl.log2Mlen) - 1.U)
  val startElemPos   = curVl & startElemMask
  val canLoadElemCnt = WireInit(0.U((log2Ceil(dataBytes) + 1).W))
  canLoadElemCnt := Mux(
    canAccelerate,
    (splitCount - curSplitIdx) min
      Mux(
        negStride,
        (offset >> log2Stride) + 1.U((log2Ceil(dataBytes) + 1).W),
        (dataBytes.U >> log2Stride) - (offset >> log2Stride),
      ),
    1.U,
  )

  when(addrQueue.io.enq.fire) {
    curSplitIdx := curSplitIdx + canLoadElemCnt
    addrReg := Mux(
      canAccelerate && strideAbs =/= 0.U,
      Mux(negStride, addr - (canLoadElemCnt << log2Stride), addr + (canLoadElemCnt << log2Stride)),
      addr,
    )
  }

  // * Calculate Addr
  // * END

  addrQueue.io.enq.valid               := isValidAddr
  addrQueue.io.enq.bits.addr           := addr
  addrQueue.io.enq.bits.startElemPos   := startElemPos
  addrQueue.io.enq.bits.canLoadElemCnt := canLoadElemCnt
  addrQueue.io.enq.bits.curSplitIdx    := curSplitIdx
  addrQueue.io.enq.bits.curVl          := curVl

  // pipeline stage 1
  val deqAddrInfoValid = addrQueue.io.deq.valid
  val deqAddrInfo      = addrQueue.io.deq.bits
  addrQueue.io.deq.ready := !isFull(uopEnqPtr, uopDeqPtr)

  val s1_addrMisalign = AddrUtil.isAddrMisalign(deqAddrInfo.addr, enqMeta.ldstCtrl.log2Memwb)
  val s1_alignedAddr  = AddrUtil.getAlignedAddr(deqAddrInfo.addr)
  val s1_offset       = AddrUtil.getAlignedOffset(deqAddrInfo.addr)

  val elemMaskVec = VecInit(Seq.fill(vlenb)(false.B))
  val isNotMasked = elemMaskVec.asUInt =/= 0.U

  canEnqueue := (enqMeta.ldstCtrl.vm || isNotMasked) && (deqAddrInfo.curSplitIdx + deqAddrInfo.canLoadElemCnt >= splitStart)
  val destVRegEnd =
    (deqAddrInfo.curSplitIdx + deqAddrInfo.canLoadElemCnt >= splitCount) && enqMeta.muopInfo.destVRegEnd

  when(addrQueue.io.deq.fire) {
    when(destVRegEnd) {
      metaAccessPtr := metaAccessPtr + 1.U
    }

    ldstUopQueue(uopEnqPtr.value).valid        := canEnqueue
    ldstUopQueue(uopEnqPtr.value).status       := Mux(s1_addrMisalign, LdstUopStatus.ready, LdstUopStatus.notReady)
    ldstUopQueue(uopEnqPtr.value).memOp        := accessMeta.ldstCtrl.isStore
    ldstUopQueue(uopEnqPtr.value).addr         := deqAddrInfo.addr
    ldstUopQueue(uopEnqPtr.value).pos          := deqAddrInfo.curVl
    ldstUopQueue(uopEnqPtr.value).xcptValid    := s1_addrMisalign
    ldstUopQueue(uopEnqPtr.value).addrMisalign := s1_addrMisalign
    ldstUopQueue(uopEnqPtr.value).elemCnt      := deqAddrInfo.canLoadElemCnt
    ldstUopQueue(uopEnqPtr.value).destVRegEnd  := destVRegEnd
    ldstUopQueue(uopEnqPtr.value).destElem     := deqAddrInfo.startElemPos
    ldstUopQueue(uopEnqPtr.value).metaPtr      := metaAccessPtr.value

    uopEnqPtr := Mux(canEnqueue, uopEnqPtr + 1.U, uopEnqPtr)

    (0 until vlenb).foreach { i =>
      val curElemPos = i.U >> accessMeta.ldstCtrl.log2Memwb
      val belong =
        curElemPos >= deqAddrInfo.startElemPos && curElemPos < (deqAddrInfo.startElemPos + deqAddrInfo.canLoadElemCnt)
      val maskIdx = curElemPos - deqAddrInfo.startElemPos + deqAddrInfo.curVl

      elemMaskVec(i) := Mux(
        belong,
        (accessMeta.ldstCtrl.vm || accessMeta.muopInfo.mask(maskIdx))
          && (curElemPos >= splitStart) && (curElemPos < splitCount),
        false.B,
      )

      val maskCond = elemMaskVec(i) && !s1_addrMisalign

      when(belong) {
        // vregInfoValid(i) := maskCond
        accessMeta.vregDataValid(i) := maskCond
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
    val belong     = (curElemPos >= issueUop.destElem) && (curElemPos < (issueUop.destElem + issueUop.elemCnt))
    when(belong) {
      val elemInnerOffset =
        AddrUtil.getAlignedOffset(issueUop.addr) + (i.U - (curElemPos << deqMeta.ldstCtrl.log2Memwb))
      val curElemOffset = Mux(
        deqMeta.zeroStride,
        0.U,
        Mux(
          deqMeta.negStride,
          -(curElemPos - issueUop.destElem) << deqMeta.log2Stride,
          (curElemPos - issueUop.destElem) << deqMeta.log2Stride,
        ),
      )

      storeDataVec(elemInnerOffset + curElemOffset) := deqMeta.vregDataVec(i)
      storeMaskVec(elemInnerOffset + curElemOffset) := deqMeta.vregDataValid(i)
    }
  }

  io.dataExchange.req.valid      := isNoXcptUop
  io.dataExchange.req.bits.addr  := AddrUtil.getAlignedAddr(issueUop.addr)
  io.dataExchange.req.bits.cmd   := issueUop.memOp
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

  when(io.dataExchange.resp.valid && respUop.memOp === VMemCmd.read) {
    (0 until vlenb).foreach { i =>
      val curElemPos = i.U >> deqMeta.ldstCtrl.log2Memwb
      val belong     = (curElemPos >= respUop.destElem) && (curElemPos < (respUop.destElem + respUop.elemCnt))
      when(belong) {
        val elemInnerOffset =
          AddrUtil.getAlignedOffset(respUop.addr) + (i.U - (curElemPos << deqMeta.ldstCtrl.log2Memwb))
        val curElemOffset = Mux(
          deqMeta.zeroStride,
          0.U,
          Mux(
            deqMeta.negStride,
            -(curElemPos - respUop.destElem) << deqMeta.log2Stride,
            (curElemPos - respUop.destElem) << deqMeta.log2Stride,
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

  val vregCanCommit = RegInit(false.B)

  when(deqXcpt) {
    // update xcpt info
    val xcptInfo = WireInit(0.U.asTypeOf(new HellaCacheExceptions))
    when(deqUop.addrMisalign) {
      xcptInfo.ma.ld := deqUop.memOp === VMemCmd.read
      xcptInfo.ma.st := deqUop.memOp === VMemCmd.write
    }.otherwise {
      xcptInfo := deqMeta.xcpt.generateHellaXcpt(deqUop.memOp === VMemCmd.write)
    }
    xcptVlReg    := deqUop.pos
    xcptAddrReg  := deqUop.addr
    hellaXcptReg := xcptInfo
    hasXcpt      := true.B
  }.elsewhen(canDeque) {
    deqUop.valid  := false.B
    vregCanCommit := deqUop.destVRegEnd
    uopDeqPtr     := uopDeqPtr + 1.U
  }

  // * Commit to VRegIngo
  // * END

  // * BEGIN
  // * Writeback to uopQueue
  val vregWbReady = (splitCount === 0.U && RegNext(io.lsuReq.fire)) || vregCanCommit
  val wbValid     = vregWbReady || hasXcpt
  val fofValid    = deqMeta.ldstCtrl.unitSMop === UnitStrideMop.fault_only_first && xcptVlReg > 0.U

  io.lsuOut.valid                   := wbValid
  io.lsuOut.bits.muopEnd            := deqMeta.muopInfo.muopEnd
  io.lsuOut.bits.rfWriteEn          := deqMeta.muopInfo.rfWriteEn
  io.lsuOut.bits.rfWriteIdx         := deqMeta.muopInfo.ldest
  io.lsuOut.bits.rfWriteMask        := "hffff".U
  io.lsuOut.bits.regCount           := 1.U
  io.lsuOut.bits.regStartIdx        := deqMeta.muopInfo.ldest
  io.lsuOut.bits.isSegLoad          := false.B
  io.lsuOut.bits.data               := Cat(deqMeta.vregDataVec.reverseMap(entry => entry))
  io.lsuOut.bits.xcpt.exception_vld := hasXcpt & ~fofValid
  io.lsuOut.bits.xcpt.xcpt_cause    := Mux(fofValid, 0.U.asTypeOf(new HellaCacheExceptions), hellaXcptReg)
  io.lsuOut.bits.xcpt.xcpt_addr     := xcptAddrReg
  io.lsuOut.bits.xcpt.update_vl     := hasXcpt & fofValid
  io.lsuOut.bits.xcpt.update_data   := xcptVlReg

  when(hasXcpt) {
    // reset xcpt
    hasXcpt := false.B
    // clear ldstUopQueue
    ldstUopQueue.foreach(uop => uop.valid := false.B)
    metaQueue.foreach(info => info.valid := false.B)

    metaEnqPtr    := 0.U.asTypeOf(new HLSUMetaPtr)
    metaAccessPtr := 0.U.asTypeOf(new HLSUMetaPtr)
    metaDeqPtr    := 0.U.asTypeOf(new HLSUMetaPtr)

    uopIssuePtr := 0.U.asTypeOf(new HLdstQueuePtr)
    uopDeqPtr   := 0.U.asTypeOf(new HLdstQueuePtr)
    uopEnqPtr   := 0.U.asTypeOf(new HLdstQueuePtr)
  }.elsewhen(vregWbReady) {
    deqMeta.valid := false.B
    metaDeqPtr    := metaDeqPtr + 1.U
  }

  when(wbValid) {
    vregCanCommit := false.B
  }

  // * Writeback to uopQueue
  // * END
}

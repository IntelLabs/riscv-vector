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

class HLSUPtr extends CircularQueuePtr[HLSUPtr](nHLsuQueueEntries)

class HLSUInfoPtr extends CircularQueuePtr[HLSUInfoPtr](nHLsuQueueEntries)

class SplitInfo extends Bundle {
  val strideAbs      = UInt(XLEN.W)
  val negStride      = Bool()
  val log2Stride     = UInt(2.W) // 0-3
  val addr           = UInt(addrWidth.W)
  val startElemPos   = UInt(vlenbWidth.W)
  val endElemPos     = UInt(vlenbWidth.W)
  val startVRegIdx   = UInt(vlenbWidth.W)
  val endVRegIdx     = UInt(vlenbWidth.W)
  val canLoadElemCnt = UInt((log2Ceil(dataBytes) + 1).W)
  val curSplitIdx    = UInt(vlenbWidth.W)
  val curVl          = UInt(vlenbWidth.W)
}

// horizontal LSU
class SVHLsu(
    implicit p: Parameters
) extends Module with HasCircularQueuePtrHelper {
  val io = IO(new LSUIO)

  // split fsm states
  val uop_idle :: uop_split :: uop_split_finish :: Nil = Enum(3)

  val uopState     = RegInit(uop_idle)
  val nextUopState = WireInit(uop_idle)
  val completeLdst = WireInit(false.B)
  val stopSplit    = WireInit(false.B)

  val infoQueue = Module(new Queue(new SplitInfo, 1))

  val hlsuInfoEnqPtr = RegInit(0.U.asTypeOf(new HLSUInfoPtr))
  val hlsuInfoDeqPtr = RegInit(0.U.asTypeOf(new HLSUInfoPtr))
  val hlsuInfoQueue  = RegInit(VecInit(Seq.fill(nHLsuQueueEntries)(0.U.asTypeOf(new HLSUInfo))))

  // address reg
  val s1_isValidAddr = WireInit(false.B)
  val addrReg        = RegInit(0.U(addrWidth.W))

  // Split info
  val vstartGeVl  = RegInit(false.B)
  val splitCount  = RegInit(0.U(vlenbWidth.W))
  val curSplitIdx = RegInit(0.U(vlenbWidth.W))
  val splitStart  = RegInit(0.U(vlenbWidth.W))

  // ldQueue
  val canEnqueue   = WireInit(false.B)
  val enqPtr       = RegInit(0.U.asTypeOf(new HLSUPtr))
  val issuePtr     = RegInit(0.U.asTypeOf(new HLSUPtr))
  val deqPtr       = RegInit(0.U.asTypeOf(new HLSUPtr))
  val respPtr      = WireInit(0.U.asTypeOf(new HLSUPtr))
  val ldstUopQueue = RegInit(VecInit(Seq.fill(nHLsuQueueEntries)(0.U.asTypeOf(new LdstUop))))

  // xcpt info
  val xcptVlReg    = RegInit(0.U(bVL.W))
  val xcptAddrReg  = RegInit(0.U(addrWidth.W))
  val hellaXcptReg = RegInit(0.U.asTypeOf(new HellaCacheExceptions))

  // val hasXcptHappened
  // assertion
  // exception only once
  val addrMisalign = WireInit(false.B)
  val memXcpt      = io.dataExchange.xcpt.asUInt.orR
  val hasXcpt      = RegInit(false.B)

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

  io.lsuReq.ready := (uopState === uop_idle) && !isFull(hlsuInfoEnqPtr, hlsuInfoDeqPtr)
  val hlsuEnqInfo = hlsuInfoQueue(hlsuInfoEnqPtr.value)
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
        nextUopState := uop_split_finish
      }.otherwise {
        nextUopState := uop_split
      }
    }
    is(uop_split_finish) {
      when(!s1_isValidAddr) {
        nextUopState   := uop_idle
      }.otherwise {
        nextUopState := uop_split_finish
      }
    }
  }
  // SPLIT FSM -- transition
  uopState := nextUopState

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

  val memVl = leftLen min ldstCtrl.mlen

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
    // set vreg
    when(io.lsuReq.bits.muopInfo.destVRegStart) {
      (0 until vlenb).foreach { i =>
        // vregInfoDataVec(i) := mUopInfo.old_vd(8 * i + 7, 8 * i)
        hlsuEnqInfo.vregDataVec(i) := mUopInfo.old_vd(8 * i + 7, 8 * i)
      }
      hlsuEnqInfo.valid    := true.B
      hlsuEnqInfo.ldstCtrl := ldstCtrl
      hlsuEnqInfo.muopInfo := mUopInfo
    }
  }

  hlsuInfoEnqPtr := Mux(
    (uopState === uop_split_finish) && !s1_isValidAddr && hlsuEnqInfo.muopInfo.destVRegEnd, 
    hlsuInfoEnqPtr + 1.U, 
    hlsuInfoEnqPtr
  )

  // * BEGIN
  // * Calculate Addr
  // pipeline stage 0 --> calc addr
  val addr     = WireInit(0.U(addrWidth.W))
  val offset   = WireInit(0.U(log2Ceil(dataBytes).W))
  val baseAddr = hlsuEnqInfo.muopInfo.rs1Val

  val curVl       = (hlsuEnqInfo.muopInfo.uopIdx << hlsuEnqInfo.ldstCtrl.log2MinLen) + curSplitIdx
  val isValidAddr = uopState === uop_split && !hasXcpt && (curSplitIdx < splitCount)

  // indexed addr
  val idxVal  = WireInit(0.U(XLEN.W))
  val idxMask = WireInit(0.U(XLEN.W))
  val eew     = hlsuEnqInfo.ldstCtrl.eewb << 3.U
  val beginIdx =
    (curVl - ((curVl >> hlsuEnqInfo.ldstCtrl.log2Elen) << hlsuEnqInfo.ldstCtrl.log2Elen)) << (hlsuEnqInfo.ldstCtrl.log2Eewb +& 3.U) // 3.U: to bits
  idxMask := (("h1".asUInt(addrWidth.W) << eew) - 1.U)
  idxVal  := (hlsuEnqInfo.muopInfo.vs2 >> beginIdx) & idxMask

  val stride    = WireInit(0.S(XLEN.W))
  val negStride = stride < 0.S
  val strideAbs = Mux(negStride, (-stride).asUInt, stride.asUInt)

  when(hlsuEnqInfo.ldstCtrl.ldstType === Mop.unit_stride) {
    stride := (hlsuEnqInfo.ldstCtrl.nfield << hlsuEnqInfo.ldstCtrl.log2Memwb).asSInt
  }.elsewhen(hlsuEnqInfo.ldstCtrl.ldstType === Mop.constant_stride) {
    stride := hlsuEnqInfo.muopInfo.rs2Val.asSInt
  }.otherwise {
    stride := 11111.S
  }

  val accelerateStride = Cat(strideAbs === 8.U, strideAbs === 4.U, strideAbs === 2.U, strideAbs === 1.U)
  val canAccelerate =
    (accelerateStride =/= 0.U || strideAbs === 0.U) && ~AddrUtil.isAddrMisalign(strideAbs, hlsuEnqInfo.ldstCtrl.log2Memwb)
  val log2Stride = Mux(canAccelerate, Mux1H(accelerateStride, Seq(0.U, 1.U, 2.U, 3.U)), 0.U)

  val curStridedAddr = Mux(
    curSplitIdx === 0.U && hlsuEnqInfo.muopInfo.uopIdx === 0.U,
    addrReg,
    (addrReg.asSInt + stride.asSInt).asUInt,
  )

  when(canAccelerate) {
    addr := addrReg
  }.otherwise {
    addr := Mux1H(
      hlsuEnqInfo.ldstCtrl.ldstType,
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

  val startElemPos   = curVl - ((curVl >> hlsuEnqInfo.ldstCtrl.log2Mlen) << hlsuEnqInfo.ldstCtrl.log2Mlen)
  val startVRegIdx   = startElemPos << hlsuEnqInfo.ldstCtrl.log2Memwb
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
  val endElemPos = startElemPos + canLoadElemCnt
  val endVRegIdx = Mux(canAccelerate, endElemPos << hlsuEnqInfo.ldstCtrl.log2Memwb, startVRegIdx)

  when(infoQueue.io.enq.fire) {
    curSplitIdx := curSplitIdx + canLoadElemCnt
    addrReg := Mux(
      canAccelerate && strideAbs =/= 0.U,
      Mux(negStride, addr - (canLoadElemCnt << log2Stride), addr + (canLoadElemCnt << log2Stride)),
      addr,
    )
  }

  // * Calculate Addr
  // * END

  infoQueue.io.enq.valid               := isValidAddr
  infoQueue.io.enq.bits.strideAbs      := strideAbs
  infoQueue.io.enq.bits.negStride      := negStride
  infoQueue.io.enq.bits.log2Stride     := log2Stride
  infoQueue.io.enq.bits.addr           := addr
  infoQueue.io.enq.bits.startElemPos   := startElemPos
  infoQueue.io.enq.bits.endElemPos     := endElemPos
  infoQueue.io.enq.bits.startVRegIdx   := startVRegIdx
  infoQueue.io.enq.bits.endVRegIdx     := endVRegIdx
  infoQueue.io.enq.bits.canLoadElemCnt := canLoadElemCnt
  infoQueue.io.enq.bits.curSplitIdx    := curSplitIdx
  infoQueue.io.enq.bits.curVl          := curVl

  // pipeline stage 1
  val ldstQueueFull = isFull(enqPtr, deqPtr)

  val deqSplitInfoValid = infoQueue.io.deq.valid
  val deqSplitInfo      = infoQueue.io.deq.bits

  infoQueue.io.deq.ready := !ldstQueueFull

  s1_isValidAddr := infoQueue.io.deq.fire

  val s1_addrMisalign = AddrUtil.isAddrMisalign(deqSplitInfo.addr, hlsuEnqInfo.ldstCtrl.log2Memwb)
  val s1_alignedAddr  = AddrUtil.getAlignedAddr(deqSplitInfo.addr)
  val s1_offset       = AddrUtil.getAlignedOffset(deqSplitInfo.addr)

  val elemMaskVec = VecInit(Seq.fill(vlenb)(false.B))
  val isNotMasked = elemMaskVec.asUInt =/= 0.U

  val misalignXcpt = 0.U.asTypeOf(new LdstXcpt)
  misalignXcpt.xcptValid := s1_addrMisalign
  misalignXcpt.ma        := s1_addrMisalign

  canEnqueue := (hlsuEnqInfo.ldstCtrl.vm || isNotMasked) && (deqSplitInfo.curSplitIdx + deqSplitInfo.canLoadElemCnt >= splitStart)

  when(infoQueue.io.deq.fire) {
    ldstUopQueue(enqPtr.value).valid       := canEnqueue
    ldstUopQueue(enqPtr.value).status      := Mux(s1_addrMisalign, LdstUopStatus.ready, LdstUopStatus.notReady)
    ldstUopQueue(enqPtr.value).memOp       := hlsuEnqInfo.ldstCtrl.isStore
    ldstUopQueue(enqPtr.value).addr        := deqSplitInfo.addr
    ldstUopQueue(enqPtr.value).size        := hlsuEnqInfo.ldstCtrl.log2Memwb
    ldstUopQueue(enqPtr.value).pos         := deqSplitInfo.curVl
    ldstUopQueue(enqPtr.value).xcpt        := misalignXcpt
    ldstUopQueue(enqPtr.value).zeroStride  := deqSplitInfo.strideAbs === 0.U
    ldstUopQueue(enqPtr.value).negStride   := deqSplitInfo.negStride
    ldstUopQueue(enqPtr.value).log2Stride  := deqSplitInfo.log2Stride
    ldstUopQueue(enqPtr.value).elemCnt     := deqSplitInfo.canLoadElemCnt
    ldstUopQueue(enqPtr.value).hlsuInfoPtr := hlsuInfoEnqPtr.value
    // FIXME
    ldstUopQueue(enqPtr.value).destElem :=
      deqSplitInfo.curVl - ((deqSplitInfo.curVl >> hlsuEnqInfo.ldstCtrl.log2Mlen) << hlsuEnqInfo.ldstCtrl.log2Mlen)
    ldstUopQueue(enqPtr.value).destVRegEnd :=
      (deqSplitInfo.curSplitIdx + deqSplitInfo.canLoadElemCnt >= splitCount) && hlsuEnqInfo.muopInfo.destVRegEnd

    enqPtr := Mux(canEnqueue, enqPtr + 1.U, enqPtr)

    (0 until vlenb).foreach { i =>
      val curElemPos = i.U >> hlsuEnqInfo.ldstCtrl.log2Memwb
      val belong     = curElemPos >= deqSplitInfo.startElemPos && curElemPos < deqSplitInfo.endElemPos
      val maskIdx    = curElemPos - deqSplitInfo.startElemPos + deqSplitInfo.curVl

      elemMaskVec(i) := Mux(
        belong,
        (hlsuEnqInfo.ldstCtrl.vm || hlsuEnqInfo.muopInfo.mask(maskIdx)) && (curElemPos >= splitStart) && (curElemPos < splitCount),
        false.B,
      )

      val maskCond = elemMaskVec(i) && !s1_addrMisalign

      when(belong) {
        // vregInfoValid(i) := maskCond
        hlsuEnqInfo.vregDataValid(i) := maskCond
      }
    }
  }
  // * Split LdstUop
  // * END

  // * BEGIN
  // * Issue LdstUop
  val hlsuDeqInfo = hlsuInfoQueue(hlsuInfoDeqPtr.value)

  val issueUop = ldstUopQueue(issuePtr.value)
  val isNoXcptUop = issueUop.valid & (~issueUop.xcpt.xcptValid) &&
    !isFull(issuePtr, deqPtr) &&
    (issueUop.hlsuInfoPtr === hlsuInfoDeqPtr.value)

  respPtr.value := io.dataExchange.resp.bits.idx(nLSUMaxQueueWidth - 1, 0)
  respPtr.flag  := io.dataExchange.resp.bits.flag
  val respData = io.dataExchange.resp.bits.data

  // non store waiting code
  when(io.dataExchange.resp.bits.nack && (respPtr < issuePtr)) {
    issuePtr := respPtr
  }.elsewhen(isNoXcptUop && io.dataExchange.req.ready) {
    issuePtr := issuePtr + 1.U
  }

  val storeDataVec = VecInit(Seq.fill(dataBytes)(0.U(8.W)))
  val storeMaskVec = VecInit(Seq.fill(dataBytes)(0.U(1.W)))

  (0 until vlenb).foreach { i =>
    val curElemPos = i.U >> hlsuDeqInfo.ldstCtrl.log2Memwb
    val belong     = (curElemPos >= issueUop.destElem) && (curElemPos < (issueUop.destElem + issueUop.elemCnt))
    when(belong) {
      val elemInnerOffset =
        AddrUtil.getAlignedOffset(issueUop.addr) + (i.U - (curElemPos << hlsuDeqInfo.ldstCtrl.log2Memwb))
      val curElemOffset = Mux(
        issueUop.zeroStride,
        0.U,
        Mux(
          issueUop.negStride,
          -(curElemPos - issueUop.destElem) << issueUop.log2Stride,
          (curElemPos - issueUop.destElem) << issueUop.log2Stride,
        ),
      )

      storeDataVec(elemInnerOffset + curElemOffset) := hlsuDeqInfo.vregDataVec(i)
      storeMaskVec(elemInnerOffset + curElemOffset) := hlsuDeqInfo.vregDataValid(i)
    }
  }
  
  io.dataExchange.req.valid      := isNoXcptUop
  io.dataExchange.req.bits.addr  := AddrUtil.getAlignedAddr(issueUop.addr)
  io.dataExchange.req.bits.cmd   := issueUop.memOp
  io.dataExchange.req.bits.srcId := 0.U
  io.dataExchange.req.bits.flag  := issuePtr.flag
  io.dataExchange.req.bits.idx   := issuePtr.value
  io.dataExchange.req.bits.data  := storeDataVec.asUInt
  io.dataExchange.req.bits.mask  := storeMaskVec.asUInt

  // * Issue LdstUop
  // * END

  // * BEGIN
  // * Recv Resp

  val respUop     = ldstUopQueue(respPtr.value)
  val respDataVec = VecInit(Seq.fill(dataBytes)(0.U(8.W)))
  (0 until dataBytes).foreach { i =>
    respDataVec(i) := respData(8 * i + 7, 8 * i)
  }

  when(io.dataExchange.resp.valid || memXcpt) {
    respUop.status         := LdstUopStatus.ready
    respUop.xcpt.xcptValid := memXcpt
    respUop.xcpt.fromHellaXcpt(io.dataExchange.xcpt)
  }

  when(io.dataExchange.resp.valid && respUop.memOp === VMemCmd.read) {
    (0 until vlenb).foreach { i =>
      val curElemPos = i.U >> hlsuDeqInfo.ldstCtrl.log2Memwb
      val belong     = (curElemPos >= respUop.destElem) && (curElemPos < (respUop.destElem + respUop.elemCnt))
      when(belong) {
        val elemInnerOffset =
          AddrUtil.getAlignedOffset(respUop.addr) + (i.U - (curElemPos << hlsuDeqInfo.ldstCtrl.log2Memwb))
        val curElemOffset = Mux(
          respUop.zeroStride,
          0.U,
          Mux(
            respUop.negStride,
            -(curElemPos - respUop.destElem) << respUop.log2Stride,
            (curElemPos - respUop.destElem) << respUop.log2Stride,
          ),
        )

        hlsuDeqInfo.vregDataVec(i) := Mux(
          hlsuDeqInfo.vregDataValid(i), 
          respDataVec(elemInnerOffset + curElemOffset), 
          hlsuDeqInfo.vregDataVec(i)
        )
      }
    }
  }

  completeLdst := ldstUopQueue.forall(uop => uop.valid === false.B) // ld completed or xcpt happened

  // * Recv Resp
  // * END

  // * BEGIN
  // * Commit to VRegInfo

  val dequeUop = ldstUopQueue(deqPtr.value)
  val canDeque = dequeUop.valid && dequeUop.status === LdstUopStatus.ready
  val deqXcpt  = canDeque && dequeUop.xcpt.xcptValid

  val vregCanCommit = RegInit(false.B)

  when(deqXcpt) {
    // update xcpt info
    xcptVlReg    := dequeUop.pos
    xcptAddrReg  := dequeUop.addr
    hellaXcptReg := dequeUop.xcpt.generateHellaXcpt(dequeUop.memOp)
    hasXcpt      := true.B
  }.elsewhen(canDeque) {
    dequeUop.valid := false.B
    vregCanCommit  := dequeUop.destVRegEnd
    deqPtr         := deqPtr + 1.U
  }

  // * Commit to VRegIngo
  // * END

  // * BEGIN
  // * Writeback to uopQueue
  val vregWbReady = (splitCount === 0.U && RegNext(io.lsuReq.fire)) || vregCanCommit
  val wbValid     = vregWbReady || hasXcpt
  val fofValid    = hlsuDeqInfo.ldstCtrl.unitSMop === UnitStrideMop.fault_only_first && xcptVlReg > 0.U

  io.lsuOut.valid                   := wbValid
  io.lsuOut.bits.muopEnd            := hlsuDeqInfo.muopInfo.muopEnd
  io.lsuOut.bits.rfWriteEn          := hlsuDeqInfo.muopInfo.rfWriteEn
  io.lsuOut.bits.rfWriteIdx         := hlsuDeqInfo.muopInfo.ldest
  io.lsuOut.bits.rfWriteMask        := "hffff".U
  io.lsuOut.bits.regCount           := 1.U
  io.lsuOut.bits.regStartIdx        := hlsuDeqInfo.muopInfo.ldest
  io.lsuOut.bits.isSegLoad          := false.B
  io.lsuOut.bits.data               := Cat(hlsuDeqInfo.vregDataVec.reverseMap(entry => entry))
  io.lsuOut.bits.xcpt.exception_vld := hasXcpt & ~fofValid
  io.lsuOut.bits.xcpt.xcpt_cause    := Mux(fofValid, 0.U.asTypeOf(new HellaCacheExceptions), hellaXcptReg)
  io.lsuOut.bits.xcpt.xcpt_addr     := xcptAddrReg
  io.lsuOut.bits.xcpt.update_vl     := hasXcpt & fofValid
  io.lsuOut.bits.xcpt.update_data   := xcptVlReg

  when(vregWbReady) {
    hlsuDeqInfo.valid := false.B
    hlsuDeqInfo.vregDataValid.foreach(_ := false.B)
    hlsuInfoDeqPtr := hlsuInfoDeqPtr + 1.U
  }.elsewhen(hasXcpt) {
    hlsuInfoQueue.foreach(info => info.valid := false.B)
    hlsuInfoQueue.foreach(info => info.vregDataValid.foreach(_ := false.B))
    hlsuInfoEnqPtr := 0.U.asTypeOf(new HLSUInfoPtr)
    hlsuInfoDeqPtr := 0.U.asTypeOf(new HLSUInfoPtr)
  }

  when(wbValid) {
    vregCanCommit := false.B
  }

  when(hasXcpt) {
    // clear ldstUopQueue
    ldstUopQueue.foreach(uop =>
      uop.valid := false.B
    )
    // reset xcpt
    hasXcpt := false.B

    issuePtr := 0.U.asTypeOf(new HLSUPtr)
    deqPtr   := 0.U.asTypeOf(new HLSUPtr)
    enqPtr   := 0.U.asTypeOf(new HLSUPtr)
  }

  // * Writeback to uopQueue
  // * END
}

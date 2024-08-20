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

// horizontal LSU
class SVHLsu(
    implicit p: Parameters
) extends Module {
  val io = IO(new LdstIO())

  // split fsm states
  val uop_idle :: uop_split :: uop_split_finish :: Nil = Enum(3)
  val uopState                                         = RegInit(uop_idle)
  val nextUopState                                     = WireInit(uop_idle)
  val completeLdst                                     = WireInit(false.B)
  val stopSplit                                        = WireInit(false.B)

  // address reg
  val s1_isValidAddr = Reg(Bool())
  val addrReg        = RegInit(0.U(addrWidth.W))

  // uop & control related
  val mUopInfoReg = RegInit(0.U.asTypeOf(new mUopInfo))
  val ldstCtrlReg = RegInit(0.U.asTypeOf(new LSULdstCtrl))

  val vregInfoStatusVec = RegInit(VecInit(Seq.fill(vlenb)(VRegSegmentStatus.invalid)))
  val vregInfoIdxVec    = RegInit(VecInit(Seq.fill(vlenb)(ldstUopQueueSize.U)))
  val vregInfoOffsetVec = RegInit(VecInit(Seq.fill(vlenb)(0.U(log2Ceil(dataBytes).W))))
  val vregInfoDataVec   = RegInit(VecInit(Seq.fill(vlenb)(0.U(8.W))))

  // Split info
  val vstartGeVl  = RegInit(false.B)
  val splitCount  = RegInit(0.U(vlenbWidth.W))
  val curSplitIdx = RegInit(0.U(vlenbWidth.W))
  val splitStart  = RegInit(0.U(vlenbWidth.W))

  // ldQueue
  val canEnqueue   = WireInit(false.B)
  val ldstEnqPtr   = RegInit(0.U(ldstUopQueueWidth.W))
  val issueLdstPtr = RegInit(0.U(ldstUopQueueWidth.W))
  val commitPtr    = RegInit(0.U(ldstUopQueueWidth.W))
  val ldstUopQueue = RegInit(VecInit(Seq.fill(ldstUopQueueSize)(0.U.asTypeOf(new LdstUop))))

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
  val (vstart, vl)     = (io.mUop.bits.uop.info.vstart, io.mUop.bits.uop.info.vl)
  val (uopIdx, uopEnd) = (io.mUop.bits.uop.uopIdx, io.mUop.bits.uop.uopEnd)

  val ldstCtrl = LSULdstDecoder(io.mUop.bits, io.mUopMergeAttr.bits)
  val mUopInfo = mUopInfoSelecter(io.mUop.bits, io.mUopMergeAttr.bits)

  io.lsuReady := Mux(uopState === uop_idle, true.B, false.B)
  // SPLIT FSM -- decide next state
  when(uopState === uop_idle) {
    when(io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst && ldstCtrl.nfield === 1.U) { // not segment
      nextUopState := uop_split
    }.otherwise {
      nextUopState := uop_idle
    }
  }.elsewhen(uopState === uop_split) {
    when(stopSplit) {
      nextUopState := uop_split_finish
    }.otherwise {
      nextUopState := uop_split
    }
  }.elsewhen(uopState === uop_split_finish) {
    when(completeLdst && !s1_isValidAddr) {
      nextUopState := uop_idle
    }.otherwise {
      nextUopState := uop_split_finish
    }
  }.otherwise {
    nextUopState := uop_idle
  }
  // SPLIT FSM -- transition
  uopState := nextUopState

  // if exception occurs or split finished, stop split
  stopSplit := hasXcpt || (curSplitIdx + 1.U >= splitCount) || (splitCount === 0.U)

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

  // val vregClean   = vregInfo.forall(info => info.status === VRegSegmentStatus.invalid)
  val vregClean = ParallelOR(vregInfoStatusVec) === 0.U

  when(uopState === uop_idle) {
    when(io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst && ldstCtrl.nfield === 1.U) {
      mUopInfoReg := mUopInfo
      ldstCtrlReg := ldstCtrl
      addrReg := Mux(
        io.mUop.bits.uop.uopIdx === 0.U,
        io.mUop.bits.scalar_opnd_1 + (mUopInfo.segIdx << ldstCtrl.log2Memwb),
        addrReg,
      )
      // Set split info
      ldstEnqPtr   := 0.U
      issueLdstPtr := 0.U
      commitPtr    := 0.U
      curSplitIdx  := 0.U
      splitCount   := microVl
      splitStart   := microVstart
      vstartGeVl   := vstart > actualVl
      // set vreg
      when(vregClean) {
        (0 until vlenb).foreach { i =>
          val pos = i.U >> ldstCtrl.log2Memwb
          vregInfoDataVec(i)   := io.mUop.bits.uopRegInfo.old_vd(8 * i + 7, 8 * i)
          vregInfoStatusVec(i) := Mux(pos < memVl, VRegSegmentStatus.needLdst, VRegSegmentStatus.srcData)
        }
      }
    }
  }

  // * BEGIN
  // * Calculate Addr

  // pipeline stage 0 --> calc addr
  val addr     = WireInit(0.U(addrWidth.W))
  val offset   = WireInit(0.U(log2Ceil(dataBytes).W))
  val baseAddr = mUopInfoReg.rs1Val

  val curVl       = (mUopInfoReg.uopIdx << ldstCtrlReg.log2MinLen) + curSplitIdx
  val isValidAddr = uopState === uop_split && !hasXcpt && curSplitIdx < splitCount

  // indexed addr
  val idxVal  = WireInit(0.U(XLEN.W))
  val idxMask = WireInit(0.U(XLEN.W))
  val eew     = ldstCtrlReg.eewb << 3.U
  val beginIdx =
    (curVl - ((curVl >> ldstCtrlReg.log2Elen) << ldstCtrlReg.log2Elen)) << (ldstCtrlReg.log2Eewb +& 3.U) // 3.U: to bits
  idxMask := (("h1".asUInt(addrWidth.W) << eew) - 1.U)
  idxVal  := (mUopInfoReg.vs2 >> beginIdx) & idxMask

  val stride    = WireInit(0.S(XLEN.W))
  val negStride = stride < 0.S
  val strideAbs = Mux(negStride, (-stride).asUInt, stride.asUInt)

  when(ldstCtrlReg.ldstType === Mop.unit_stride) {
    stride := (ldstCtrlReg.nfield << ldstCtrlReg.log2Memwb).asSInt
  }.elsewhen(ldstCtrlReg.ldstType === Mop.constant_stride) {
    stride := mUopInfoReg.rs2Val.asSInt
  }.otherwise {
    stride := 11111.S
  }

  val accelerateStride = Cat(strideAbs === 8.U, strideAbs === 4.U, strideAbs === 2.U, strideAbs === 1.U)
  val canAccelerate =
    (accelerateStride =/= 0.U || strideAbs === 0.U) && ~AddrUtil.isAddrMisalign(strideAbs, ldstCtrlReg.log2Memwb)
  val log2Stride = Mux(canAccelerate, Mux1H(accelerateStride, Seq(0.U, 1.U, 2.U, 3.U)), 0.U)

  val curStridedAddr = Mux(
    curSplitIdx === 0.U && mUopInfoReg.uopIdx === 0.U,
    addrReg,
    (addrReg.asSInt + stride.asSInt).asUInt,
  )

  when(canAccelerate) {
    addr := addrReg
  }.otherwise {
    addr := Mux1H(
      ldstCtrlReg.ldstType,
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

  val startElemPos   = curVl - ((curVl >> ldstCtrlReg.log2Mlen) << ldstCtrlReg.log2Mlen)
  val startVRegIdx   = startElemPos << ldstCtrlReg.log2Memwb
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
  val endVRegIdx = Mux(canAccelerate, endElemPos << ldstCtrlReg.log2Memwb, startVRegIdx)

  when(isValidAddr) {
    curSplitIdx := curSplitIdx + canLoadElemCnt
    addrReg := Mux(
      canAccelerate && strideAbs =/= 0.U,
      Mux(negStride, addr - (canLoadElemCnt << log2Stride), addr + (canLoadElemCnt << log2Stride)),
      addr,
    )
  }

  // * Calculate Addr
  // * END

  // pipeline stage 1

  s1_isValidAddr := isValidAddr
  val s1_strideAbs      = RegEnable(strideAbs, isValidAddr)
  val s1_negStride      = RegEnable(negStride, isValidAddr)
  val s1_log2Stride     = RegEnable(log2Stride, isValidAddr)
  val s1_addr           = RegEnable(addr, isValidAddr)
  val s1_startElemPos   = RegEnable(startElemPos, isValidAddr)
  val s1_endElemPos     = RegEnable(endElemPos, isValidAddr)
  val s1_startVRegIdx   = RegEnable(startVRegIdx, isValidAddr)
  val s1_endVRegIdx     = RegEnable(endVRegIdx, isValidAddr)
  val s1_canLoadElemCnt = RegEnable(canLoadElemCnt, isValidAddr)
  val s1_curSplitIdx    = RegEnable(curSplitIdx, isValidAddr)
  val s1_curVl          = RegEnable(curVl, isValidAddr)

  val s1_addrMisalign = AddrUtil.isAddrMisalign(s1_addr, ldstCtrlReg.log2Memwb)
  val s1_alignedAddr  = AddrUtil.getAlignedAddr(s1_addr)
  val s1_offset       = AddrUtil.getAlignedOffset(s1_addr)

  val elemMaskVec = VecInit(Seq.fill(vlenb)(false.B))
  val isNotMasked = elemMaskVec.asUInt =/= 0.U

  val misalignXcpt = 0.U.asTypeOf(new LdstXcpt)
  misalignXcpt.xcptValid := s1_addrMisalign
  misalignXcpt.ma        := s1_addrMisalign

  when(s1_isValidAddr) {
    canEnqueue := (ldstCtrlReg.vm || isNotMasked) && (s1_curSplitIdx + s1_canLoadElemCnt >= splitStart)

    ldstUopQueue(ldstEnqPtr).valid  := canEnqueue
    ldstUopQueue(ldstEnqPtr).status := Mux(s1_addrMisalign, LdstUopStatus.ready, LdstUopStatus.notReady)
    ldstUopQueue(ldstEnqPtr).memOp  := ldstCtrlReg.isStore
    // NOTE: for misalign xcpt, put addr into ldstQueue, output directly to xcpt_addr
    ldstUopQueue(ldstEnqPtr).addr := Mux(s1_addrMisalign, s1_addr, s1_alignedAddr)
    ldstUopQueue(ldstEnqPtr).pos  := s1_curVl
    ldstUopQueue(ldstEnqPtr).xcpt := misalignXcpt
    ldstEnqPtr                    := Mux(canEnqueue, ldstEnqPtr + 1.U, ldstEnqPtr)

    (0 until vlenb).foreach { i =>
      val curElemPos = i.U >> ldstCtrlReg.log2Memwb
      val belong     = curElemPos >= s1_startElemPos && curElemPos < s1_endElemPos
      val maskIdx    = curElemPos - s1_startElemPos + s1_curVl

      elemMaskVec(i) := Mux(
        belong,
        (ldstCtrlReg.vm || mUopInfoReg.mask(maskIdx)) && (curElemPos >= splitStart),
        false.B,
      )

      val maskCond = elemMaskVec(i) && !s1_addrMisalign

      when(belong) {
        vregInfoStatusVec(i) := Mux(maskCond, VRegSegmentStatus.notReady, VRegSegmentStatus.srcData)
        vregInfoIdxVec(i)    := Mux(maskCond, ldstEnqPtr, vregInfoIdxVec(i))

        val elemInnerOffset = s1_offset + (i.U - (curElemPos << ldstCtrlReg.log2Memwb)) // which byte inside the element
        val curElemOffset = Mux(
          s1_strideAbs === 0.U,
          0.U,
          Mux(negStride, -(curElemPos - s1_startElemPos) << log2Stride, (curElemPos - s1_startElemPos) << log2Stride),
        ) // which element

        vregInfoOffsetVec(i) := Mux(maskCond, elemInnerOffset + curElemOffset, vregInfoOffsetVec(i))
      }
    }
  }
  // * Split LdstUop
  // * END

  // * BEGIN
  // * Issue LdstUop

  val issueUop    = ldstUopQueue(issueLdstPtr)
  val isNoXcptUop = issueUop.valid & (~issueUop.xcpt.xcptValid)

  // non store waiting code
  when(io.dataExchange.resp.bits.nack && io.dataExchange.resp.bits.idx <= issueLdstPtr) {
    issueLdstPtr := io.dataExchange.resp.bits.idx
  }.elsewhen(isNoXcptUop && io.dataExchange.req.ready) {
    issueLdstPtr := issueLdstPtr + 1.U
  }

  val storeDataVec = VecInit(Seq.fill(dataBytes)(0.U(8.W)))
  val storeMaskVec = VecInit(Seq.fill(dataBytes)(0.U(1.W)))

  (0 until vlenb).foreach { i =>
    when(
      ldstCtrlReg.isStore && vregInfoStatusVec(i) === VRegSegmentStatus.notReady && vregInfoIdxVec(i) === issueLdstPtr
    ) {
      val offset = vregInfoOffsetVec(i)
      storeDataVec(offset) := vregInfoDataVec(i)
      storeMaskVec(offset) := 1.U
    }
  }

  io.dataExchange.req.valid     := isNoXcptUop
  io.dataExchange.req.bits.addr := issueUop.addr
  io.dataExchange.req.bits.cmd  := issueUop.memOp
  io.dataExchange.req.bits.idx  := issueLdstPtr
  io.dataExchange.req.bits.data := storeDataVec.asUInt
  io.dataExchange.req.bits.mask := storeMaskVec.asUInt

  // * Issue LdstUop
  // * END

  // * BEGIN
  // * Recv Resp
  val (respLdstPtr, respData) = (io.dataExchange.resp.bits.idx, io.dataExchange.resp.bits.data)

  val respDataVec = VecInit(Seq.fill(dataBytes)(0.U(8.W)))
  (0 until dataBytes).foreach { i =>
    respDataVec(i) := respData(8 * i + 7, 8 * i)
  }

  when(io.dataExchange.resp.valid || memXcpt) {
    ldstUopQueue(respLdstPtr).status         := LdstUopStatus.ready
    ldstUopQueue(respLdstPtr).xcpt.xcptValid := memXcpt
    ldstUopQueue(respLdstPtr).xcpt.fromHellaXcpt(io.dataExchange.xcpt)
  }

  when(io.dataExchange.resp.valid) {
    when(ldstUopQueue(respLdstPtr).memOp === VMemCmd.read) {
      (0 until vlenb).foreach { i =>
        when(vregInfoStatusVec(i) === VRegSegmentStatus.notReady && vregInfoIdxVec(i) === respLdstPtr) {
          val offsetOH = UIntToOH(vregInfoOffsetVec(i), dataBytes)
          vregInfoDataVec(i) := Mux1H(offsetOH, respDataVec)
        }
      }
    }
  }

  completeLdst := ldstUopQueue.forall(uop => uop.valid === false.B) // ld completed or xcpt happened

  // * Recv Resp
  // * END

  // * BEGIN
  // * Commit to VRegIngo
  val canCommit  = ldstUopQueue(commitPtr).valid && ldstUopQueue(commitPtr).status === LdstUopStatus.ready
  val commitXcpt = canCommit && ldstUopQueue(commitPtr).xcpt.xcptValid

  when(commitXcpt) {
    (0 until vlenb).foreach { i =>
      when(vregInfoIdxVec(i) >= commitPtr && vregInfoIdxVec(i) < ldstUopQueueSize.U) {
        vregInfoStatusVec(i) := VRegSegmentStatus.xcpt
      }
    }
    // update xcpt info
    xcptVlReg   := ldstUopQueue(commitPtr).pos
    xcptAddrReg := ldstUopQueue(commitPtr).addr

    val commitUop = ldstUopQueue(commitPtr)
    hellaXcptReg := commitUop.xcpt.generateHellaXcpt(commitUop.memOp)

    hasXcpt := true.B
  }.elsewhen(canCommit) {
    ldstUopQueue(commitPtr).valid := false.B

    (0 until vlenb).foreach { i =>
      when(vregInfoStatusVec(i) === VRegSegmentStatus.notReady && vregInfoIdxVec(i) === commitPtr) {
        vregInfoStatusVec(i) := VRegSegmentStatus.ready
        vregInfoIdxVec(i)    := ldstUopQueueSize.U
      }
    }
    commitPtr := commitPtr + 1.U
  }

  // * Commit to VRegIngo
  // * END

  // * BEGIN
  // * Writeback to uopQueue
  val allReadyOrSrcData =
    vregInfoStatusVec.forall(status => status === VRegSegmentStatus.ready || status === VRegSegmentStatus.srcData)
  val vregWbReady = (splitCount === 0.U || completeLdst) && allReadyOrSrcData
  val wbValid     = vregWbReady || hasXcpt
  val fofValid    = ldstCtrlReg.unitSMop === UnitStrideMop.fault_only_first && xcptVlReg > 0.U

  io.lsuOut.valid                   := wbValid
  io.lsuOut.bits.muopEnd            := mUopInfoReg.muopEnd
  io.lsuOut.bits.rfWriteEn          := mUopInfoReg.rfWriteEn
  io.lsuOut.bits.rfWriteIdx         := mUopInfoReg.ldest
  io.lsuOut.bits.rfWriteMask        := Cat(vregInfoStatusVec.reverseMap(_ =/= VRegSegmentStatus.ready)).asUInt
  io.lsuOut.bits.regCount           := 1.U
  io.lsuOut.bits.regStartIdx        := mUopInfoReg.ldest
  io.lsuOut.bits.isSegLoad          := false.B
  io.lsuOut.bits.data               := Cat(vregInfoDataVec.reverseMap(entry => entry))
  io.lsuOut.bits.xcpt.exception_vld := hasXcpt & ~fofValid
  io.lsuOut.bits.xcpt.xcpt_cause    := Mux(fofValid, 0.U.asTypeOf(new HellaCacheExceptions), hellaXcptReg)
  io.lsuOut.bits.xcpt.xcpt_addr     := xcptAddrReg
  io.lsuOut.bits.xcpt.update_vl     := hasXcpt & fofValid
  io.lsuOut.bits.xcpt.update_data   := xcptVlReg

  when(wbValid) {
    // Reset vreg info
    for (i <- 0 until vlenb) {
      vregInfoStatusVec(i) := VRegSegmentStatus.invalid
      vregInfoIdxVec(i)    := ldstUopQueueSize.U
      vregInfoOffsetVec(i) := DontCare
      vregInfoDataVec(i)   := DontCare
    }

  }

  when(hasXcpt) {
    // clear ldstUopQueue
    ldstUopQueue.foreach(uop => uop.valid := false.B)
    // reset xcpt
    hasXcpt := false.B
  }

  // * Writeback to uopQueue
  // * END
}

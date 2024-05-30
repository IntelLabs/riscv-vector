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
class SVHLsu(implicit p: Parameters) extends Module with HasCircularQueuePtrHelper {
    val io = IO(new LdstIO())
    // split fsm states
    val uop_idle :: uop_split :: uop_split_finish :: Nil = Enum(3)
    val uopState        = RegInit(uop_idle)
    val nextUopState    = WireInit(uop_idle)
    val completeLdst    = WireInit(false.B)
    val stopSplit       = WireInit(false.B)

    // address reg
    val addrReg         = RegInit(0.U(addrWidth.W))

    // uop & control related
    val mUopInfoReg     = RegInit(0.U.asTypeOf(new mUopInfo))
    val ldstCtrlReg     = RegInit(0.U.asTypeOf(new LSULdstCtrl))

    // vreg seg info
    val defaultPtr      = 0.U.asTypeOf(new HLSUPtr)
    defaultPtr.value   := ldstUopQueueSize.U
    val vregInitVal     = Wire(new VRegSegmentInfo)
    vregInitVal.status := VRegSegmentStatus.invalid
    vregInitVal.idx    := defaultPtr
    vregInitVal.offset := DontCare
    vregInitVal.data   := DontCare

    val vregInfo        = RegInit(VecInit(Seq.fill(vlenb)(vregInitVal)))

    // Split info
    val vstartGeVl      = RegInit(false.B)
    val splitCount      = RegInit(0.U(vlenbWidth.W))
    val curSplitIdx     = RegInit(0.U(vlenbWidth.W))
    val splitStart      = RegInit(0.U(vlenbWidth.W))

    // ldQueue
    val canEnqueue      = WireInit(false.B)
    val enqPtr          = RegInit(0.U.asTypeOf(new HLSUPtr))
    val issuePtr        = RegInit(0.U.asTypeOf(new HLSUPtr))
    val deqPtr          = RegInit(0.U.asTypeOf(new HLSUPtr))
    val respPtr         = WireInit(0.U.asTypeOf(new HLSUPtr))
    val ldstUopQueue    = RegInit(VecInit(Seq.fill(ldstUopQueueSize)(0.U.asTypeOf(new LdstUop))))

    // xcpt info
    val xcptVlReg       = RegInit(0.U(bVL.W))
    val xcptAddrReg     = RegInit(0.U(addrWidth.W))
    val hellaXcptReg    = RegInit(0.U.asTypeOf(new HellaCacheExceptions))

    // val hasXcptHappened
    // assertion
    // exception only once
    val addrMisalign    = WireInit(false.B)
    val memXcpt         = io.dataExchange.xcpt.asUInt.orR
    val hasXcpt         = RegInit(false.B)

        
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
    when (uopState === uop_idle) {
        when (io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst && ldstCtrl.nfield === 1.U) { // not segment
            nextUopState := uop_split
        }.otherwise {
            nextUopState := uop_idle
        }
    }.elsewhen (uopState === uop_split) {
        when (stopSplit) {
            nextUopState := uop_split_finish
        }.otherwise {
            nextUopState := uop_split
        }
    }.elsewhen (uopState === uop_split_finish) {
        when (completeLdst) {
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
    stopSplit := memXcpt || (uopState === uop_split && curSplitIdx < splitCount && addrMisalign && canEnqueue) ||
                (curSplitIdx + 1.U >= splitCount) || (splitCount === 0.U)


    // decide micro vl
    val actualVl    = Mux(ldstCtrl.unitSMop === UnitStrideMop.mask, (vl + 7.U) >> 3.U, vl) // ceil(vl/8)
    val doneLen     = uopIdx << ldstCtrl.log2MinLen
    val leftLen     = Mux(ldstCtrl.unitSMop === UnitStrideMop.whole_register, ldstCtrl.mlen, Mux(actualVl > doneLen, actualVl - doneLen, 0.U))
    val microVl     = ldstCtrl.minLen min leftLen
    val microVstart = Mux(vstart < doneLen, 0.U, ldstCtrl.minLen min (vstart - doneLen))

    val memVl       = leftLen min ldstCtrl.mlen
    val memVstart   = Mux(vstart < doneLen, 0.U, ldstCtrl.mlen min (vstart - doneLen))
    
    val vregClean   = vregInfo.forall(info => info.status === VRegSegmentStatus.invalid)

    when (uopState === uop_idle) {
        when (io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst && ldstCtrl.nfield === 1.U) {
            mUopInfoReg  := mUopInfo
            ldstCtrlReg  := ldstCtrl
            addrReg      := Mux(io.mUop.bits.uop.uopIdx === 0.U, io.mUop.bits.scalar_opnd_1 + (mUopInfo.segIdx << ldstCtrl.log2Memwb), addrReg)
            // Set split info
            // enqPtr      := 0.U
            // issuePtr    := 0.U
            // deqPtr      := 0.U
            curSplitIdx  := 0.U
            splitCount   := microVl  
            splitStart   := microVstart
            vstartGeVl   := vstart > actualVl
            // set vreg
            when (vregClean) {
                (0 until vlenb).foreach { i => 
                    val pos = i.U >> ldstCtrl.log2Memwb
                    vregInfo(i).data   := io.mUop.bits.uopRegInfo.old_vd(8 * i + 7, 8 * i)
                    vregInfo(i).status := Mux(pos < memVl, 
                        VRegSegmentStatus.needLdst, 
                        Mux(io.mUop.bits.uop.info.ta, VRegSegmentStatus.agnostic, VRegSegmentStatus.srcData)
                    )
                }
            }
        }
    }

    // * BEGIN
    // * Calculate Addr
    val addr        = WireInit(0.U(addrWidth.W))
    val addrMask    = WireInit(0.U(addrWidth.W))
    val alignedAddr = WireInit(0.U(addrWidth.W))
    val offset      = WireInit(0.U(log2Ceil(addrWidth / 8).W))

    val curVl       = (mUopInfoReg.uopIdx << ldstCtrlReg.log2MinLen) + curSplitIdx
    val baseAddr    = mUopInfoReg.rs1Val

    // indexed addr
    val idxVal      = WireInit(0.U(XLEN.W))
    val idxMask     = WireInit(0.U(XLEN.W))
    val eew         = ldstCtrlReg.eewb << 3.U
    val beginIdx    = (curVl - ((curVl >> ldstCtrlReg.log2Elen) << ldstCtrlReg.log2Elen)) << (ldstCtrlReg.log2Eewb +& 3.U) // 3.U: to bits
    idxMask := (("h1".asUInt(addrWidth.W) << eew) - 1.U)
    idxVal  := (mUopInfoReg.vs2 >> beginIdx) & idxMask

    val stride = WireInit(0.S(XLEN.W))
    val negStride = stride < 0.S
    val strideAbs = Mux(negStride, (-stride).asUInt, stride.asUInt)

    when (ldstCtrlReg.ldstType === Mop.unit_stride) {
        stride := (ldstCtrlReg.nfield << ldstCtrlReg.log2Memwb).asSInt
    }.elsewhen (ldstCtrlReg.ldstType === Mop.constant_stride) {
        stride := mUopInfoReg.rs2Val.asSInt
    }.otherwise {
        stride := 11111.S
    }

    val accelerateStride = Cat(strideAbs === 4.U, strideAbs === 2.U, strideAbs === 1.U)
    val canAccelerate = accelerateStride =/= 0.U || strideAbs === 0.U
    val log2Stride = Mux(canAccelerate, Mux1H(accelerateStride, Seq(0.U, 1.U, 2.U)), 0.U)

    val curStridedAddr = Mux(
        curSplitIdx === 0.U && mUopInfoReg.uopIdx === 0.U, 
        addrReg,
        Mux(negStride, addrReg - strideAbs, addrReg + strideAbs)
    )
    
    when (canAccelerate) {
        addr := addrReg
    }.otherwise {
        addr := Mux1H(ldstCtrlReg.ldstType, Seq(
            // unit stride
            curStridedAddr,
            // index_unodered
            ((mUopInfoReg.segIdx << ldstCtrlReg.log2Memwb) + (Cat(false.B, baseAddr).asSInt + idxVal.asSInt).asUInt),
            // strided
            curStridedAddr,
            // index_odered
            ((mUopInfoReg.segIdx << ldstCtrlReg.log2Memwb) + (Cat(false.B, baseAddr).asSInt + idxVal.asSInt).asUInt)
        ))
    }

    addrMisalign := AddrUtil.isAddrMisalign(addr, ldstCtrlReg.log2Memwb)
    alignedAddr  := AddrUtil.getAlignedAddr(addr)
    offset       := AddrUtil.getAlignedOffset(addr)

    val startElemPos  = (curVl - ((curVl >> ldstCtrlReg.log2Mlen) << ldstCtrlReg.log2Mlen)) 
    val startVRegIdx  = startElemPos << ldstCtrlReg.log2Memwb
    val canLoadElemCnt = WireInit(0.U((log2Ceil(8) + 1).W))
    canLoadElemCnt := Mux(canAccelerate,
                        (splitCount - curSplitIdx) min Mux(negStride, (offset >> log2Stride) + 1.U(4.W), ((8.U >> log2Stride) - (offset >> log2Stride))), 
                        1.U
                    )
    val endElemPos = startElemPos + canLoadElemCnt
    val endVRegIdx = Mux(canAccelerate, endElemPos << ldstCtrlReg.log2Memwb, startVRegIdx)
    
    // * Calculate Addr
    // * END

    val elemMaskVec = VecInit(Seq.fill(vlenb)(false.B))
    val isNotMasked = elemMaskVec.asUInt =/= 0.U

    val misalignXcpt        = 0.U.asTypeOf(new LdstXcpt)
    misalignXcpt.xcptValid := addrMisalign
    misalignXcpt.ma        := addrMisalign

    val isQFull = isFull(enqPtr, deqPtr)
    when (uopState === uop_split && !memXcpt && curSplitIdx < splitCount && !isQFull) {
        canEnqueue := (ldstCtrlReg.vm || isNotMasked) && (curSplitIdx + canLoadElemCnt >= splitStart)
        when (canEnqueue) {
            ldstUopQueue(enqPtr.value).valid  := true.B
            ldstUopQueue(enqPtr.value).status := Mux(addrMisalign, LdstUopStatus.ready, LdstUopStatus.notReady)
            ldstUopQueue(enqPtr.value).memOp  := ldstCtrlReg.isStore
            // NOTE: for misalign xcpt, put addr into ldstQueue, output directly to xcpt_addr
            ldstUopQueue(enqPtr.value).addr   := Mux(addrMisalign, addr, alignedAddr)
            ldstUopQueue(enqPtr.value).pos    := curVl
            ldstUopQueue(enqPtr.value).xcpt   := misalignXcpt
            enqPtr  := enqPtr + 1.U
        }

        (0 until vlenb).foreach { i =>
            val curElemPos = WireInit(0.U(vlenbWidth.W))
            curElemPos := i.U >> ldstCtrlReg.log2Memwb

            elemMaskVec(i) := Mux(
                curElemPos >= startElemPos && curElemPos < endElemPos,
                (ldstCtrlReg.vm || mUopInfoReg.mask((curElemPos - startElemPos) + curVl)) && (curElemPos >= splitStart),
                false.B
            )

            val maskCond = elemMaskVec(i) && !addrMisalign

            when (curElemPos >= startElemPos && curElemPos < endElemPos) {
                vregInfo(i).status  := Mux(maskCond, VRegSegmentStatus.notReady, Mux(ldstCtrlReg.ma && !addrMisalign, VRegSegmentStatus.agnostic, VRegSegmentStatus.srcData))
                vregInfo(i).idx     := Mux(maskCond, enqPtr, vregInfo(i).idx)

                val elemInnerOffset = offset + (i.U - (curElemPos << ldstCtrlReg.log2Memwb)) // which byte inside the element
                val curElemOffset = Mux(strideAbs === 0.U, 0.U, Mux(negStride, -(curElemPos - startElemPos) << log2Stride, (curElemPos - startElemPos) << log2Stride)) // which element
                vregInfo(i).offset := Mux(maskCond, elemInnerOffset + curElemOffset, vregInfo(i).offset)
            }
        }
        curSplitIdx := curSplitIdx + canLoadElemCnt
        addrReg := Mux(canAccelerate && strideAbs =/= 0.U, 
                    Mux(negStride, addr - (canLoadElemCnt << log2Stride), addr + (canLoadElemCnt << log2Stride)), addr)
    }
    // * Split LdstUop
    // * END


    // * BEGIN
    // * Issue LdstUop
    val issueUop    = ldstUopQueue(issuePtr.value)
    val isNoXcptUop = issueUop.valid & (~issueUop.xcpt.xcptValid)
    
    respPtr.value := io.dataExchange.resp.bits.idx(3, 0)
    respPtr.flag  := io.dataExchange.resp.bits.flag
    val respData   = io.dataExchange.resp.bits.data

    // non store waiting code
    when (io.dataExchange.resp.bits.nack &&  (respPtr < issuePtr)) {
        issuePtr := respPtr
    }.elsewhen (isNoXcptUop && io.dataExchange.req.ready) {
        issuePtr := issuePtr + 1.U
    }

    val storeDataVec = VecInit(Seq.fill(8)(0.U(8.W)))
    val storeMaskVec = VecInit(Seq.fill(8)(0.U(1.W)))

    (0 until vlenb).foreach { i =>
        when (ldstCtrlReg.isStore && vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === issuePtr) {
            val offset = vregInfo(i).offset
            storeDataVec(offset) := vregInfo(i).data
            storeMaskVec(offset) := 1.U
        }
    }

    when (isNoXcptUop) {
        io.dataExchange.req.valid       := true.B
        io.dataExchange.req.bits.addr   := issueUop.addr
        io.dataExchange.req.bits.cmd    := issueUop.memOp
        io.dataExchange.req.bits.srcId  := 0.U
        io.dataExchange.req.bits.flag   := issuePtr.flag
        io.dataExchange.req.bits.idx    := issuePtr.value
        io.dataExchange.req.bits.data   := storeDataVec.asUInt
        io.dataExchange.req.bits.mask   := storeMaskVec.asUInt
    }.otherwise {
        io.dataExchange.req.valid       := false.B
        io.dataExchange.req.bits        := DontCare
    }

    // * Issue LdstUop
    // * END


    // * BEGIN
    // * Recv Resp

    val respUop = ldstUopQueue(respPtr.value)

    when (io.dataExchange.resp.valid || memXcpt) {
        respUop.status := LdstUopStatus.ready
        respUop.xcpt.xcptValid := memXcpt
        respUop.xcpt.fromHellaXcpt(io.dataExchange.xcpt)
    }

    when (io.dataExchange.resp.valid && respUop.memOp === VMemCmd.read && !hasXcpt) {
        (0 until vlenb).foreach { i =>
            when (vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === respPtr) {
                val offsetOH = UIntToOH(vregInfo(i).offset, 8)
                vregInfo(i).data := Mux1H(
                    offsetOH, 
                    Seq(respData( 7,  0), respData(15,  8), respData(23, 16), respData(31, 24),
                        respData(39, 32), respData(47, 40), respData(55, 48), respData(63, 56))
                )
            }
        }
    }
    completeLdst := ldstUopQueue.forall(uop => uop.valid === false.B) // ld completed or xcpt happened
    // * Recv Resp
    // * END


    // * BEGIN
    // * Commit to VRegIngo
    val dequeUop   = ldstUopQueue(deqPtr.value)
    val canDeque   = dequeUop.valid && dequeUop.status === LdstUopStatus.ready
    val deqXcpt    = canDeque && dequeUop.xcpt.xcptValid

    when(deqXcpt) {
        (0 until vlenb).foreach { i =>
            when (vregInfo(i).idx >= deqPtr && vregInfo(i).idx.value < ldstUopQueueSize.U) {
                vregInfo(i).status := VRegSegmentStatus.xcpt
            } 
        }
        // 1. clear ldstUopQueue
        ldstUopQueue.foreach(uop => uop.valid := false.B)
        // 2. update xcpt info
        xcptVlReg       := dequeUop.pos
        xcptAddrReg     := dequeUop.addr
        hellaXcptReg    := dequeUop.xcpt.generateHellaXcpt(dequeUop.memOp)
        hasXcpt         := true.B
    }.elsewhen (canDeque) {
        dequeUop.valid  := false.B

        (0 until vlenb).foreach { i =>
            when (vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === deqPtr) {
                vregInfo(i).status      := VRegSegmentStatus.ready
                vregInfo(i).idx.value   := ldstUopQueueSize.U
            } 
        }
        deqPtr := deqPtr + 1.U
    }

    // * Commit to VRegIngo
    // * END



    // * BEGIN
    // * Writeback to uopQueue
    val vregWbReady = WireInit(false.B)

    val allSrcData = vregInfo.forall(info => info.status === VRegSegmentStatus.srcData || info.status === VRegSegmentStatus.agnostic)
    val allReadyOrSrcData = vregInfo.forall(info => info.status === VRegSegmentStatus.ready 
                                         || info.status === VRegSegmentStatus.srcData 
                                         || info.status === VRegSegmentStatus.agnostic)

    when (splitCount === 0.U && allSrcData) {
        vregWbReady := RegNext(splitCount === 0.U && allSrcData) // RegNext for scoreboard clear & write contradiction
    }.elsewhen (allReadyOrSrcData && completeLdst) {
        vregWbReady := true.B
    }.otherwise {
        vregWbReady := false.B
    }

    val wbValid = vregWbReady || hasXcpt
    io.lsuOut.valid             := wbValid
    io.lsuOut.bits.muopEnd      := mUopInfoReg.muopEnd
    io.lsuOut.bits.rfWriteEn    := mUopInfoReg.rfWriteEn
    io.lsuOut.bits.rfWriteIdx   := mUopInfoReg.ldest
    io.lsuOut.bits.rfWriteMask  := Cat(vregInfo.reverseMap(info => info.status =/= VRegSegmentStatus.ready)).asUInt
    io.lsuOut.bits.regCount     := 1.U
    io.lsuOut.bits.regStartIdx  := mUopInfoReg.ldest
    io.lsuOut.bits.isSegLoad    := false.B
    io.lsuOut.bits.data         := Mux(ldstCtrlReg.isLoad, 
        Cat(vregInfo.reverseMap(entry => entry.data)), 
        DontCare
    )

    val fofValid = ldstCtrlReg.unitSMop === UnitStrideMop.fault_only_first && xcptVlReg > 0.U
    io.lsuOut.bits.xcpt.exception_vld := hasXcpt & ~fofValid
    io.lsuOut.bits.xcpt.xcpt_cause    := Mux(fofValid, 0.U.asTypeOf(new HellaCacheExceptions), hellaXcptReg)
    io.lsuOut.bits.xcpt.xcpt_addr     := xcptAddrReg
    io.lsuOut.bits.xcpt.update_vl     := hasXcpt & fofValid
    io.lsuOut.bits.xcpt.update_data   := xcptVlReg


    when (wbValid) {
        // Reset vreg info
        for (i <- 0 until vlenb) {
            vregInfo(i).status      := VRegSegmentStatus.invalid
            vregInfo(i).idx.value   := ldstUopQueueSize.U
            vregInfo(i).offset      := DontCare
            vregInfo(i).data        := DontCare
        }

        hasXcpt := false.B
    }
    // * Writeback to uopQueue
    // * END
}
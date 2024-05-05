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
class SVHLsu(implicit p: Parameters) extends Module {
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
    val vregInitVal     = Wire(new VRegSegmentInfo)
    vregInitVal.status := VRegSegmentStatus.invalid
    vregInitVal.idx    := ldstUopQueueSize.U
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
    val ldstEnqPtr      = RegInit(0.U(ldstUopQueueWidth.W))
    val issueLdstPtr    = RegInit(0.U(ldstUopQueueWidth.W))
    val ldstUopQueue    = RegInit(VecInit(Seq.fill(ldstUopQueueSize)(0.U.asTypeOf(new LdstUop))))

    // xcpt info
    val xcptVlReg       = RegInit(0.U(bVL.W))
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
        when (io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst) {
            mUopInfoReg  := mUopInfo
            ldstCtrlReg  := ldstCtrl
            addrReg      := Mux(io.mUop.bits.uop.uopIdx === 0.U, io.mUop.bits.scalar_opnd_1 + (mUopInfo.segIdx << ldstCtrl.log2Memwb), addrReg)
            // Set split info
            ldstEnqPtr   := 0.U
            issueLdstPtr := 0.U
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

    addrMask     := (("h1".asUInt(addrWidth.W) << ldstCtrlReg.log2Memwb) - 1.U)
    addrMisalign := (addr & addrMask).orR  // align addr to memwb ?
    alignedAddr  := (addr >> (log2Ceil(dataWidth / 8)).U) << (log2Ceil(dataWidth / 8)).U // align addr to 64 bits
    offset       := addr - alignedAddr 

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

    when (uopState === uop_split && !memXcpt && curSplitIdx < splitCount) {
        canEnqueue := (ldstCtrlReg.vm || isNotMasked) && (curSplitIdx + canLoadElemCnt >= splitStart)
        when (canEnqueue) {
            ldstUopQueue(ldstEnqPtr).valid  := true.B
            ldstUopQueue(ldstEnqPtr).addr   := alignedAddr
            ldstUopQueue(ldstEnqPtr).pos    := curVl
            ldstUopQueue(ldstEnqPtr).xcpt   := Mux(addrMisalign, LdstUopXcptCause.misalign, 0.U)
            ldstEnqPtr  := ldstEnqPtr + 1.U
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
                vregInfo(i).idx     := Mux(maskCond, ldstEnqPtr, vregInfo(i).idx)

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

    val isNoXcptUop = ldstUopQueue(issueLdstPtr).valid && (ldstUopQueue(issueLdstPtr).xcpt === 0.U)
    
    // // non store waiting code
    // when (io.dataExchange.resp.bits.nack && io.dataExchange.resp.bits.idx <= issueLdstPtr) {
    //     issueLdstPtr := io.dataExchange.resp.bits.idx
    // }.elsewhen (isNoXcptUop && io.dataExchange.req.ready) {
    //     issueLdstPtr := issueLdstPtr + 1.U
    // }

    // when (isNoXcptUop) {
    
    /*
     * load => issue
     * store => wait until resp
     * replay => reissue
     */

    val issue_go :: issue_wait :: Nil = Enum(2)

    val issueState = RegInit(issue_go)

    switch (issueState) {
        is (issue_go) {
            when (ldstCtrlReg.isLoad) {
                issueState := issue_go
            }.elsewhen (ldstCtrlReg.isStore && io.dataExchange.req.fire) {
                issueState := issue_wait
            }
            // when (io.dataExchange.req.fire) { // stall load & store until resp back
            //     issueState := issue_wait
            // }
        }
        is (issue_wait) {
            when (io.dataExchange.resp.valid || io.dataExchange.resp.bits.nack || memXcpt) {
                issueState := issue_go
            }
        }
    }

    when (io.dataExchange.resp.bits.nack && io.dataExchange.resp.bits.idx <= issueLdstPtr) {
        issueLdstPtr := io.dataExchange.resp.bits.idx
    }.elsewhen (io.dataExchange.req.ready && issueState === issue_go && !completeLdst) {
        issueLdstPtr := issueLdstPtr + 1.U // NOTE: exsits multiple issues for the same uop
    }

    when (issueState === issue_go && isNoXcptUop) {
        val storeDataVec = VecInit(Seq.fill(8)(0.U(8.W)))
        val storeMaskVec = VecInit(Seq.fill(8)(0.U(1.W)))

        (0 until vlenb).foreach { i =>
            when (ldstCtrlReg.isStore && vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === issueLdstPtr) {
                val offset = vregInfo(i).offset
                storeDataVec(offset) := vregInfo(i).data
                storeMaskVec(offset) := 1.U
            }
        }
        io.dataExchange.req.valid       := true.B
        io.dataExchange.req.bits.addr   := ldstUopQueue(issueLdstPtr).addr
        io.dataExchange.req.bits.cmd    := Mux(ldstCtrlReg.isLoad, VMemCmd.read, VMemCmd.write)
        io.dataExchange.req.bits.idx    := issueLdstPtr
        io.dataExchange.req.bits.data   := Mux(ldstCtrlReg.isLoad, DontCare, storeDataVec.asUInt)
        io.dataExchange.req.bits.mask   := Mux(ldstCtrlReg.isLoad, DontCare, storeMaskVec.asUInt)
    }.otherwise {
        io.dataExchange.req.valid       := false.B
        io.dataExchange.req.bits        := DontCare
    }

    // * Issue LdstUop
    // * END


    // * BEGIN
    // * Recv Resp
    val (respLdstPtr, respData) = (io.dataExchange.resp.bits.idx, io.dataExchange.resp.bits.data)
    val lastXcptInfo = RegInit(0.U.asTypeOf(new HellaCacheExceptions))
    val lastXcptIdx  = RegInit(ldstUopQueueSize.U(ldstUopQueueWidth.W))

    when (io.dataExchange.resp.valid && uopState =/= uop_idle && respLdstPtr < lastXcptIdx) {
        val loadComplete  = ldstCtrlReg.isLoad && io.dataExchange.resp.bits.has_data
        val storeComplete = ldstCtrlReg.isStore

        ldstUopQueue(respLdstPtr).valid := false.B

        (0 until vlenb).foreach { i =>
            when (vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === respLdstPtr) {
                 vregInfo(i).status := VRegSegmentStatus.ready
            } 
        }

        when (loadComplete) {
            (0 until vlenb).foreach { i =>
                when (vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === respLdstPtr) {
                    val offsetOH = UIntToOH(vregInfo(i).offset, 8)
                    vregInfo(i).data := Mux1H(
                        offsetOH, 
                        Seq(respData( 7,  0), respData(15,  8), respData(23, 16), respData(31, 24),
                            respData(39, 32), respData(47, 40), respData(55, 48), respData(63, 56))
                    )
                }
            }
         }

    }
    
    val ldstMinValidIdx = PriorityEncoder(ldstUopQueue.map(uop => uop.valid))
    val ldstMinXcptIdx  = PriorityEncoder(ldstUopQueue.map(uop => uop.valid & (uop.xcpt =/= 0.U)))

    when (memXcpt && ldstUopQueue(respLdstPtr).valid && respLdstPtr < lastXcptIdx) { // exception handling
        // update xcpt info
        ldstUopQueue(respLdstPtr).xcpt := LdstUopXcptCause.mem_xcpt
        lastXcptIdx  := respLdstPtr
        lastXcptInfo := io.dataExchange.xcpt
    }
    
    val misalignXcpt    = Wire(new HellaCacheExceptions)
    misalignXcpt.ma.ld := ldstCtrlReg.isLoad
    misalignXcpt.ma.st := ldstCtrlReg.isStore
    misalignXcpt.pf.ld := false.B
    misalignXcpt.pf.st := false.B
    misalignXcpt.gf.ld := false.B
    misalignXcpt.gf.st := false.B
    misalignXcpt.ae.ld := false.B
    misalignXcpt.ae.st := false.B

    when (ldstMinValidIdx === ldstMinXcptIdx && (ldstMinValidIdx =/= (ldstUopQueueSize - 1).U)) {
        // 1. clear ldstUopQueue
        ldstUopQueue.foreach(uop => uop.valid := false.B)
        // 2. update xcpt info
        xcptVlReg       := ldstUopQueue(ldstMinValidIdx).pos

        (0 until vlenb).foreach { i =>
            when(vregInfo(i).idx >= ldstMinXcptIdx) {
                vregInfo(i).status := VRegSegmentStatus.xcpt
            } 
        }

        hellaXcptReg    := Mux(ldstUopQueue(ldstMinValidIdx).xcpt === LdstUopXcptCause.misalign, misalignXcpt, lastXcptInfo)
        hasXcpt         := true.B
    }

    completeLdst := ldstUopQueue.forall(uop => uop.valid === false.B) // ld completed or xcpt happened

    // * Recv Resp
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

    when (vregWbReady || hasXcpt) {
        io.lsuOut.valid             := true.B
        io.lsuOut.bits.muopEnd      := mUopInfoReg.muopEnd
        io.lsuOut.bits.rfWriteEn    := mUopInfoReg.rfWriteEn
        io.lsuOut.bits.rfWriteIdx   := mUopInfoReg.ldest

        // io.lsuOut.bits.rfWriteMask  := Cat(vregInfo.map(info => info.status === VRegSegmentStatus.srcData)).asUInt
        io.lsuOut.bits.rfWriteMask  := 0.U


        io.lsuOut.bits.data         := Mux(ldstCtrlReg.isLoad,
            Mux(!vstartGeVl,
                Cat(vregInfo.reverseMap(entry => Mux(entry.status === VRegSegmentStatus.agnostic, "hff".U(8.W), entry.data))),
                Cat(vregInfo.reverseMap(entry => entry.data))// Concatenate data from all vregInfo elements)
            ),
            DontCare
        )

        // Reset vreg info
        for (i <- 0 until vlenb) {
            vregInfo(i).status  := VRegSegmentStatus.invalid
            vregInfo(i).idx     := ldstUopQueueSize.U
            vregInfo(i).offset  := DontCare
            vregInfo(i).data    := DontCare
        }
    }.otherwise {
        io.lsuOut.valid          := false.B
        io.lsuOut.bits           := DontCare
        io.lsuOut.bits.rfWriteEn := false.B
    }

    // exception output
    when (hasXcpt) {
        when (ldstCtrlReg.unitSMop === UnitStrideMop.fault_only_first && xcptVlReg > 0.U) {
            io.xcpt.exception_vld   := false.B
            io.xcpt.xcpt_cause      := 0.U.asTypeOf(new HellaCacheExceptions)
        }.otherwise {
            io.xcpt.exception_vld   := true.B
            io.xcpt.xcpt_cause      := hellaXcptReg
        }
        io.xcpt.update_vl           := true.B
        io.xcpt.update_data         := xcptVlReg

        // reset xcpt
        hasXcpt     := false.B
        lastXcptIdx := ldstUopQueueSize.U
    }.otherwise {
        io.xcpt.exception_vld       := false.B
        io.xcpt.update_vl           := false.B
        io.xcpt.update_data         := DontCare
        io.xcpt.xcpt_cause          := 0.U.asTypeOf(new HellaCacheExceptions)
    }
    // * Writeback to uopQueue
    // * END
}
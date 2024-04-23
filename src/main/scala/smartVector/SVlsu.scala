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

class SVlsu(implicit p: Parameters) extends Module {
    val io = IO(new LdstIO())

    // global used signals
    // {{{

    // group queue
    val groupQ = Module(new GroupInfoQueue(GroupNum))
    // split fsm states
    val uop_idle :: uop_split :: Nil = Enum(2)
    val uopState        = RegInit(uop_idle)
    val nextUopState    = WireInit(uop_idle)
    val cleanLdst    = WireInit(false.B)
    val stopSplit       = WireInit(false.B)

    // address reg
    val addrReg         = RegInit(0.U(addrWidth.W))

    // uop & control related
    val mUopInfoRegVec     = VecInit(Seq.fill(2)(RegInit(0.U.asTypeOf(new mUopInfo))))
    val ldstCtrlRegVec     = VecInit(Seq.fill(2)(RegInit(0.U.asTypeOf(new LSULdstCtrl))))

    // vreg seg info
    val vregSegInitVal     = Wire(new VRegSegmentInfo)
    vregSegInitVal.status := VRegSegmentStatus.invalid
    vregSegInitVal.idx    := ldstUopQueueSize.U
    vregSegInitVal.offset := DontCare
    vregSegInitVal.data   := DontCare

    val vregInfoInitVal = Wire(new VRegInfo)
    vregInfoInitVal.busy := false.B
    vregInfoInitVal.clean := true.B
    // vregInfoInitVal.vregSeg := vregSegInitVal

    (0 until vlenb).foreach { i =>
        vregInfoInitVal.vregSeg(i) := vregSegInitVal
    }
    val vregInfoVec = RegInit(VecInit(Seq.fill(GroupNum)(vregInfoInitVal)))

    // Split info
    val vstartGeVl      = RegInit(false.B)
    val splitCount      = RegInit(0.U(vlenbWidth.W))
    val curSplitIdx     = RegInit(0.U(vlenbWidth.W))
    val splitStart      = RegInit(0.U(vlenbWidth.W))

    // ldstQueue
    val ldstEnqPtr      = RegInit(0.U(ldstUopQueueWidth.W))
    val issueLdstPtr    = RegInit(0.U(ldstUopQueueWidth.W))
    val ldstUopQueue    = RegInit(VecInit(Seq.fill(ldstUopQueueSize)(0.U.asTypeOf(new LdstUop))))

    // xcpt info
    val xcptVlReg       = RegInit(0.U(bVL.W))
    val hellaXcptReg    = RegInit(0.U.asTypeOf(new HellaCacheExceptions))

    // TODO: 1. assertion 2. exception only once
    val addrMisalign    = WireInit(false.B)
    val memXcpt         = io.dataExchange.xcpt.asUInt.orR
    val hasXcpt         = RegInit(false.B)
    
    // }}}

    // split stage begin
    // {{{
  
    val vRegInfoReady = vregInfoVec(0).busy === false.B || vregInfoVec(1).busy === false.B || !vregInfoVec(0).clean || !vregInfoVec(1).clean
    io.lsuReady := Mux(uopState === uop_idle && vRegInfoReady && !hasXcpt && groupQ.io.enq.ready, true.B, false.B)

    // SPLIT FSM -- decide next state
    when (uopState === uop_idle) {
        when (io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst && vRegInfoReady && groupQ.io.enq.ready) {
            nextUopState := uop_split
        }.otherwise {
            nextUopState := uop_idle
        }
    }.elsewhen (uopState === uop_split) {
        when (stopSplit) {
            nextUopState := uop_idle
        }.otherwise {
            nextUopState := uop_split
        }
    }.otherwise {
        nextUopState := uop_idle
    }

    // SPLIT FSM -- transition
    uopState := nextUopState

    // if exception occurs or split finished, stop split
    stopSplit := memXcpt || (uopState === uop_split && curSplitIdx < splitCount && addrMisalign) ||
                (curSplitIdx + 1.U >= splitCount) || (splitCount === 0.U)

    // split idle stage begin
    val (vstart, vl)     = (io.mUop.bits.uop.info.vstart, io.mUop.bits.uop.info.vl)
    val (uopIdx, uopEnd) = (io.mUop.bits.uop.uopIdx, io.mUop.bits.uop.uopEnd)
    
    val ldstCtrl = LSULdstDecoder(io.mUop.bits, io.mUopMergeAttr.bits)
    val mUopInfo = mUopInfoSelecter(io.mUop.bits, io.mUopMergeAttr.bits)

    // decide micro vl
    val actualVl    = Mux(ldstCtrl.unitSMop === UnitStrideMop.mask, (vl + 7.U) >> 3.U, vl) // ceil(vl/8)
    val doneLen     = uopIdx << ldstCtrl.log2MinLen
    val leftLen     = Mux(ldstCtrl.unitSMop === UnitStrideMop.whole_register, ldstCtrl.mlen, Mux(actualVl > doneLen, actualVl - doneLen, 0.U))
    val microVl     = ldstCtrl.minLen min leftLen
    val microVstart = Mux(vstart < doneLen, 0.U, ldstCtrl.minLen min (vstart - doneLen))

    val memVl       = leftLen min ldstCtrl.mlen
    val memVstart   = Mux(vstart < doneLen, 0.U, ldstCtrl.mlen min (vstart - doneLen))

    // alloc index
    val prevVRegNotComplete = vregInfoVec.map(info => info.busy && !info.clean).reduce(_||_)
    val allocIdxReg = RegInit(0.U(1.W))
    val allocIdx = Mux(
        prevVRegNotComplete, 
        PriorityEncoder(vregInfoVec.map(info => info.busy || !info.clean)), 
        PriorityEncoder(vregInfoVec.map(info => !info.busy))
    )

    val groupQEnqInfo = WireInit(0.U.asTypeOf(new GroupInfo))
    groupQEnqInfo.allocIdx := allocIdx
    groupQEnqInfo.ldstCtrl := ldstCtrl
    groupQEnqInfo.muopInfo := mUopInfo

    groupQ.io.enq.valid := io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst && vRegInfoReady && io.lsuReady
    groupQ.io.enq.bits := groupQEnqInfo
    

    when(uopState === uop_idle) {
        when(io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst && vRegInfoReady && io.lsuReady) {
            addrReg      := Mux(io.mUop.bits.uop.uopIdx === 0.U, io.mUop.bits.scalar_opnd_1 + (mUopInfo.segIdx << ldstCtrl.log2Memwb), addrReg)
            
            // Set split info
            curSplitIdx  := 0.U
            splitCount   := microVl
            splitStart   := microVstart
            vstartGeVl   := vstart > actualVl
            vregInfoVec(allocIdx).busy := true.B
            // set vreg
            when(vregInfoVec(allocIdx).clean) { // FIXME: how to judge it's clean
                (0 until vlenb).foreach { i => 
                    val pos = i.U >> ldstCtrl.log2Memwb

                    vregInfoVec(allocIdx).vregSeg(i).data   := io.mUop.bits.uopRegInfo.old_vd(8 * i + 7, 8 * i)
                    vregInfoVec(allocIdx).vregSeg(i).status := Mux(
                        pos < memVl, 
                        VRegSegmentStatus.needLdst, 
                        Mux(io.mUop.bits.uop.info.ta, VRegSegmentStatus.agnostic, VRegSegmentStatus.srcData)
                    )
                }
                groupQ.io.deq.ready := false.B
            }.otherwise {
                groupQ.io.deq.ready := true.B
            }
        }
    }

    // calc addr begin
    // {{{
    val addr        = WireInit(0.U(addrWidth.W))
    val addrMask    = WireInit(0.U(addrWidth.W))
    val alignedAddr = WireInit(0.U(addrWidth.W))
    val offset      = WireInit(0.U(log2Ceil(addrWidth / 8).W))

    val curVl       = (groupQ.io.tail.bits.muopInfo.uopIdx << groupQ.io.tail.bits.ldstCtrl.log2MinLen) + curSplitIdx
    val baseAddr    = groupQ.io.tail.bits.muopInfo.rs1Val

    // indexed addr
    val idxVal      = WireInit(0.U(XLEN.W))
    val idxMask     = WireInit(0.U(XLEN.W))
    val eew         = groupQ.io.tail.bits.ldstCtrl.eewb << 3.U
    val beginIdx    = (curVl - ((curVl >> groupQ.io.tail.bits.ldstCtrl.log2Elen) << groupQ.io.tail.bits.ldstCtrl.log2Elen)) << (groupQ.io.tail.bits.ldstCtrl.log2Eewb +& 3.U) // 3.U: to bits
    idxMask := (("h1".asUInt(addrWidth.W) << eew) - 1.U)
    idxVal  := (groupQ.io.tail.bits.muopInfo.vs2 >> beginIdx) & idxMask

    val stride = WireInit(0.S(XLEN.W))
    val negStride = stride < 0.S
    val strideAbs = Mux(negStride, (-stride).asUInt, stride.asUInt)

    when(groupQ.io.tail.bits.ldstCtrl.ldstType === Mop.unit_stride) {
        stride := (groupQ.io.tail.bits.ldstCtrl.nfield << groupQ.io.tail.bits.ldstCtrl.log2Memwb).asSInt
    }.elsewhen(groupQ.io.tail.bits.ldstCtrl.ldstType === Mop.constant_stride) {
        stride := groupQ.io.tail.bits.muopInfo.rs2Val.asSInt
    }.otherwise {
        stride := 11111.S
    }

    val accelerateStride = Cat(strideAbs === 4.U, strideAbs === 2.U, strideAbs === 1.U)
    val canAccelerate = accelerateStride =/= 0.U || strideAbs === 0.U
    val log2Stride = Mux(canAccelerate, Mux1H(accelerateStride, Seq(0.U, 1.U, 2.U)), 0.U)

    val curStridedAddr = Mux(
        curSplitIdx === 0.U && groupQ.io.tail.bits.muopInfo.uopIdx === 0.U,
        addrReg,
        Mux(negStride, addrReg - strideAbs, addrReg + strideAbs)
    )
    
    when(canAccelerate) {
        addr := addrReg
    }.otherwise {
        addr := Mux1H(groupQ.io.tail.bits.ldstCtrl.ldstType, Seq(
            // unit stride
            curStridedAddr,
            // index_unodered
            ((groupQ.io.tail.bits.muopInfo.segIdx << groupQ.io.tail.bits.ldstCtrl.log2Memwb) + (Cat(false.B, baseAddr).asSInt + idxVal.asSInt).asUInt),
            // strided
            curStridedAddr,
            // index_odered
            ((groupQ.io.tail.bits.muopInfo.segIdx << groupQ.io.tail.bits.ldstCtrl.log2Memwb) + (Cat(false.B, baseAddr).asSInt + idxVal.asSInt).asUInt)
        ))
    }

    addrMask     := (("h1".asUInt(addrWidth.W) << groupQ.io.tail.bits.ldstCtrl.log2Memwb) - 1.U)
    addrMisalign := (addr & addrMask).orR  // align addr to memwb ?
    alignedAddr  := (addr >> (log2Ceil(dataWidth / 8)).U) << (log2Ceil(dataWidth / 8)).U // align addr to 64 bits
    offset       := addr - alignedAddr 

    val startElemPos   = (curVl - ((curVl >> groupQ.io.tail.bits.ldstCtrl.log2Mlen) << groupQ.io.tail.bits.ldstCtrl.log2Mlen)) 
    val startVRegIdx   = startElemPos << groupQ.io.tail.bits.ldstCtrl.log2Memwb
    val canLoadElemCnt = WireInit(0.U((log2Ceil(8) + 1).W))
    canLoadElemCnt := Mux(canAccelerate,
        (splitCount - curSplitIdx) min Mux(negStride, (offset >> log2Stride) + 1.U(4.W), ((8.U >> log2Stride) - (offset >> log2Stride))), 
        1.U)
    val endElemPos = startElemPos + canLoadElemCnt
    val endVRegIdx = Mux(canAccelerate, endElemPos << groupQ.io.tail.bits.ldstCtrl.log2Memwb, startVRegIdx)

    // calc addr end
    // }}}

    val elemMaskVec = VecInit(Seq.fill(vlenb)(false.B))
    val isNotMasked = elemMaskVec.asUInt =/= 0.U

    val groupEnqCountVec = Reg(Vec(GroupNum, UInt(vlenbWidth.W)))
    val groupEnqStartVec = Reg(Vec(GroupNum, UInt(vlenbWidth.W)))

    val ldstUopQueueReady = ldstUopQueue.map(uop => !uop.valid).reduce(_||_) // FIXME

    when(uopState === uop_split && !memXcpt && curSplitIdx < splitCount && ldstUopQueueReady) {
        val allocIdx = groupQ.io.tail.bits.allocIdx
        val canEnqueue = (groupQ.io.tail.bits.ldstCtrl.vm || isNotMasked) && (curSplitIdx + canLoadElemCnt >= splitStart)
        when(canEnqueue) {
            ldstUopQueue(ldstEnqPtr).valid  := true.B
            ldstUopQueue(ldstEnqPtr).addr   := alignedAddr
            ldstUopQueue(ldstEnqPtr).pos    := curVl
            ldstUopQueue(ldstEnqPtr).groupIdx := groupQ.io.tail.bits.allocIdx
            ldstUopQueue(ldstEnqPtr).splitIdx := curSplitIdx
            ldstUopQueue(ldstEnqPtr).memCmd := groupQ.io.tail.bits.ldstCtrl.isStore
            ldstUopQueue(ldstEnqPtr).xcpt   := Mux(addrMisalign, LdstUopXcptCause.misalign, 0.U)
            ldstEnqPtr                      := ldstEnqPtr + 1.U // FIXME

            groupEnqCountVec(allocIdx) := groupEnqCountVec(allocIdx) + 1.U
            when(groupEnqCountVec(allocIdx) === 0.U) {
                groupEnqStartVec(allocIdx) := ldstEnqPtr
            }
        }

        (0 until vlenb).foreach { i =>
            val curElemPos = WireInit(0.U(vlenbWidth.W))
            curElemPos := i.U >> groupQ.io.tail.bits.ldstCtrl.log2Memwb

            elemMaskVec(i) := Mux(
                curElemPos >= startElemPos && curElemPos < endElemPos,
                (groupQ.io.tail.bits.ldstCtrl.vm || groupQ.io.tail.bits.muopInfo.mask((curElemPos - startElemPos) + curVl)) && (curElemPos >= splitStart),
                false.B
            )

            val maskCond = elemMaskVec(i) && !addrMisalign

            when( curElemPos >= startElemPos && curElemPos < endElemPos) {
                vregInfoVec(allocIdx).vregSeg(i).status  := Mux(maskCond, VRegSegmentStatus.notReady, Mux(groupQ.io.tail.bits.ldstCtrl.ma && !addrMisalign, VRegSegmentStatus.agnostic, VRegSegmentStatus.srcData))
                vregInfoVec(allocIdx).vregSeg(i).idx     := Mux(maskCond, ldstEnqPtr, vregInfoVec(allocIdx).vregSeg(i).idx)

                val elemInnerOffset = offset + (i.U - (curElemPos << groupQ.io.tail.bits.ldstCtrl.log2Memwb)) // which byte inside the element
                val curElemOffset = Mux(strideAbs === 0.U, 0.U, Mux(negStride, -(curElemPos - startElemPos) << log2Stride, (curElemPos - startElemPos) << log2Stride)) // which element
                vregInfoVec(allocIdx).vregSeg(i).offset := Mux(maskCond, elemInnerOffset + curElemOffset, vregInfoVec(allocIdx).vregSeg(i).offset)
            }
        }
        curSplitIdx := curSplitIdx + canLoadElemCnt
        addrReg := Mux(canAccelerate && strideAbs =/= 0.U, 
                    Mux(negStride, addr - (canLoadElemCnt << log2Stride), addr + (canLoadElemCnt << log2Stride)), addr)
    }

    
    // judge vreg is clean
    when (io.lsuReady && vregInfoVec(groupQ.io.tail.bits.allocIdx).busy) {
        vregInfoVec(groupQ.io.tail.bits.allocIdx).clean := !((0 until vlenb).map(i => vregInfoVec(groupQ.io.tail.bits.allocIdx).vregSeg(i).status === VRegSegmentStatus.needLdst).reduce(_||_))
    }
    // split stage end
    // }}}

    // issue stage start
    // {{{
    val notXcptUop = ldstUopQueue(issueLdstPtr).valid && (ldstUopQueue(issueLdstPtr).xcpt === 0.U)
    // update issueLdPtr
    // <= or < ?
    // && io.dataExchange.resp.bits.idx <= issueLdstPtr
    when(io.dataExchange.resp.bits.nack) { // FIXME
        issueLdstPtr := io.dataExchange.resp.bits.idx
    }.elsewhen(notXcptUop && io.dataExchange.req.ready) {
        issueLdstPtr := issueLdstPtr + 1.U
    }

    when(notXcptUop) {
        val issueGroupIdx = ldstUopQueue(issueLdstPtr).groupIdx
        val storeDataVec = VecInit(Seq.fill(8)(0.U(8.W)))
        val storeMaskVec = VecInit(Seq.fill(8)(0.U(1.W)))

        (0 until vlenb).foreach { i =>
            when(ldstCtrlRegVec(issueGroupIdx).isStore && vregInfoVec(issueGroupIdx).vregSeg(i).status === VRegSegmentStatus.notReady && vregInfoVec(issueGroupIdx).vregSeg(i).idx === issueLdstPtr) {
                val offset = vregInfoVec(issueGroupIdx).vregSeg(i).offset
                storeDataVec(offset) := vregInfoVec(issueGroupIdx).vregSeg(i).data
                storeMaskVec(offset) := 1.U
            }
        }
        io.dataExchange.req.valid       := true.B
        io.dataExchange.req.bits.addr   := ldstUopQueue(issueLdstPtr).addr
        io.dataExchange.req.bits.cmd    := ldstUopQueue(issueLdstPtr).memCmd
        io.dataExchange.req.bits.idx    := issueLdstPtr
        io.dataExchange.req.bits.data   := Mux(ldstUopQueue(issueLdstPtr).memCmd, storeDataVec.asUInt, DontCare)
        io.dataExchange.req.bits.mask   := Mux(ldstUopQueue(issueLdstPtr).memCmd, storeMaskVec.asUInt, DontCare)
    }.otherwise {
        io.dataExchange.req.valid       := false.B
        io.dataExchange.req.bits        := DontCare
    }
    // }}}
    // issue stage end


    // resp stage start
    // {{{

    val (respLdstPtr, respData) = (io.dataExchange.resp.bits.idx, io.dataExchange.resp.bits.data)
    val (respGroupIdx, respSplitIdx) = (ldstUopQueue(respLdstPtr).groupIdx, ldstUopQueue(respLdstPtr).splitIdx)


    val lastXcptInfo        = RegInit(0.U.asTypeOf(new HellaCacheExceptions))
    val lastXcptIdx         = RegInit(ldstUopQueueSize.U(ldstUopQueueWidth.W))
    val lastXcptGroupIdx    = RegInit(2.U(GroupNum.W))
    val lastXcptSplitIdx    = RegInit(ldstUopQueueSize.U(vlenbWidth.W))

    val respGroupIdxReg = RegInit(0.U(GroupNum.W))
    when(io.dataExchange.resp.valid || memXcpt || io.dataExchange.resp.bits.nack) {
        respGroupIdxReg := respGroupIdx
    }

    // val smallThanXcpt = respSplitIdx < lastXcptSplitIdx

    when(io.dataExchange.resp.valid && respSplitIdx < lastXcptSplitIdx) {
        val loadclean  = !ldstUopQueue(respLdstPtr).memCmd && io.dataExchange.resp.bits.has_data
        val storeclean = ldstUopQueue(respLdstPtr).memCmd

        ldstUopQueue(respLdstPtr).valid := false.B

        (0 until vlenb).foreach { i =>
            when(vregInfoVec(respGroupIdx).vregSeg(i).status === VRegSegmentStatus.notReady && vregInfoVec(respGroupIdx).vregSeg(i).idx === respLdstPtr) {
                vregInfoVec(respGroupIdx).vregSeg(i).status := VRegSegmentStatus.ready
            }
        }

        when(loadclean) {
            (0 until vlenb).foreach { i =>
                when(vregInfoVec(respGroupIdx).vregSeg(i).status === VRegSegmentStatus.notReady && vregInfoVec(respGroupIdx).vregSeg(i).idx === respLdstPtr) {
                    val offsetOH = UIntToOH(vregInfoVec(respGroupIdx).vregSeg(i).offset, 8)
                    vregInfoVec(respGroupIdx).vregSeg(i).data := Mux1H(
                        offsetOH, 
                        Seq(respData( 7,  0), respData(15,  8), respData(23, 16), respData(31, 24),
                            respData(39, 32), respData(47, 40), respData(55, 48), respData(63, 56))
                    )
                }
            }
        }
    }

    val beforeXcptNotComplete = ldstUopQueue.map { uop =>
        uop.valid && uop.groupIdx === respGroupIdxReg && uop.splitIdx < lastXcptSplitIdx // FIXME lastXcptrIdx不正确 
    }.reduce(_||_)

    when(memXcpt && ldstUopQueue(respLdstPtr).valid && respSplitIdx < lastXcptSplitIdx) { // exception handling
        // update xcpt info
        ldstUopQueue(respLdstPtr).xcpt := LdstUopXcptCause.mem_xcpt
        lastXcptIdx  := respLdstPtr
        lastXcptSplitIdx := respSplitIdx
        lastXcptInfo := io.dataExchange.xcpt
    }.elsewhen(ldstUopQueue(issueLdstPtr).valid && (ldstUopQueue(issueLdstPtr).xcpt =/= 0.U) && ldstUopQueue(issueLdstPtr).splitIdx < lastXcptSplitIdx) {
        // update xcpt info
        lastXcptIdx  := issueLdstPtr
        lastXcptSplitIdx := ldstUopQueue(issueLdstPtr).splitIdx
        lastXcptInfo := io.dataExchange.xcpt
    }
    
    val misalignXcpt    = Wire(new HellaCacheExceptions)
    misalignXcpt.ma.ld := groupQ.io.front.bits.ldstCtrl.isLoad
    misalignXcpt.ma.st := groupQ.io.front.bits.ldstCtrl.isStore
    misalignXcpt.pf.ld := false.B
    misalignXcpt.pf.st := false.B
    misalignXcpt.gf.ld := false.B
    misalignXcpt.gf.st := false.B
    misalignXcpt.ae.ld := false.B
    misalignXcpt.ae.st := false.B

    when(!beforeXcptNotComplete && ldstUopQueue(lastXcptIdx).xcpt > 0.U) { // FIXME hasXcpt
        // 1. clear ldstUopQueue
        ldstUopQueue.foreach(uop => uop.valid := false.B)
        // 2. update xcpt info
        xcptVlReg       := ldstUopQueue(lastXcptIdx).pos

        (0 until vlenb).foreach { i =>
            when(vregInfoVec(respGroupIdxReg).vregSeg(i).idx >= lastXcptIdx) { // FIXME FIXME
                vregInfoVec(respGroupIdxReg).vregSeg(i).status := VRegSegmentStatus.xcpt
            }
        }

        hellaXcptReg    := Mux(ldstUopQueue(lastXcptIdx).xcpt === LdstUopXcptCause.misalign, misalignXcpt, lastXcptInfo) // FIXME
        hasXcpt         := true.B
    }
    
    // }}}
    // resp stage end

    // writeback to uopQueue start
    // {{{
    val vregWbReady = WireInit(false.B)
    val commitGroupInfo = groupQ.io.front.bits
    val commitGroupIdx = commitGroupInfo.allocIdx


    val allSrcData = vregInfoVec(commitGroupIdx).vregSeg.forall(info => info.status === VRegSegmentStatus.srcData || info.status === VRegSegmentStatus.agnostic)
    val allReadyOrSrcData = vregInfoVec(commitGroupIdx).vregSeg.forall(info => info.status === VRegSegmentStatus.ready 
                                         || info.status === VRegSegmentStatus.srcData 
                                         || info.status === VRegSegmentStatus.agnostic)

    when(splitCount === 0.U && allSrcData && vregInfoVec(commitGroupIdx).busy) {
        vregWbReady := RegNext(splitCount === 0.U && allSrcData) // RegNext for scoreboard clear & write contradiction
    }.elsewhen(allReadyOrSrcData && vregInfoVec(commitGroupIdx).busy) {
        vregWbReady := true.B
    }.otherwise {
        vregWbReady := false.B
    }

    groupQ.io.deq.ready := vregWbReady
    groupQ.io.clear := hasXcpt // 会不会没有拿到commitGroupIdx就清空

    when(vregWbReady || hasXcpt) {
        io.lsuOut.valid             := true.B
        io.lsuOut.bits.muopEnd      := groupQ.io.tail.bits.muopInfo.muopEnd
        io.lsuOut.bits.rfWriteEn    := groupQ.io.tail.bits.muopInfo.rfWriteEn
        io.lsuOut.bits.rfWriteIdx   := groupQ.io.tail.bits.muopInfo.ldest


        io.lsuOut.bits.data         := Mux(groupQ.io.tail.bits.ldstCtrl.isLoad,
            Mux(!vstartGeVl,
                Cat(vregInfoVec(commitGroupIdx).vregSeg.reverseMap(entry => Mux(entry.status === VRegSegmentStatus.agnostic, "hff".U(8.W), entry.data))),
                Cat(vregInfoVec(commitGroupIdx).vregSeg.reverseMap(entry => entry.data))// Concatenate data from all vregInfo elements)
            ),
            DontCare
        )

        groupEnqCountVec(commitGroupIdx) := 0.U
        groupEnqStartVec(commitGroupIdx) := 0.U

        // Reset vreg info
        vregInfoVec(commitGroupIdx).busy := false.B
        vregInfoVec(commitGroupIdx).clean := true.B

        for (i <- 0 until vlenb) {
            vregInfoVec(commitGroupIdx).vregSeg(i).status  := VRegSegmentStatus.invalid
            vregInfoVec(commitGroupIdx).vregSeg(i).idx     := ldstUopQueueSize.U
            vregInfoVec(commitGroupIdx).vregSeg(i).offset  := DontCare
            vregInfoVec(commitGroupIdx).vregSeg(i).data    := DontCare
        }
    }.otherwise {
        io.lsuOut.valid := false.B
        io.lsuOut.bits  := DontCare

        groupQ.io.deq.ready := false.B
    }

    // exception output
    when(hasXcpt) {
        when(commitGroupInfo.ldstCtrl.unitSMop === UnitStrideMop.fault_only_first && xcptVlReg > 0.U) {
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
        lastXcptGroupIdx := (-1).S.asUInt
        lastXcptIdx := ldstUopQueueSize.U
        lastXcptSplitIdx := ldstUopQueueSize.U
    }.otherwise {
        io.xcpt.exception_vld       := false.B
        io.xcpt.update_vl           := false.B
        io.xcpt.update_data         := DontCare
        io.xcpt.xcpt_cause          := 0.U.asTypeOf(new HellaCacheExceptions)
    }

    // writeback to uopQueue end
    // }}}
}

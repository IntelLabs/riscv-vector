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

class SVVLsu(implicit p: Parameters) extends Module {
    val io = IO(new LdstIO())

    // * BEGIN
    // * signal define

    // save last addr
    val addrReg = RegInit(0.U(addrWidth.W))

    // ldQueue
    val canEnqueue      = WireInit(false.B)
    val ldstEnqPtr      = RegInit(0.U(vLdstUopQueueWidth.W))
    val issueLdstPtr    = RegInit(0.U(vLdstUopQueueWidth.W))
    val commitPtr       = RegInit(0.U(vLdstUopQueueWidth.W))
    val ldstUopQueue    = RegInit(VecInit(Seq.fill(vLdstUopQueueSize)(0.U.asTypeOf(new SegLdstUop))))

    val ldstQueueCnt   = Mux(ldstEnqPtr >= commitPtr, ldstEnqPtr - commitPtr, ldstEnqPtr + vLdstUopQueueSize.U - commitPtr)
    val ldstQueueFull  = ldstQueueCnt >= (vLdstUopQueueSize - 1).U

    // * signal define
    // * END
    io.lsuReady := !ldstQueueFull

    // decode nfield / indexed / unit-stride / strided
    val (vstart, vl)     = (io.mUop.bits.uop.info.vstart, io.mUop.bits.uop.info.vl)
    val (uopIdx, uopEnd) = (io.mUop.bits.uop.uopIdx, io.mUop.bits.uop.uopEnd)
    val (regCount, regStartIdx) = (io.mUopMergeAttr.bits.regCount, io.mUopMergeAttr.bits.regDstIdx)
    
    val ldstCtrl = LSULdstDecoder(io.mUop.bits, io.mUopMergeAttr.bits)
    val mUopInfo = mUopInfoSelecter(io.mUop.bits, io.mUopMergeAttr.bits)

    // * BEGIN
    // * Calculate Addr
    val addr        = WireInit(0.U(addrWidth.W))

    val curVl       = uopIdx
    val baseAddr    = mUopInfo.rs1Val

    // indexed addr
    val idxVal      = WireInit(0.U(XLEN.W))
    val idxMask     = WireInit(0.U(XLEN.W))
    val eew         = ldstCtrl.eewb << 3.U // change eew byte to eew bit
    val beginIdx    = (curVl  - ((curVl >> ldstCtrl.log2Elen) << ldstCtrl.log2Elen)) << (ldstCtrl.log2Eewb +& 3.U)
    idxMask        := (("h1".asUInt(addrWidth.W) << eew) - 1.U)
    idxVal         := (mUopInfo.vs2 >> beginIdx) & idxMask

    // strided addr
    val stride      = WireInit(0.S(XLEN.W))
    val negStride   = stride < 0.S
    val strideAbs   = Mux(negStride, (-stride).asUInt, stride.asUInt)

    stride         := Mux(ldstCtrl.ldstType === Mop.constant_stride, 
                            mUopInfo.rs2Val.asSInt, 11111.S)

    val validLdstSegReq = io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst && ldstCtrl.nfield > 1.U

    when (validLdstSegReq) {
        when (ldstCtrl.ldstType === Mop.unit_stride) {
            when (mUopInfo.segIdx === 0.U && mUopInfo.uopIdx === 0.U) {
                addr := baseAddr
            }.otherwise {
                addr := addrReg + ldstCtrl.memwb
            }
            addrReg := addr
        }.elsewhen (ldstCtrl.ldstType === Mop.constant_stride) {
            when (mUopInfo.segIdx === 0.U && mUopInfo.uopIdx === 0.U) {
                addr := baseAddr
                addrReg := addr
            }.elsewhen (mUopInfo.segIdx === 0.U) {
                addr := Mux(negStride, addrReg - strideAbs, addrReg + strideAbs)
                addrReg := addr
            }.otherwise {
                addr := addrReg + (mUopInfo.segIdx << ldstCtrl.log2Memwb)
            }
        }.elsewhen (ldstCtrl.ldstType === Mop.index_ordered || ldstCtrl.ldstType === Mop.index_unodered) {
            when (mUopInfo.segIdx === 0.U) {
                addr := baseAddr + idxVal
                addrReg := addr
            }.otherwise {
               addr := addrReg + (mUopInfo.segIdx << ldstCtrl.log2Memwb)
            }
        }.otherwise {
            addr := 0.U
        }
    }.otherwise {
        addr := 0.U
    }
 
    val addrMisalign = AddrUtil.isAddrMisalign(addr, ldstCtrl.log2Memwb)
    
    // * Calculate Addr
    // * END


    // store data to queue
    val memw     = ldstCtrl.memwb << 3.U
    val destElem = (curVl  - ((curVl >> ldstCtrl.log2Mlen) << ldstCtrl.log2Mlen))
    val destMask = (1.U << memw) - 1.U
    val destData = (io.mUop.bits.uopRegInfo.old_vd >> (destElem << (ldstCtrl.log2Memwb +& 3.U))) & destMask
    
    // push request to queue
    
    // * if vm=1 => not masked
    // * if vm=0 =>
    // *          v0(i) = 1 => not masked
    // *          v0(i) = 0 => masked
    // * vl = 0 => dummy masked req
    val isVlEq0  = (vl === 0.U)
    val isMasked = Mux(ldstCtrl.vm, false.B, !mUopInfo.mask(curVl)) | isVlEq0
    canEnqueue  := validLdstSegReq && ((curVl >= vstart && curVl < vl) || uopEnd)
    
    val misalignXcpt        = 0.U.asTypeOf(new LdstXcpt)
    misalignXcpt.xcptValid := addrMisalign & ~isMasked
    misalignXcpt.ma        := addrMisalign & ~isMasked

    when (canEnqueue) {
        ldstUopQueue(ldstEnqPtr).valid                  := true.B
        ldstUopQueue(ldstEnqPtr).status                 := Mux(addrMisalign | isMasked, LdstUopStatus.ready, LdstUopStatus.notReady)
        ldstUopQueue(ldstEnqPtr).memOp                  := ldstCtrl.isStore
        ldstUopQueue(ldstEnqPtr).masked                 := isMasked
        ldstUopQueue(ldstEnqPtr).addr                   := addr
        ldstUopQueue(ldstEnqPtr).pos                    := curVl
        ldstUopQueue(ldstEnqPtr).size                   := ldstCtrl.log2Memwb
        ldstUopQueue(ldstEnqPtr).destElem               := destElem
        ldstUopQueue(ldstEnqPtr).data                   := Mux(ldstCtrl.isStore, destData, ldstUopQueue(ldstEnqPtr).data)
        ldstUopQueue(ldstEnqPtr).commitInfo.muopEnd     := uopEnd
        ldstUopQueue(ldstEnqPtr).commitInfo.rfWriteEn   := mUopInfo.rfWriteEn
        ldstUopQueue(ldstEnqPtr).commitInfo.rfWriteIdx  := mUopInfo.ldest
        ldstUopQueue(ldstEnqPtr).commitInfo.isFof       := ldstCtrl.unitSMop === UnitStrideMop.fault_only_first
        ldstUopQueue(ldstEnqPtr).commitInfo.xcpt        := misalignXcpt
        ldstUopQueue(ldstEnqPtr).commitInfo.regCount    := regCount
        ldstUopQueue(ldstEnqPtr).commitInfo.regStartIdx := regStartIdx

        ldstEnqPtr  := ldstEnqPtr + 1.U
    }


    // * BEGIN
    // * Issue LdstUop
    val (respLdstPtr, respData) = (io.dataExchange.resp.bits.idx(3, 0), io.dataExchange.resp.bits.data)
    val issueLdstUop    = ldstUopQueue(issueLdstPtr)
    val isNoXcptMaskUop = issueLdstUop.valid & (~issueLdstUop.commitInfo.xcpt.xcptValid) & (~issueLdstUop.masked)
    val isMaskedUop     = issueLdstUop.valid & issueLdstUop.masked
    
    // nack index smaller than issuePtr can replay
    val issue2CommitDist = Mux(issueLdstPtr >= commitPtr, issueLdstPtr - commitPtr, issueLdstPtr + vLdstUopQueueSize.U - commitPtr)
    val nack2CommitDist  = Mux(respLdstPtr  >= commitPtr, respLdstPtr  - commitPtr, respLdstPtr  + vLdstUopQueueSize.U - commitPtr)
    val smallerNack      = issue2CommitDist > nack2CommitDist

    when (io.dataExchange.resp.bits.nack && smallerNack) {
        issueLdstPtr := respLdstPtr
    }.elsewhen(isMaskedUop) {
        issueLdstPtr := issueLdstPtr + 1.U
    }.elsewhen (isNoXcptMaskUop && io.dataExchange.req.ready) {
        issueLdstPtr := issueLdstPtr + 1.U
    }

    // TODO: store waiting resp

    val issueData    = issueLdstUop.data
    val issueDataSz  = (1.U << issueLdstUop.size)
    val issueOffset  = AddrUtil.getAlignedOffset(issueLdstUop.addr)

    val issueWData = issueData << (issueOffset << 3.U)
    val issueWMask = VecInit(Seq.fill(dataBytes)(0.U(1.W)))

    for (i <- 0 until dataBytes) {
        // 1 to write, 0 to skip
        issueWMask(i) := Mux(i.U >= issueOffset && i.U < issueOffset + issueDataSz, 1.U, 0.U)
    }

    val memOp = issueLdstUop.memOp
    io.dataExchange.req.valid       := isNoXcptMaskUop
    io.dataExchange.req.bits.addr   := AddrUtil.getAlignedAddr(issueLdstUop.addr)
    io.dataExchange.req.bits.cmd    := memOp
    io.dataExchange.req.bits.idx    := (1 << 4).U | issueLdstPtr // to figure out hlsu or vlsu
    io.dataExchange.req.bits.data   := Mux(memOp, issueWData, DontCare)
    io.dataExchange.req.bits.mask   := Mux(memOp, issueWMask.asUInt, DontCare)

    // * Issue LdstUop
    // * END


    // * BEGIN
    // * Recv Resp

    val memXcpt = io.dataExchange.xcpt.asUInt.orR
    val isLoadResp = ldstUopQueue(respLdstPtr).memOp === VMemCmd.read
    val isLoadRespDataValid = io.dataExchange.resp.bits.has_data
    val loadComplete  = isLoadResp && isLoadRespDataValid && !memXcpt

    val respDataSz = (1.U << ldstUopQueue(respLdstPtr).size)
    val respLdData = WireInit(0.U(dataWidth.W))
    val respOffset = AddrUtil.getAlignedOffset(ldstUopQueue(respLdstPtr).addr)
    // ldData := io.dataExchange.resp.bits.data((offset + dataSz) << 3.U - 1.U, offset << 3.U)
    respLdData := (respData >> (respOffset << 3.U)) & ((1.U << (respDataSz << 3.U)) - 1.U)
    when (io.dataExchange.resp.valid || memXcpt) {
        ldstUopQueue(respLdstPtr).data   := Mux(loadComplete, respLdData, ldstUopQueue(respLdstPtr).data)
        ldstUopQueue(respLdstPtr).status := LdstUopStatus.ready
        ldstUopQueue(respLdstPtr).commitInfo.xcpt.xcptValid := memXcpt  // misalign xcpt wont get resp
        ldstUopQueue(respLdstPtr).commitInfo.xcpt.fromHellaXcpt(io.dataExchange.xcpt)
    }

    // * Recv Resp
    // * END


    // * BEGIN
    // * Commit
    val canCommit    = ldstUopQueue(commitPtr).valid && ldstUopQueue(commitPtr).status === LdstUopStatus.ready
    val commitXcpt   = canCommit && ldstUopQueue(commitPtr).commitInfo.xcpt.xcptValid
    val commitMasked = canCommit && ldstUopQueue(commitPtr).masked
    
    val commitDestElem    = ldstUopQueue(commitPtr).destElem
    val commitData        = ldstUopQueue(commitPtr).data
    val commitDataSz      = (1.U << ldstUopQueue(commitPtr).size)
    val commitLog2DataSz  = ldstUopQueue(commitPtr).size


    val dataLeftShiftCnt = WireInit(0.U((log2Up(VLEN + 1)).W))
    dataLeftShiftCnt := (commitDestElem << commitLog2DataSz) << 3.U

    val commitWData       = commitData << dataLeftShiftCnt
    val commitWMask       = VecInit(Seq.fill(vlenb)(0.U(1.W)))

    for (i <- 0 until vlenb) {
        commitWMask(i) := ~(i.U >= (commitDestElem << commitLog2DataSz) && i.U < (commitDestElem << commitLog2DataSz) + commitDataSz)
    }

    io.lsuOut.valid             := canCommit
    io.lsuOut.bits.muopEnd      := ldstUopQueue(commitPtr).commitInfo.muopEnd
    io.lsuOut.bits.rfWriteEn    := Mux(commitXcpt, false.B, ldstUopQueue(commitPtr).commitInfo.rfWriteEn)
    io.lsuOut.bits.rfWriteIdx   := ldstUopQueue(commitPtr).commitInfo.rfWriteIdx
    io.lsuOut.bits.data         := commitWData
    io.lsuOut.bits.rfWriteMask  := Mux(commitMasked, Fill(vlenb, 1.U), commitWMask.asUInt)
    io.lsuOut.bits.isSegLoad    := ldstUopQueue(commitPtr).memOp === VMemCmd.read
    io.lsuOut.bits.regCount     := ldstUopQueue(commitPtr).commitInfo.regCount
    io.lsuOut.bits.regStartIdx  := ldstUopQueue(commitPtr).commitInfo.regStartIdx

    val xcptVl   = ldstUopQueue(commitPtr).pos
    val fofValid = ldstUopQueue(commitPtr).commitInfo.isFof && xcptVl > 0.U

    val commitUop = ldstUopQueue(commitPtr)
    val hellaXcpt = commitUop.commitInfo.xcpt.generateHellaXcpt(commitUop.memOp)

    io.lsuOut.bits.xcpt.exception_vld := Mux(commitXcpt, ~fofValid, false.B)
    io.lsuOut.bits.xcpt.xcpt_cause    := Mux(fofValid || !commitXcpt, 0.U.asTypeOf(new HellaCacheExceptions), hellaXcpt)
    io.lsuOut.bits.xcpt.update_vl     := Mux(commitXcpt, fofValid, false.B)
    io.lsuOut.bits.xcpt.update_data   := xcptVl
    io.lsuOut.bits.xcpt.xcpt_addr     := ldstUopQueue(commitPtr).addr

    when (io.lsuOut.fire) {
        when (commitXcpt) {
            // clear ldstUop Queue
            for (i <- 0 until vLdstUopQueueSize) {
                ldstUopQueue(i) := 0.U.asTypeOf(new SegLdstUop)
            }
            ldstEnqPtr   := 0.U
            issueLdstPtr := 0.U
            commitPtr    := 0.U
        }.otherwise {
            commitPtr                      := commitPtr + 1.U
            ldstUopQueue(commitPtr).status := LdstUopStatus.notReady
            ldstUopQueue(commitPtr).valid  := false.B
        }
    }
    // * Commit
    // * END

}
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

class SVVLsu(implicit p: Parameters) extends Module with HasCircularQueuePtrHelper {
    val io = IO(new LdstIO())

    // * BEGIN
    // * signal define

    // save last addr
    val addrReg = RegInit(0.U(addrWidth.W))

    // ldQueue
    class VLSUPtr extends CircularQueuePtr[VLSUPtr](vLdstUopQueueSize)

    val canEnqueue      = WireInit(false.B)
    val enqPtr          = RegInit(0.U.asTypeOf(new VLSUPtr))
    val issuePtr        = RegInit(0.U.asTypeOf(new VLSUPtr))
    val deqPtr          = RegInit(0.U.asTypeOf(new VLSUPtr))
    val ldstUopQueue    = RegInit(VecInit(Seq.fill(vLdstUopQueueSize)(0.U.asTypeOf(new SegLdstUop))))
    val ldstQueueCnt    = distanceBetween(enqPtr, deqPtr)
    val ldstQueueFull   = isFull(enqPtr, deqPtr)

    // * signal define
    // * END
    io.lsuReady      := !ldstQueueFull

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
    val isMasked = Mux(ldstCtrl.vm, false.B, !mUopInfo.mask(curVl))
    val isVlEq0  = (vl === 0.U)
    canEnqueue  := (validLdstSegReq && !(isMasked && !uopEnd) && curVl >= vstart && curVl < vl) | isVlEq0
    
    val misalignXcpt        = 0.U.asTypeOf(new LdstXcpt)
    misalignXcpt.xcptValid := addrMisalign & ~(isMasked | isVlEq0)
    misalignXcpt.ma        := addrMisalign & ~(isMasked | isVlEq0)

    when (canEnqueue) {
        ldstUopQueue(enqPtr.value).valid                  := true.B
        ldstUopQueue(enqPtr.value).status                 := Mux(addrMisalign | isMasked | isVlEq0, LdstUopStatus.ready, LdstUopStatus.notReady)
        ldstUopQueue(enqPtr.value).memOp                  := ldstCtrl.isStore
        ldstUopQueue(enqPtr.value).masked                 := isMasked | isVlEq0
        ldstUopQueue(enqPtr.value).addr                   := addr
        ldstUopQueue(enqPtr.value).pos                    := curVl
        ldstUopQueue(enqPtr.value).size                   := ldstCtrl.log2Memwb
        ldstUopQueue(enqPtr.value).destElem               := destElem
        ldstUopQueue(enqPtr.value).data                   := Mux(ldstCtrl.isStore, destData, ldstUopQueue(enqPtr.value).data)
        ldstUopQueue(enqPtr.value).commitInfo.muopEnd     := uopEnd
        ldstUopQueue(enqPtr.value).commitInfo.rfWriteEn   := mUopInfo.rfWriteEn
        ldstUopQueue(enqPtr.value).commitInfo.rfWriteIdx  := mUopInfo.ldest
        ldstUopQueue(enqPtr.value).commitInfo.isFof       := ldstCtrl.unitSMop === UnitStrideMop.fault_only_first
        ldstUopQueue(enqPtr.value).commitInfo.xcpt        := misalignXcpt
        ldstUopQueue(enqPtr.value).commitInfo.regCount    := regCount
        ldstUopQueue(enqPtr.value).commitInfo.regStartIdx := regStartIdx

        enqPtr  := enqPtr + 1.U
    }


    // * BEGIN
    // * Issue LdstUop
    val respLdstPtr    = 0.U.asTypeOf(new VLSUPtr)
    respLdstPtr.value := io.dataExchange.resp.bits.idx(3, 0)
    respLdstPtr.flag  := io.dataExchange.resp.bits.flag
    val respData       = io.dataExchange.resp.bits.data

    val issueLdstUop    = ldstUopQueue(issuePtr.value)
    val isNoXcptMaskUop = issueLdstUop.valid & (~issueLdstUop.commitInfo.xcpt.xcptValid) & (~issueLdstUop.masked)
    val isMaskedUop     = issueLdstUop.valid & issueLdstUop.masked

    when (io.dataExchange.resp.bits.nack && (respLdstPtr < issuePtr)) {
        issuePtr := respLdstPtr
    }.elsewhen(isMaskedUop) {
        issuePtr := issuePtr + 1.U
    }.elsewhen (isNoXcptMaskUop && io.dataExchange.req.ready) {
        issuePtr := issuePtr + 1.U
    }

    // TODO: store waiting resp
    when (isNoXcptMaskUop) {
        val data    = issueLdstUop.data
        val dataSz  = (1.U << issueLdstUop.size)
        val offset  = AddrUtil.getAlignedOffset(issueLdstUop.addr)

        val wData = data << (offset << 3.U)
        val wMask = VecInit(Seq.fill(8)(0.U(1.W)))

        for (i <- 0 until 8) {
            // 1 to write, 0 to skip
            wMask(i) := Mux(i.U >= offset && i.U < offset + dataSz, 1.U, 0.U)
        }

        val memOp = issueLdstUop.memOp
        io.dataExchange.req.valid       := true.B
        io.dataExchange.req.bits.addr   := AddrUtil.getAlignedAddr(issueLdstUop.addr)
        io.dataExchange.req.bits.cmd    := memOp
        io.dataExchange.req.bits.srcId  := 1.U
        io.dataExchange.req.bits.flag   := issuePtr.flag
        io.dataExchange.req.bits.idx    := issuePtr.value
        io.dataExchange.req.bits.data   := Mux(memOp, wData, DontCare)
        io.dataExchange.req.bits.mask   := Mux(memOp, wMask.asUInt, DontCare)
    }.otherwise {
        io.dataExchange.req.valid       := false.B
        io.dataExchange.req.bits        := DontCare
    }
    // * Issue LdstUop
    // * END


    // * BEGIN
    // * Recv Resp

    val memXcpt = io.dataExchange.xcpt.asUInt.orR
    when (io.dataExchange.resp.valid || memXcpt) {
        val isLoadResp = ldstUopQueue(respLdstPtr.value).memOp === VMemCmd.read
        val isLoadRespDataValid = io.dataExchange.resp.bits.has_data
        val loadComplete  = isLoadResp && isLoadRespDataValid && !memXcpt

        val dataSz = (1.U << ldstUopQueue(respLdstPtr.value).size)
        val ldData = WireInit(0.U(64.W))
        val offset = AddrUtil.getAlignedOffset(ldstUopQueue(respLdstPtr.value).addr)
        // ldData := io.dataExchange.resp.bits.data((offset + dataSz) << 3.U - 1.U, offset << 3.U)
        ldData := (respData >> (offset << 3.U)) & ((1.U << (dataSz << 3.U)) - 1.U)

        ldstUopQueue(respLdstPtr.value).data   := Mux(loadComplete, ldData, ldstUopQueue(respLdstPtr.value).data)
        ldstUopQueue(respLdstPtr.value).status := LdstUopStatus.ready
        ldstUopQueue(respLdstPtr.value).commitInfo.xcpt.xcptValid := memXcpt  // misalign xcpt wont get resp
        ldstUopQueue(respLdstPtr.value).commitInfo.xcpt.fromHellaXcpt(io.dataExchange.xcpt)
    }

    // * Recv Resp
    // * END


    // * BEGIN
    // * Commit
    val canCommit    = ldstUopQueue(deqPtr.value).valid && ldstUopQueue(deqPtr.value).status === LdstUopStatus.ready
    val commitXcpt   = canCommit && ldstUopQueue(deqPtr.value).commitInfo.xcpt.xcptValid
    val commitMasked = canCommit && ldstUopQueue(deqPtr.value).masked
    

    when (canCommit) {
        val destElem    = ldstUopQueue(deqPtr.value).destElem
        val data        = ldstUopQueue(deqPtr.value).data
        val dataSz      = (1.U << ldstUopQueue(deqPtr.value).size)
        val log2DataSz  = ldstUopQueue(deqPtr.value).size
    
        val wData       = data << ((destElem << log2DataSz) << 3.U)
        val wMask       = VecInit(Seq.fill(vlenb)(0.U(1.W)))

        for (i <- 0 until vlenb) {
            wMask(i) := ~(i.U >= (destElem << log2DataSz) && i.U < (destElem << log2DataSz) + dataSz)
        }

        io.lsuOut.valid             := true.B
        io.lsuOut.bits.muopEnd      := ldstUopQueue(deqPtr.value).commitInfo.muopEnd
        io.lsuOut.bits.rfWriteEn    := Mux(commitXcpt, false.B, ldstUopQueue(deqPtr.value).commitInfo.rfWriteEn)
        io.lsuOut.bits.rfWriteIdx   := ldstUopQueue(deqPtr.value).commitInfo.rfWriteIdx
        io.lsuOut.bits.data         := wData
        io.lsuOut.bits.rfWriteMask  := Mux(commitMasked, Fill(vlenb, 1.U), wMask.asUInt)
        io.lsuOut.bits.isSegLoad    := ldstUopQueue(deqPtr.value).memOp === VMemCmd.read
        io.lsuOut.bits.regCount     := ldstUopQueue(deqPtr.value).commitInfo.regCount
        io.lsuOut.bits.regStartIdx  := ldstUopQueue(deqPtr.value).commitInfo.regStartIdx
    }.otherwise {
        io.lsuOut.valid             := false.B
        io.lsuOut.bits              := DontCare
        io.lsuOut.bits.rfWriteEn    := false.B
    }

    when (commitXcpt) {
        val xcptVl   = ldstUopQueue(deqPtr.value).pos
        val fofValid = ldstUopQueue(deqPtr.value).commitInfo.isFof && xcptVl > 0.U

        val commitUop = ldstUopQueue(deqPtr.value)
        val hellaXcpt = commitUop.commitInfo.xcpt.generateHellaXcpt(commitUop.memOp)

        io.lsuOut.bits.xcpt.exception_vld := ~fofValid
        io.lsuOut.bits.xcpt.xcpt_cause    := Mux(fofValid, 0.U.asTypeOf(new HellaCacheExceptions), hellaXcpt)
        io.lsuOut.bits.xcpt.update_vl     := fofValid
        io.lsuOut.bits.xcpt.update_data   := xcptVl
        io.lsuOut.bits.xcpt.xcpt_addr     := ldstUopQueue(deqPtr.value).addr

        // clear ldstUop Queue
        for (i <- 0 until vLdstUopQueueSize) {
            ldstUopQueue(i) := 0.U.asTypeOf(new SegLdstUop)
        }
    }.otherwise {
        io.lsuOut.bits.xcpt := 0.U.asTypeOf(new VLSUXcpt)
    }


    when (io.lsuOut.fire) {
        deqPtr                            := deqPtr + 1.U
        ldstUopQueue(deqPtr.value).status := LdstUopStatus.notReady
        ldstUopQueue(deqPtr.value).valid  := false.B
    }
    // * Commit
    // * END

}
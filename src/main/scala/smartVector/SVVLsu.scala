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

class VLSUPtr extends CircularQueuePtr[VLSUPtr](vVLSUQueueEntries)

class SVVLsu(
    implicit p: Parameters
) extends Module with HasCircularQueuePtrHelper {
  val io = IO(new LSUIO)

  // * BEGIN
  // * signal define

  // save last addr
  val addrReg = RegInit(0.U(addrWidth.W))

  // ldQueue
  val canEnqueue   = WireInit(false.B)
  val enqPtr       = RegInit(0.U.asTypeOf(new VLSUPtr))
  val issuePtr     = RegInit(0.U.asTypeOf(new VLSUPtr))
  val deqPtr       = RegInit(0.U.asTypeOf(new VLSUPtr))
  val ldstUopQueue = RegInit(VecInit(Seq.fill(vVLSUQueueEntries)(0.U.asTypeOf(new SegLdstUop))))

  val ldstQueueFull = isFull(enqPtr, deqPtr)

  // * signal define
  // * END
  io.lsuReq.ready := !ldstQueueFull

  // decode nfield / indexed / unit-stride / strided
  val (vstart, vl)            = (io.lsuReq.bits.vstart, io.lsuReq.bits.vl)
  val (uopIdx, uopEnd)        = (io.lsuReq.bits.uopIdx, io.lsuReq.bits.uopEnd)
  val (regCount, regStartIdx) = (io.lsuReq.bits.muopInfo.regCount, io.lsuReq.bits.muopInfo.regDstIdx)

  val ldstCtrl = io.lsuReq.bits.ldstCtrl
  val mUopInfo = io.lsuReq.bits.muopInfo

  // * BEGIN
  // * Calculate Addr
  val addr = WireInit(0.U(addrWidth.W))

  val curVl    = uopIdx
  val baseAddr = mUopInfo.rs1Val

  // indexed addr
  val idxVal   = WireInit(0.U(XLEN.W))
  val idxMask  = WireInit(0.U(XLEN.W))
  val eew      = ldstCtrl.eewb << 3.U // change eew byte to eew bit
  val beginIdx = (curVl - ((curVl >> ldstCtrl.log2Elen) << ldstCtrl.log2Elen)) << (ldstCtrl.log2Eewb +& 3.U)
  idxMask := (("h1".asUInt(addrWidth.W) << eew) - 1.U)
  idxVal  := (mUopInfo.vs2 >> beginIdx) & idxMask

  // strided addr
  val stride    = WireInit(0.S(XLEN.W))
  val negStride = stride < 0.S
  val strideAbs = Mux(negStride, (-stride).asUInt, stride.asUInt)

  stride := Mux(ldstCtrl.ldstType === Mop.constant_stride, mUopInfo.rs2Val.asSInt, 11111.S)

  when(io.lsuReq.fire) {
    when(ldstCtrl.ldstType === Mop.unit_stride) {
      when(mUopInfo.segIdx === 0.U && mUopInfo.uopIdx === 0.U) {
        addr := baseAddr
      }.otherwise {
        addr := addrReg + ldstCtrl.memwb
      }
      addrReg := addr
    }.elsewhen(ldstCtrl.ldstType === Mop.constant_stride) {
      when(mUopInfo.segIdx === 0.U && mUopInfo.uopIdx === 0.U) {
        addr    := baseAddr
        addrReg := addr
      }.elsewhen(mUopInfo.segIdx === 0.U) {
        addr    := Mux(negStride, addrReg - strideAbs, addrReg + strideAbs)
        addrReg := addr
      }.otherwise {
        addr := addrReg + (mUopInfo.segIdx << ldstCtrl.log2Memwb)
      }
    }.elsewhen(ldstCtrl.ldstType === Mop.index_ordered || ldstCtrl.ldstType === Mop.index_unodered) {
      when(mUopInfo.segIdx === 0.U) {
        addr    := baseAddr + idxVal
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
  val destElem = curVl - ((curVl >> ldstCtrl.log2Mlen) << ldstCtrl.log2Mlen)
  val destMask = (1.U << memw) - 1.U
  val destData = (mUopInfo.old_vd >> (destElem << (ldstCtrl.log2Memwb +& 3.U))) & destMask

  // push request to queue

  // * if vm=1 => not masked
  // * if vm=0 =>
  // *          v0(i) = 1 => not masked
  // *          v0(i) = 0 => masked
  // * vl = 0 => dummy masked req
  val isVlEq0  = vl === 0.U
  val isMasked = Mux(ldstCtrl.vm, false.B, !mUopInfo.mask(curVl)) | isVlEq0
  canEnqueue := io.lsuReq.fire && ((curVl >= vstart && curVl < vl) || uopEnd)

  val misalignXcpt = 0.U.asTypeOf(new LdstXcpt)
  misalignXcpt.xcptValid := addrMisalign & ~isMasked
  misalignXcpt.ma        := addrMisalign & ~isMasked

  when(canEnqueue) {
    ldstUopQueue(enqPtr.value).valid    := true.B
    ldstUopQueue(enqPtr.value).status   := Mux(addrMisalign | isMasked, LdstUopStatus.ready, LdstUopStatus.notReady)
    ldstUopQueue(enqPtr.value).memOp    := ldstCtrl.isStore
    ldstUopQueue(enqPtr.value).masked   := isMasked
    ldstUopQueue(enqPtr.value).addr     := addr
    ldstUopQueue(enqPtr.value).pos      := curVl
    ldstUopQueue(enqPtr.value).size     := ldstCtrl.log2Memwb
    ldstUopQueue(enqPtr.value).destElem := destElem
    ldstUopQueue(enqPtr.value).data     := Mux(ldstCtrl.isStore, destData, ldstUopQueue(enqPtr.value).data)
    ldstUopQueue(enqPtr.value).commitInfo.muopEnd     := uopEnd
    ldstUopQueue(enqPtr.value).commitInfo.rfWriteEn   := mUopInfo.rfWriteEn
    ldstUopQueue(enqPtr.value).commitInfo.rfWriteIdx  := mUopInfo.ldest
    ldstUopQueue(enqPtr.value).commitInfo.isFof       := ldstCtrl.unitSMop === UnitStrideMop.fault_only_first
    ldstUopQueue(enqPtr.value).commitInfo.xcpt        := misalignXcpt
    ldstUopQueue(enqPtr.value).commitInfo.regCount    := regCount
    ldstUopQueue(enqPtr.value).commitInfo.regStartIdx := regStartIdx

    enqPtr := enqPtr + 1.U
  }

  // * BEGIN
  // * Issue LdstUop
  val respLdstPtr = 0.U.asTypeOf(new VLSUPtr)
  respLdstPtr.value := io.dataExchange.resp.bits.idx(nLSUMaxQueueWidth - 1, 0)
  respLdstPtr.flag  := io.dataExchange.resp.bits.flag
  val respData = io.dataExchange.resp.bits.data

  val issueLdstUop    = ldstUopQueue(issuePtr.value)
  val isNoXcptMaskUop = issueLdstUop.valid & (~issueLdstUop.commitInfo.xcpt.xcptValid) & (~issueLdstUop.masked)
  val isMaskedUop     = issueLdstUop.valid & issueLdstUop.masked

  when(io.dataExchange.resp.bits.nack && (respLdstPtr < issuePtr)) {
    issuePtr := respLdstPtr
  }.elsewhen(isMaskedUop) {
    issuePtr := issuePtr + 1.U
  }.elsewhen(isNoXcptMaskUop && io.dataExchange.req.ready) {
    issuePtr := issuePtr + 1.U
  }

  // TODO: store waiting resp

  val issueData   = issueLdstUop.data
  val issueDataSz = 1.U << issueLdstUop.size
  val issueOffset = AddrUtil.getAlignedOffset(issueLdstUop.addr)

  val issueWData = issueData << (issueOffset << 3.U)
  val issueWMask = VecInit(Seq.fill(dataBytes)(0.U(1.W)))

  for (i <- 0 until dataBytes) {
    // 1 to write, 0 to skip
    issueWMask(i) := Mux(i.U >= issueOffset && i.U < issueOffset + issueDataSz, 1.U, 0.U)
  }

  val memOp = issueLdstUop.memOp
  io.dataExchange.req.valid      := isNoXcptMaskUop
  io.dataExchange.req.bits.addr  := AddrUtil.getAlignedAddr(issueLdstUop.addr)
  io.dataExchange.req.bits.cmd   := memOp
  io.dataExchange.req.bits.srcId := 1.U
  io.dataExchange.req.bits.flag  := issuePtr.flag
  io.dataExchange.req.bits.idx   := issuePtr.value
  io.dataExchange.req.bits.data  := Mux(memOp, issueWData, DontCare)
  io.dataExchange.req.bits.mask  := Mux(memOp, issueWMask.asUInt, DontCare)

  // * Issue LdstUop
  // * END

  // * BEGIN
  // * Recv Resp

  val memXcpt             = io.dataExchange.xcpt.asUInt.orR
  val isLoadResp          = ldstUopQueue(respLdstPtr.value).memOp === VMemCmd.read
  val isLoadRespDataValid = io.dataExchange.resp.bits.has_data
  val loadComplete        = isLoadResp && isLoadRespDataValid && !memXcpt

  val respDataSz = 1.U << ldstUopQueue(respLdstPtr.value).size
  val respLdData = WireInit(0.U(dataWidth.W))
  val respOffset = AddrUtil.getAlignedOffset(ldstUopQueue(respLdstPtr.value).addr)
  // ldData := io.dataExchange.resp.bits.data((offset + dataSz) << 3.U - 1.U, offset << 3.U)
  respLdData := (respData >> (respOffset << 3.U)) & ((1.U << (respDataSz << 3.U)) - 1.U)
  when(io.dataExchange.resp.valid || memXcpt) {
    ldstUopQueue(respLdstPtr.value).data   := Mux(loadComplete, respLdData, ldstUopQueue(respLdstPtr.value).data)
    ldstUopQueue(respLdstPtr.value).status := LdstUopStatus.ready
    ldstUopQueue(respLdstPtr.value).commitInfo.xcpt.xcptValid := memXcpt // misalign xcpt wont get resp
    ldstUopQueue(respLdstPtr.value).commitInfo.xcpt.fromHellaXcpt(io.dataExchange.xcpt)
  }

  // * Recv Resp
  // * END

  // * BEGIN
  // * Commit
  val canCommit    = ldstUopQueue(deqPtr.value).valid && ldstUopQueue(deqPtr.value).status === LdstUopStatus.ready
  val commitXcpt   = canCommit && ldstUopQueue(deqPtr.value).commitInfo.xcpt.xcptValid
  val commitMasked = canCommit && ldstUopQueue(deqPtr.value).masked

  val commitDestElem   = ldstUopQueue(deqPtr.value).destElem
  val commitData       = ldstUopQueue(deqPtr.value).data
  val commitDataSz     = 1.U << ldstUopQueue(deqPtr.value).size
  val commitLog2DataSz = ldstUopQueue(deqPtr.value).size

  val dataLeftShiftCnt = WireInit(0.U(log2Up(VLEN + 1).W))
  dataLeftShiftCnt := (commitDestElem << commitLog2DataSz) << 3.U

  val commitWData = commitData << dataLeftShiftCnt
  val commitWMask = VecInit(Seq.fill(vlenb)(0.U(1.W)))

  for (i <- 0 until vlenb) {
    commitWMask(
      i
    ) := ~(i.U >= (commitDestElem << commitLog2DataSz) && i.U < (commitDestElem << commitLog2DataSz) + commitDataSz)
  }

  io.lsuOut.valid            := canCommit
  io.lsuOut.bits.muopEnd     := ldstUopQueue(deqPtr.value).commitInfo.muopEnd
  io.lsuOut.bits.rfWriteEn   := Mux(commitXcpt, false.B, ldstUopQueue(deqPtr.value).commitInfo.rfWriteEn)
  io.lsuOut.bits.rfWriteIdx  := ldstUopQueue(deqPtr.value).commitInfo.rfWriteIdx
  io.lsuOut.bits.data        := commitWData
  io.lsuOut.bits.rfWriteMask := Mux(commitMasked, Fill(vlenb, 1.U), commitWMask.asUInt)
  io.lsuOut.bits.isSegLoad   := ldstUopQueue(deqPtr.value).memOp === VMemCmd.read
  io.lsuOut.bits.regCount    := ldstUopQueue(deqPtr.value).commitInfo.regCount
  io.lsuOut.bits.regStartIdx := ldstUopQueue(deqPtr.value).commitInfo.regStartIdx

  val xcptVl   = ldstUopQueue(deqPtr.value).pos
  val fofValid = ldstUopQueue(deqPtr.value).commitInfo.isFof && xcptVl > 0.U

  val commitUop = ldstUopQueue(deqPtr.value)
  val hellaXcpt = commitUop.commitInfo.xcpt.generateHellaXcpt(commitUop.memOp)

  io.lsuOut.bits.xcpt.exception_vld := Mux(commitXcpt, ~fofValid, false.B)
  io.lsuOut.bits.xcpt.xcpt_cause    := Mux(fofValid || !commitXcpt, 0.U.asTypeOf(new HellaCacheExceptions), hellaXcpt)
  io.lsuOut.bits.xcpt.update_vl     := Mux(commitXcpt, fofValid, false.B)
  io.lsuOut.bits.xcpt.update_data   := xcptVl
  io.lsuOut.bits.xcpt.xcpt_addr     := ldstUopQueue(deqPtr.value).addr

  when(io.lsuOut.fire) {
    when(commitXcpt) {
      // clear ldstUop Queue
      for (i <- 0 until vVLSUQueueEntries) {
        ldstUopQueue(i) := 0.U.asTypeOf(new SegLdstUop)
      }
      enqPtr   := 0.U.asTypeOf(new VLSUPtr)
      issuePtr := 0.U.asTypeOf(new VLSUPtr)
      deqPtr   := 0.U.asTypeOf(new VLSUPtr)
    }.otherwise {
      deqPtr                            := deqPtr + 1.U
      ldstUopQueue(deqPtr.value).status := LdstUopStatus.notReady
      ldstUopQueue(deqPtr.value).valid  := false.B
    }
  }
  // * Commit
  // * END

}

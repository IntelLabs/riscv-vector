/***************************************************************************************
*Copyright (c) 2023-2024 Intel Corporation
*Vector Acceleration IP core for RISC-V* is licensed under Mulan PSL v2.
*You can use this software according to the terms and conditions of the Mulan PSL v2.
*You may obtain a copy of Mulan PSL v2 at:
*        http://license.coscl.org.cn/MulanPSL2
*THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
*EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
*MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*See the Mulan PSL v2 for more details.
***************************************************************************************/

/** OVI (Open Vector Interface) adoption
 *    Every instrucion in VRob will finally be commited OR flushed
 *  Notes:
 *    1) following signal does not support VCommitWidth > 2
 *      a) deqPtrRhb
 *      b) io.commits.valid
 *    2) Currently the ovi_completed is ordered 
 *  Features: 
 *    1) Decoupled rename history buffer
 *    2) Small rd (scalar dest) buffer, source of io.fromDispatch.ready
 */
package darecreek

import chisel3._
import chisel3.util._
import utils._
import darecreek.util._

class OviCompltSigs extends Bundle {
  val fflags = UInt(5.W)
  val vxsat = Bool()
}
class RenameHistoryBufferEntry extends Bundle {
  val sb_id = UInt(5.W)  // only need by assertion
  val pdestVal = Bool()
  val ldest = UInt(5.W)
  val pdest = UInt(VPRegIdxWidth.W)
  val old_pdest = UInt(VPRegIdxWidth.W)
}
class RdBufferEntry extends Bundle {
  val rd = UInt(xLen.W)
  val robPtr = new VRobPtr 
}

class VRob extends Module with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // from decoder
    val in = new Bundle {
      val valid = Input(Bool())
      val sb_id = Input(UInt(5.W))
      val ldestVal = Input(Bool())
      val rdVal = Input(Bool())
    }
    // OVI dispatch
    val ovi_dispatch = new OVIdispatch
    // from VIllegalInstrn.io.ill
    val illegal = Flipped(ValidIO(new VRobPtr))
    // extract from VInfoCalc.io.infoAll
    val partialVInfo = Flipped(ValidIO(new PartialVInfo))
    // from Rename block
    val fromDispatch = Vec(VRenameWidth, Flipped(Decoupled(new VExpdUOp)))
    // writebacks from EXU and LSU
    val wbArith_lane = Input(ValidIO(new WbArith_lane))
    val wbArith_cross = Input(ValidIO(new WbArith_cross))
    val wbLSU = Input(ValidIO(new VExpdUOp))
    // OVI completed
    val ovi_completed = new OVIcompleted
    // Vec ROB commits
    val commits = new VRobCommitIO
    val flush = ValidIO(new VRobPtr)
    // OVI issue credit
    val ovi_issueCredit = Output(Bool())
    // Just for debug
    val rvfi_sb_id = Output(UInt(5.W))
    val commitEnd = Output(Bool())
  })

  // VQ has the same length with Vec ROB
  val sb_id = Reg(Vec(VQSize, UInt(5.W)))
  val valid = RegInit(VecInit(Seq.fill(VQSize)(false.B)))
  val busy = Reg(Vec(VQSize, Bool()))
  val illegal = Reg(Vec(VQSize, Bool()))
  val NEXT_SENIOR = 1.U(2.W)
  val KILL = 2.U(2.W)
  val senior_or_kill = RegInit(VecInit(Seq.fill(VQSize)(0.U(2.W))))
  val oviCompltSigs = Reg(Vec(VQSize, new OviCompltSigs))
  val ldestVal = Reg(Vec(VQSize, Bool()))
  val emulVd = RegInit(VecInit(Seq.fill(VQSize)(0.U(4.W))))

  /**
    * Enq
    */
  val enqPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  when (io.in.valid) {
    sb_id(enqPtr.value) := io.in.sb_id
    valid(enqPtr.value) := true.B
    busy(enqPtr.value) := true.B
    illegal(enqPtr.value) := false.B
    oviCompltSigs(enqPtr.value) := 0.U.asTypeOf(new OviCompltSigs)
    ldestVal(enqPtr.value) := io.in.ldestVal
  }
  when (io.in.valid) { enqPtr := enqPtr + 1.U }

  /**
    * Dispatch from CPU
    */
  val dispatchValid = io.ovi_dispatch.next_senior || io.ovi_dispatch.kill
  val dispthAddrOH = Wire(UInt(VQSize.W)) // One-hot
  dispthAddrOH := UIntToOH(io.ovi_dispatch.sb_id, VQSize) & Fill(VQSize, dispatchValid)
  senior_or_kill zip dispthAddrOH.asBools map { case (d, v) => 
    when (v) { d := Cat(io.ovi_dispatch.kill, io.ovi_dispatch.next_senior) }
  }

  // Illegal instruction
  when (io.illegal.valid) {
    illegal(io.illegal.bits.value) := true.B
    busy(io.illegal.bits.value) := false.B
  }
  // Partial VInfo
  when (io.partialVInfo.valid) {
    emulVd(io.partialVInfo.bits.vRobPtr.value) := io.partialVInfo.bits.emulVd
  }

  /**
   * Rename History Buffer
   */
  val rhb = RegInit(VecInit.fill(NVPhyRegs)(0.U.asTypeOf(new RenameHistoryBufferEntry)))
  class VRhbPtr extends CircularQueuePtr[VRhbPtr](NVPhyRegs) 
  val enqPtrRhb = RegInit(0.U.asTypeOf(new VRhbPtr))
  val deqPtrRhb = RegInit(0.U.asTypeOf(new VRhbPtr))
  val uopExtractRhb = Wire(Vec(VRenameWidth, new RenameHistoryBufferEntry))
  uopExtractRhb zip io.fromDispatch.map(_.bits) map { case (y, x) =>
    y.sb_id := x.sb_id
    y.pdestVal := x.pdestVal
    y.ldest := x.ldestExpd
    y.pdest := x.pdest
    y.old_pdest := x.psrc(2)
  }

  // Enq of RHB
  val enqPtrRhbOHs = Wire(Vec(VRenameWidth, UInt(NVPhyRegs.W)))
  enqPtrRhbOHs(0) := enqPtrRhb.toOH
  val enqRhbFire = io.fromDispatch.map(x => x.fire && x.bits.pdestVal)
  val enqRhbPopCnt = PopCount(enqRhbFire)
  for (i <- 1 until VRenameWidth) {
    enqPtrRhbOHs(i) := Mux(enqRhbFire(i-1), CircularShift.left(enqPtrRhbOHs(i-1), 1), enqPtrRhbOHs(i-1))
  }
  for (i <- 0 until NVPhyRegs) {
    rhb(i) := WriteRegFile.oneHot(enqRhbFire, enqPtrRhbOHs.map(_(i)), uopExtractRhb, rhb(i))
  }
  enqPtrRhb := enqPtrRhb + enqRhbPopCnt

  /** rd buffer */
  val RdBufSize = 4
  val rdBuf = Reg(Vec(RdBufSize, new RdBufferEntry))
  class RdBufPtr extends CircularQueuePtr[RdBufPtr](RdBufSize)
  val enqPtrRdBuf = RegInit(0.U.asTypeOf(new RdBufPtr))
  val deqPtrRdBuf = RegInit(0.U.asTypeOf(new RdBufPtr))
  when(io.in.valid && io.in.rdVal) {
    rdBuf(enqPtrRdBuf.value).robPtr := enqPtr
    enqPtrRdBuf := enqPtrRdBuf + 1.U
  }
  val wbA_lane_rdBuf_oneHot = rdBuf.map(_.robPtr === io.wbArith_lane.bits.uop.vRobIdx)
  rdBuf zip wbA_lane_rdBuf_oneHot map { case (entry, hit) => 
    when (hit && io.wbArith_lane.valid && io.wbArith_lane.bits.uop.ctrl.rdVal) {
      entry.rd := io.wbArith_lane.bits.rd
    }
  }
  val wbA_cross_rdBuf_oneHot = rdBuf.map(_.robPtr === io.wbArith_cross.bits.uop.vRobIdx)
  rdBuf zip wbA_cross_rdBuf_oneHot map { case (entry, hit) => 
    when (hit && io.wbArith_cross.valid && io.wbArith_cross.bits.uop.ctrl.rdVal) {
      entry.rd := io.wbArith_cross.bits.rd
    }
  }
  val emptyRdBuf = isEmpty(enqPtrRdBuf, deqPtrRdBuf)
  val fullRdBuf = isFull(enqPtrRdBuf, deqPtrRdBuf)
  io.fromDispatch.foreach(_.ready := !fullRdBuf)

  /**
    * Write back
    * Note: So far, set constant 0 to OVI_COMPLETED.vstart (does not support load retry)
    */
  //---- Write-back of arith_lane ----
  val wbA_lane = io.wbArith_lane.bits.uop
  val wbA_lane_valid = io.wbArith_lane.valid
  val wbA_lane_vRobIdx_value = wbA_lane.vRobIdx.value
  // Only support in-order write-backs under the same instruction
  when (wbA_lane_valid) { busy(wbA_lane_vRobIdx_value) := !wbA_lane.expdEnd }
  val wbA_lane_oviComplt = Cat(io.wbArith_lane.bits.fflags, io.wbArith_lane.bits.vxsat).asTypeOf(new OviCompltSigs)
  when (wbA_lane_valid) { oviCompltSigs(wbA_lane_vRobIdx_value) := wbA_lane_oviComplt }
  //---- Write-back of arith_cross ----
  val wbA_cross = io.wbArith_cross.bits.uop
  val wbA_cross_valid = io.wbArith_cross.valid
  val wbA_cross_vRobIdx_value = wbA_cross.vRobIdx.value
  // Only support in-order write-backs under the same instruction
  when (wbA_cross_valid) { busy(wbA_cross_vRobIdx_value) := !wbA_cross.expdEnd }
  val wbA_cross_oviComplt = Cat(io.wbArith_cross.bits.fflags, false.B).asTypeOf(new OviCompltSigs)
  when (wbA_cross_valid) { oviCompltSigs(wbA_cross_vRobIdx_value) := wbA_cross_oviComplt }
  //---- Write back of ld/st ----
  val wbLs = io.wbLSU.bits
  val wbLs_valid = io.wbLSU.valid
  val wbLs_vRobIdx_value = wbLs.vRobIdx.value
  // Only support in-order write-backs under the same instruction
  when (wbLs_valid) { busy(wbLs_vRobIdx_value) := !wbLs.expdEnd }

  /**
    * Complete
    */
  // --From OVI spec: for any instruction, the completed.valid signal can only be set after receiving a
  //   dispatch.next_senior with the sb_id matching the instruction. Conversely, a completed.valid signal 
  //   will not be set for any instruction that receives a dispatch.kill.
  val compltPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  val canComplete = senior_or_kill(compltPtr.value) === NEXT_SENIOR && !busy(compltPtr.value) && valid(compltPtr.value)
  val ovi_completed = Wire(new OVIcompleted)
  ovi_completed.valid := canComplete
  ovi_completed.sb_id := sb_id(compltPtr.value)
  ovi_completed.illegal := illegal(compltPtr.value)
  // !! Fake !!
  ovi_completed.vstart := 0.U  // !! Fake
  ovi_completed.fflags := oviCompltSigs(compltPtr.value).fflags
  ovi_completed.vxsat := oviCompltSigs(compltPtr.value).vxsat
  ovi_completed.dest_reg := rdBuf(deqPtrRdBuf.value).rd
  when (canComplete || valid(compltPtr.value) && senior_or_kill(compltPtr.value) === KILL) {
    compltPtr := compltPtr + 1.U
  }
  when (ovi_completed.valid && !emptyRdBuf && compltPtr === rdBuf(deqPtrRdBuf.value).robPtr) {
    deqPtrRdBuf := deqPtrRdBuf + 1.U
  }
  io.ovi_completed := RegEnable(ovi_completed, ovi_completed.valid)
  io.ovi_completed.valid := RegNext(ovi_completed.valid)

  /**
    * Commit
    */
  // Deq pointer (commit or flush)
  val deqPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  val expdCnt = RegInit(0.U(3.W))
  // Can commit for expanded instructions (may need multiple cycles)
  val canCommit = valid(deqPtr.value) && (senior_or_kill(deqPtr.value) === NEXT_SENIOR && !busy(deqPtr.value))
  val vdRemain = emulVd(deqPtr.value) - expdCnt
  // Commit finish, deq pointer can + 1
  val commitEnd = canCommit && (vdRemain <= VCommitWidth.U || !ldestVal(deqPtr.value))
  when (commitEnd) {
    expdCnt := 0.U
  }.elsewhen(canCommit) {
    expdCnt := expdCnt + VCommitWidth.U
  }
  when (commitEnd) {
    valid(deqPtr.value) := false.B
    senior_or_kill(deqPtr.value) := 0.U
  }
  val commitInfo = Wire(new VRobCommitIO)

  // !! Only works for VCommitWidth = 2
  when (canCommit && ldestVal(deqPtr.value)) { 
      deqPtrRhb := deqPtrRhb + Mux(vdRemain === 1.U, 1.U, VCommitWidth.U)
  }
  when (canCommit) { 
    commitInfo.valid(0) := true.B
    commitInfo.valid(VCommitWidth - 1) := Mux(vdRemain === 1.U, false.B, true.B)
  }.otherwise {
    commitInfo.valid.foreach(_ := false.B)
  }

  for (i <- 0 until VCommitWidth) {
    commitInfo.info(i).pdestVal := rhb((deqPtrRhb + i.U).value).pdestVal
    commitInfo.info(i).ldest := rhb((deqPtrRhb + i.U).value).ldest
    commitInfo.info(i).pdest := rhb((deqPtrRhb + i.U).value).pdest
    commitInfo.info(i).old_pdest := rhb((deqPtrRhb + i.U).value).old_pdest
  }
  io.commits.valid := RegNext(commitInfo.valid)
  io.commits.info := RegEnable(commitInfo.info, commitInfo.valid(0))

  /** Flush */
  val canFlush = valid(deqPtr.value) && senior_or_kill(deqPtr.value) === KILL
  val flush = canFlush
  io.flush.valid := RegNext(canFlush)
  io.flush.bits := RegEnable(deqPtr, flush)

  // Update deq status
  when (commitEnd || flush) { 
    deqPtr := deqPtr + 1.U
    valid(deqPtr.value) := false.B
    senior_or_kill(deqPtr.value) := 0.U
  }

  // Clear rhb/rdBuf when flush
  when (flush) {
    enqPtrRhb := 0.U.asTypeOf(new VRhbPtr)
    deqPtrRhb := 0.U.asTypeOf(new VRhbPtr)
    enqPtrRdBuf := 0.U.asTypeOf(new RdBufPtr)
    deqPtrRdBuf := 0.U.asTypeOf(new RdBufPtr)
  }

  // Credit
  val credit = RegInit(false.B)
  credit := commitEnd || flush
  io.ovi_issueCredit := credit

  // Assertions
  assertWhen(commitInfo.valid(0) && commitInfo.info(0).pdestVal,
             rhb(deqPtrRhb.value).sb_id === sb_id(deqPtr.value), "Error: VRob sb_id not matched!")

  /**
    *  Debug
    */
  io.rvfi_sb_id := RegNext(sb_id(deqPtr.value))
  io.commitEnd := RegNext(commitEnd)
}

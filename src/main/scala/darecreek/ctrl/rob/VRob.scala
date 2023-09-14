/**
  * OVI (Open Vector Interface) adoption
  */

package darecreek

import chisel3._
import chisel3.util._
import utils._
import darecreek.util._

class OviCompltSigs extends Bundle {
  val fflags = UInt(5.W) // Floating-point accrued exception flag
  val vxsat = Bool() // Fixed-point accrued saturation flag
  val rd = UInt(xLen.W)
}

class RenameHistoryBufferEntry extends Bundle {
  val sb_id = UInt(5.W)
  val pdestVal = Bool()
  val ldest = UInt(5.W)
  val pdest = UInt(VPRegIdxWidth.W)
  val old_pdest = UInt(VPRegIdxWidth.W)
}

class VRob extends Module with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    // OVI issue
    val in = new Bundle {
      val sb_id = Input(UInt(5.W))
      val valid = Input(Bool())
    }
    // OVI dispatch
    val ovi_dispatch = new OVIdispatch
    // illegal indicator
    val illegal = Flipped(ValidIO(new VRobPtr))
    // from Rename block
    val fromRename = Vec(VRenameWidth, Input(ValidIO(new VExpdUOp)))
    // writeback from EXU and LSU
    val wbArith = Flipped(ValidIO(new WbArith))
    val wbLSU = Vec(2, Flipped(ValidIO(new VExpdUOp)))
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
  // logic and physical registers
  val expdLen = RegInit(VecInit(Seq.fill(VQSize)(0.U(4.W))))
  val oneCycleCommit = RegInit(VecInit(Seq.fill(VQSize)(false.B)))  //Commit using only one cycle

  val enqPtr = RegInit(0.U.asTypeOf(new VRobPtr))

  /**
    * Enq
    */
  when (io.in.valid) {
    sb_id(enqPtr.value) := io.in.sb_id
    valid(enqPtr.value) := true.B
    busy(enqPtr.value) := true.B
    illegal(enqPtr.value) := false.B
    oviCompltSigs(enqPtr.value) := 0.U.asTypeOf(new OviCompltSigs)
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

  /**
   * Rename History Buffer
   */
  val rhb = RegInit(VecInit.fill(NVPhyRegs)(0.U.asTypeOf(new RenameHistoryBufferEntry)))
  class VRhbPtr extends CircularQueuePtr[VRhbPtr](NVPhyRegs) 
  val enqPtrRhb = RegInit(0.U.asTypeOf(new VRhbPtr))
  val deqPtrRhb = RegInit(0.U.asTypeOf(new VRhbPtr))
  val uopExtractRhb = Wire(Vec(VRenameWidth, new RenameHistoryBufferEntry))
  uopExtractRhb zip io.fromRename.map(_.bits) map { case (y, x) =>
    y.sb_id := x.sb_id
    y.pdestVal := x.pdestVal
    y.ldest := x.ldestExpd
    y.pdest := x.pdest
    y.old_pdest := x.psrc(2)
  }

  // Enq of RHB
  val enqPtrRhbOHs = Wire(Vec(VRenameWidth, UInt(NVPhyRegs.W)))
  enqPtrRhbOHs(0) := enqPtrRhb.toOH
  val enqRhbFire = io.fromRename.map(x => x.valid && x.bits.pdestVal)
  val enqRhbPopCnt = PopCount(enqRhbFire)
  for (i <- 1 until VRenameWidth) {
    enqPtrRhbOHs(i) := Mux(enqRhbFire(i-1), CircularShift.left(enqPtrRhbOHs(i-1), 1), enqPtrRhbOHs(i-1))
  }
  for (i <- 0 until NVPhyRegs) {
    rhb(i) := WriteRegFile.oneHot(enqRhbFire, enqPtrRhbOHs.map(_(i)), uopExtractRhb, rhb(i))
  }
  enqPtrRhb := enqPtrRhb + enqRhbPopCnt

  /**
    * Write back
    */
  // Write back of arith
  val wbA = io.wbArith.bits.uop
  when (io.wbArith.valid) {
    busy(wbA.vRobIdx.value) := !wbA.expdEnd  // Only support in-order write-backs under the same instruction
    expdLen(wbA.vRobIdx.value) := wbA.expdLen
    oneCycleCommit(wbA.vRobIdx.value) := wbA.ctrl.narrow_to_1 || wbA.ctrl.redu || wbA.expdLen === 1.U
    oviCompltSigs(wbA.vRobIdx.value) := Cat(io.wbArith.bits.fflags, io.wbArith.bits.vxsat,
                                            io.wbArith.bits.rd).asTypeOf(new OviCompltSigs)
  }
  // Write back of ld
  val wbL = io.wbLSU(0).bits
  when (io.wbLSU(0).valid) {
    busy(wbL.vRobIdx.value) := !wbL.expdEnd
    expdLen(wbL.vRobIdx.value) := wbL.expdLen
    oneCycleCommit(wbL.vRobIdx.value) := wbL.expdLen === 1.U
  }
  // Write back of st
  val wbS = io.wbLSU(1).bits
  when (io.wbLSU(1).valid) {
    busy(wbS.vRobIdx.value) := !wbS.expdEnd
    expdLen(wbS.vRobIdx.value) := wbS.expdLen
    oneCycleCommit(wbS.vRobIdx.value) := true.B
  }

  /**
    * Complete
    */
  // --From OVI spec: for any instruction, the completed.valid signal can only be set after receiving a
  //   dispatch.next_senior with the sb_id matching the instruction. Conversely, a completed.valid signal 
  //   will not be set for any instruction that receives a dispatch.kill.
  val compltPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  val canComplete = senior_or_kill(compltPtr.value) === NEXT_SENIOR && !busy(compltPtr.value) && valid(compltPtr.value)
  io.ovi_completed.valid := canComplete
  io.ovi_completed.sb_id := sb_id(compltPtr.value)
  io.ovi_completed.illegal := illegal(compltPtr.value)
  // !! Fake !!
  io.ovi_completed.vstart := 0.U  // !! Fake
  io.ovi_completed.fflags := oviCompltSigs(compltPtr.value).fflags
  io.ovi_completed.vxsat := oviCompltSigs(compltPtr.value).vxsat
  io.ovi_completed.dest_reg := oviCompltSigs(compltPtr.value).rd
  when (canComplete || valid(compltPtr.value) && senior_or_kill(compltPtr.value) === KILL) {
    compltPtr := compltPtr + 1.U
  }

  /**
    * Commit
    */
  // Commit pointer
  val deqPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  val expdCnt = RegInit(0.U(3.W))
  // Can commit for expanded instructions (may need multiple cycles)
  val canCommit = valid(deqPtr.value) && (
                     senior_or_kill(deqPtr.value) === NEXT_SENIOR && !busy(deqPtr.value))
                  // || senior_or_kill(deqPtr.value) === KILL //??
                  // || illegal(deqPtr.value)) //??
  // Commit finish, deq pointer can + 1
  // Todo: this expression doesn't work out for VCommitWidth > 2
  val commitEnd = canCommit && (expdCnt === expdLen(deqPtr.value) - 1.U || expdCnt === expdLen(deqPtr.value) - 2.U 
                                || oneCycleCommit(deqPtr.value) 
                                || senior_or_kill(deqPtr.value) === KILL
                                || illegal(deqPtr.value))
  when (commitEnd) { deqPtr := deqPtr + 1.U }
  when (commitEnd) {
    expdCnt := 0.U
  }.elsewhen(canCommit) {
    expdCnt := expdCnt + VCommitWidth.U
  }
  when (commitEnd) {
    valid(deqPtr.value) := false.B
    senior_or_kill(deqPtr.value) := 0.U
  }

  // Payload of the Commit
  for (i <- 0 until VCommitWidth) {
    // valid            // Todo: ld/st segment needs modification
    when (senior_or_kill(deqPtr.value) === KILL || illegal(deqPtr.value)) {
      io.commits.valid(i) := false.B

      deqPtrRhb := deqPtrRhb
    }.elsewhen (oneCycleCommit(deqPtr.value)) {
      if (i == 0) {io.commits.valid(i) := canCommit}
      else {io.commits.valid(i) := false.B}

      deqPtrRhb := Mux(canCommit, deqPtrRhb + 1.U, deqPtrRhb)
    }.otherwise {
      io.commits.valid(i) := canCommit

      deqPtrRhb := Mux(canCommit, deqPtrRhb + VCommitWidth.U, deqPtrRhb)
    }

    // pdest valid
    when (oneCycleCommit(deqPtr.value)) {
      if (i == 0) { io.commits.info(i).pdestVal := rhb(deqPtrRhb.value).pdestVal }
      else { io.commits.info(i).pdestVal := false.B }
    }.otherwise {
      io.commits.info(i).pdestVal := rhb((deqPtrRhb + i.U).value).pdestVal
    }

    // others
    when (oneCycleCommit(deqPtr.value)) {
      if (i == 0) {
        io.commits.info(i).ldest := rhb(deqPtrRhb.value).ldest
        io.commits.info(i).pdest := rhb(deqPtrRhb.value).pdest
        io.commits.info(i).old_pdest := rhb(deqPtrRhb.value).old_pdest
      } else {
        io.commits.info(i).ldest := 0.U
        io.commits.info(i).pdest := 0.U
        io.commits.info(i).old_pdest := 0.U
      }
    }.otherwise {
      io.commits.info(i).ldest := rhb((deqPtrRhb + i.U).value).ldest
      io.commits.info(i).pdest := rhb((deqPtrRhb + i.U).value).pdest
      io.commits.info(i).old_pdest := rhb((deqPtrRhb + i.U).value).old_pdest
    }
  }

  // Flush
  val flush = RegInit(false.B)
  val flushPtr = Reg(new VRobPtr)
  when (commitEnd) {
    flush := (senior_or_kill(deqPtr.value) === KILL)
    flushPtr := deqPtr
  }.otherwise {
    flush := false.B
    flushPtr := flushPtr
  }
  io.flush.valid := flush
  io.flush.bits := flushPtr

  // Credit
  val credit = RegInit(false.B)
  credit := commitEnd
  io.ovi_issueCredit := credit

  /**
    *  Debug
    */
  io.rvfi_sb_id := sb_id(deqPtr.value)
  io.commitEnd := commitEnd

}

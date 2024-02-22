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

/**
  * Constraint: 1) so far only one input and one output
  *             2) VQ has the same length with Vec ROB
  */
package darecreek

import chisel3._
import chisel3.util._
import utils._

// VQ has the same length with Vec ROB
class VRobPtr extends CircularQueuePtr[VRobPtr](VQSize) 

class VecQueue extends Module with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VDecodeOut))
    // from VIllegalInstrn.io.ill
    val illegal = Flipped(ValidIO(new VRobPtr))
    // from VIllegalInstrn.io.partialVInfo
    val partialVInfo_wire = Flipped(ValidIO(new PartialVInfo))
    val partialVInfo_reg = Flipped(ValidIO(new PartialVInfo)) // _reg is one cycle later than _wire
    // flush from ROB
    val flush = Flipped(ValidIO(new VRobPtr))
    // enqPtr
    val enqPtrOut = Output(new VRobPtr)
    val out = Decoupled(new VMicroOp)
    // rs1 read requests from issue queues
    val get_rs1 = Flipped(new GetScalarOperand)
  })
  
  val vq = Reg(Vec(VQSize, new VCtrlInfo))
  val sop = Reg(Vec(VQSize, UInt(xLen.W)))
  val sb_id = Reg(Vec(VQSize, UInt(5.W)))
  val valid = RegInit(VecInit(Seq.fill(VQSize)(false.B)))
  val ill = RegInit(VecInit(Seq.fill(VQSize)(false.B)))

  val enqPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  val deqPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  assert(!(isFull(enqPtr, deqPtr) && io.in.valid), "Error: vector Queue is overflow !!!!")

  // Pass through if the queue is empty
  val bypass = isEmpty(enqPtr, deqPtr) && io.in.valid && io.out.ready
  
  /**
    * Enq
    */
  when (io.in.valid && !bypass) {
    vq(enqPtr.value).ctrl := io.in.bits.ctrl
    io.in.bits.csr.elements.foreach {
      case (name, data) => vq(enqPtr.value).info.elements(name) := data
    }
    sb_id(enqPtr.value) := io.in.bits.sb_id
    valid(enqPtr.value) := true.B
    ill(enqPtr.value) := false.B
  }
  when (io.in.valid) {
    enqPtr := enqPtr + 1.U // + 1.U even on bypass, to generate correct ROB idx
    sop(enqPtr.value) := io.in.bits.scalar_opnd
  }
  io.enqPtrOut := enqPtr

  /** Illegal instrn */
  when (io.illegal.valid) {
    ill(io.illegal.bits.value) := true.B
  }

  // Partial VInfo
  val partialVInfo = io.partialVInfo_reg
  when (partialVInfo.valid) {
    vq(partialVInfo.bits.vRobPtr.value).info.destEew := partialVInfo.bits.destEew
    vq(partialVInfo.bits.vRobPtr.value).info.emulVd := partialVInfo.bits.emulVd
    vq(partialVInfo.bits.vRobPtr.value).info.emulVs2 := partialVInfo.bits.emulVs2
  }

  /**
    * Deq
    */
  // val deq_is_illegal = vq(deqPtr.value).ill && valid(deqPtr.value)
  val deq_is_illegal = (ill(deqPtr.value) || io.illegal.valid && io.illegal.bits.value === deqPtr.value) && 
                       valid(deqPtr.value)
  when (bypass) {
    io.out.valid := true.B
    io.out.bits.ctrl := io.in.bits.ctrl
    io.in.bits.csr.elements.foreach {
      case (name, data) => io.out.bits.info.elements(name) := data
    }
    io.out.bits.info.destEew := io.partialVInfo_wire.bits.destEew
    io.out.bits.info.emulVd := io.partialVInfo_wire.bits.emulVd
    io.out.bits.info.emulVs2 := io.partialVInfo_wire.bits.emulVs2
    io.out.bits.sb_id := io.in.bits.sb_id
    io.out.bits.vRobIdx := deqPtr
  }.otherwise {
    io.out.valid := !(isEmpty(enqPtr, deqPtr)) && !io.flush.valid && !(deq_is_illegal)
    io.out.bits.ctrl := vq(deqPtr.value).ctrl
    io.out.bits.info := vq(deqPtr.value).info
    io.out.bits.sb_id := sb_id(deqPtr.value)
    io.out.bits.vRobIdx := deqPtr
  }

  when (io.out.fire && !bypass) {
    when (partialVInfo.valid && partialVInfo.bits.vRobPtr === deqPtr) {
      io.out.bits.info.destEew := partialVInfo.bits.destEew
      io.out.bits.info.emulVd := partialVInfo.bits.emulVd
      io.out.bits.info.emulVs2 := partialVInfo.bits.emulVs2
    }
  }

  when (io.flush.valid) {
    deqPtr := io.flush.bits + 1.U,
  }.elsewhen (io.out.fire || deq_is_illegal) {
    deqPtr := deqPtr + 1.U
  }
  
  when (io.flush.valid || (io.out.fire || deq_is_illegal)) {
    valid(deqPtr.value) := false.B
  }

  // rs1 read requests from issue queues
  io.get_rs1.exu.data := sop(io.get_rs1.exu.addr.value)
  io.get_rs1.ls.data := sop(io.get_rs1.ls.addr.value)
}
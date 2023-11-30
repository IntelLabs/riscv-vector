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
    val partialVInfo = Flipped(ValidIO(new PartialVInfo))
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
  
  /**
    * Enq
    */
  when (io.in.valid) {
    vq(enqPtr.value).ctrl := io.in.bits.ctrl
    io.in.bits.csr.elements.foreach {
      case (name, data) => vq(enqPtr.value).info.elements(name) := data
    }
    sop(enqPtr.value) := io.in.bits.scalar_opnd
    sb_id(enqPtr.value) := io.in.bits.sb_id
    valid(enqPtr.value) := true.B
    ill(enqPtr.value) := false.B
  }
  enqPtr := enqPtr + Mux(io.in.valid, 1.U, 0.U)
  io.enqPtrOut := enqPtr

  /** Illegal instrn */
  when (io.illegal.valid) {
    ill(io.illegal.bits.value) := true.B
  }

  // Partial VInfo
  when (io.partialVInfo.valid) {
    vq(io.partialVInfo.bits.vRobPtr.value).info.emulVd := io.partialVInfo.bits.emulVd
  }

  /**
    * Deq
    */
  // val deq_is_illegal = vq(deqPtr.value).ill && valid(deqPtr.value)
  val deq_is_illegal = (ill(deqPtr.value) || io.illegal.valid && io.illegal.bits.value === deqPtr.value) && 
                       valid(deqPtr.value)
  io.out.valid := !(isEmpty(enqPtr, deqPtr)) && !io.flush.valid && !(deq_is_illegal)

  io.out.bits.ctrl := vq(deqPtr.value).ctrl
  io.out.bits.info := vq(deqPtr.value).info
  io.out.bits.sb_id := sb_id(deqPtr.value)
  io.out.bits.vRobIdx := deqPtr

  when (io.out.fire) {
    when (io.partialVInfo.valid && io.partialVInfo.bits.vRobPtr === deqPtr) {
      io.out.bits.info.emulVd := io.partialVInfo.bits.emulVd
    }
  }

  // deqPtr := Mux(io.flush.valid, 0.U, deqPtr + Mux(io.out.fire, 1.U, 0.U))
  deqPtr := Mux(io.flush.valid, io.flush.bits + 1.U, 
                deqPtr + Mux(io.out.fire || deq_is_illegal, 1.U, 0.U))
  
  when (io.flush.valid || (io.out.fire || deq_is_illegal)) {
    valid(deqPtr.value) := false.B
  }

  // rs1 read requests from issue queues
  io.get_rs1.exu.data := sop(io.get_rs1.exu.addr.value)
  io.get_rs1.ld.data := sop(io.get_rs1.ld.addr.value)
  io.get_rs1.st.data := sop(io.get_rs1.st.addr.value)

}
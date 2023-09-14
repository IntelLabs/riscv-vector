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
    val in = new Bundle {
      val vCtrl = Input(new VCtrl)
      val scalar_opnd = Input(UInt(64.W))
      val sb_id = Input(UInt(5.W))
      val vInfo = Input(new VInfo)
      val valid = Input(Bool())
    }
    // illegal indicator
    val illegal = Flipped(ValidIO(new VRobPtr))
    // flush from ROB
    val flush = Flipped(ValidIO(new VRobPtr))
    // to illegal instrn module
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
    vq(enqPtr.value).ctrl := io.in.vCtrl
    vq(enqPtr.value).info := io.in.vInfo
    sop(enqPtr.value) := io.in.scalar_opnd
    sb_id(enqPtr.value) := io.in.sb_id
    valid(enqPtr.value) := true.B
    ill(enqPtr.value) := false.B
  }
  enqPtr := enqPtr + Mux(io.in.valid, 1.U, 0.U)
  io.enqPtrOut := enqPtr

  /** Illegal instrn */
  when (io.illegal.valid) {
    ill(io.illegal.bits.value) := true.B
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
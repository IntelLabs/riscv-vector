/**
  * Constraint: so far all inputs of io.in share one 'ready'.
  *             Only support 1 output
  */

package darecreek

import chisel3._
import chisel3.util._
import utils._

class VIssueQueue(size: Int, nEnq: Int) extends Module with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val in = Vec(nEnq, Flipped(Decoupled(new VExpdUOp)))
    // from busy table
    val fromBusyTable = Vec(nEnq, Vec(4, Input(Bool()))) // true: busy
    // wakeups from RF write ports
    val vRFWriteback = Vec(nVRFWritePorts, Flipped(ValidIO(UInt(VPRegIdxWidth.W))))
    val out = Decoupled(new VExpdUOp)
    val flush = Input(Bool())
  })

  val iq = Reg(Vec(size, new VExpdUOp)) 
  val IDLE = false.B
  val VALID = true.B
  val state = RegInit(VecInit(Seq.fill(size)(IDLE)))
  val srcReady = RegInit(VecInit(Seq.fill(size)(VecInit(Seq.fill(4)(false.B)))))

  class IQPtr extends CircularQueuePtr[IQPtr](size)
  val deqPtr = RegInit(0.U.asTypeOf(new IQPtr))
  val enqPtr = RegInit(VecInit((0 until nEnq).map(_.U.asTypeOf(new IQPtr))))
  val validCount = distanceBetween(enqPtr(0), deqPtr)

  def wakeupFromRFWb(srcIdx: Int, uop: VExpdUOp): Bool = {
    Cat((0 until nVRFWritePorts) map { i => 
         io.vRFWriteback(i).valid && io.vRFWriteback(i).bits === uop.psrc(srcIdx)
      }).orR
  } 

  /** 
    * Src operands ready
    */
  val allSrcReady = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    for (j <- 0 until 4) {
      srcReady(i)(j) := Mux(state(i) === IDLE, false.B, wakeupFromRFWb(j, iq(i)) || srcReady(i)(j))
    }
    allSrcReady(i) := (srcReady(i).zip(iq(i).psrcVal) map {case (rdy, v) => rdy || !v}).reduce(_ && _) && state(i) === VALID
  }

  /** 
    * Enq
    */
  val canEnq = RegInit(false.B)
  val enqFires = io.in.map(_.valid && io.in(0).ready)
  val numEnqFire = PopCount(enqFires)
  canEnq := Mux(validCount > (size - nEnq).U, false.B, numEnqFire <= (size - nEnq).U - validCount)
  io.in.map(_.ready := canEnq && !io.flush)
  for (i <- 0 until nEnq) {
    val enqOffset = if (i == 0) 0.U else PopCount(enqFires.take(i))
    when (enqFires(i)) {
      iq(enqPtr(enqOffset).value) := io.in(i).bits
      state(enqPtr(enqOffset).value) := VALID
      for (j <- 0 until 4) {
        srcReady(enqPtr(enqOffset).value)(j) := !io.fromBusyTable(i)(j) || wakeupFromRFWb(j, io.in(i).bits)
      }
    }
    enqPtr(i) := Mux(io.flush, i.U.asTypeOf(new IQPtr), enqPtr(i) + numEnqFire)
  }

  /** 
    * Deq
    */
  val empty = isEmpty(enqPtr(0), deqPtr)
  val validNumDeq = Mux(allSrcReady(deqPtr.value) && !empty, 1.U, 0.U)
  val numDeqFire = Mux(io.out.ready, validNumDeq, 0.U)
  io.out.bits := iq(deqPtr.value)
  io.out.valid := numDeqFire === 1.U && !io.flush
  when (numDeqFire === 1.U) { state(deqPtr.value) := IDLE }
  deqPtr := Mux(io.flush, 0.U.asTypeOf(new IQPtr), deqPtr + numDeqFire)

  when(io.flush) { state.map(_ := IDLE) }
}
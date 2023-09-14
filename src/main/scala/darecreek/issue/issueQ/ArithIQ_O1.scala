/**
  * Out-of-order arith issue queue. Only 1 issue.
  * 
  * Constraint: so far all inputs of io.in share one 'ready'. Only support 1 output
  */

package darecreek

import chisel3._
import chisel3.util._
import utils._

class VArithIQ_O1(size: Int, nEnq: Int) extends Module with HasCircularQueuePtrHelper {
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
  val IDLE = 0.U(2.W)
  val VALID = 1.U(2.W)
  val ISSUED = 2.U(2.W)
  val state = RegInit(VecInit(Seq.fill(size)(IDLE)))
  val srcReady = RegInit(VecInit(Seq.fill(size)(VecInit(Seq.fill(4)(false.B)))))
  val fuReady = RegInit(VecInit(Seq.fill(size)(false.B)))

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
    * FU ready
    */
  // for (i <- 0 until size) {
  //   val fuIsReady = Mux1H(Seq(
  //     iq(i).ctrl.alu -> io.out.readys(0),
  //     iq(i).ctrl.mul -> io.out.readys(1),
  //     iq(i).ctrl.fp  -> io.out.readys(2),
  //     iq(i).ctrl.div -> io.out.readys(3),
  //   ))
  //   fuReady(i) := state(i) === VALID && fuIsReady
  // }
  // val allReady = WireInit(VecInit(allSrcReady zip fuReady map {case (x,y) => x && y}))


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
  //---- Sequence matrix ----
  // x is column, y is row. (x, y) = 1 means entry-x is older than entry-y
  //    x 0 1 2 3
  //  y-+--------
  //  0 | 0 0 0 0
  //  1 | 1 0 0 0
  //  2 | 1 1 0 0
  //  3 | 1 1 1 0
  //  Deq will change the matrix
  // val seqMat = RegInit(VecInit(Seq.tabulate(size)(i => 
                       // ("b" + "1"*i + "0"*(size - i)).U(size.W)).map(x => x.asBools)))
  val seqMat = Reg(Vec(size, Vec(size, Bool())))
  for (i <- 0 until size) {
    when (deqPtr.value === i.U && state(deqPtr.value) === ISSUED) {
      for (j <- 0 until size if j != i) {
        seqMat(i)(j) := !seqMat(i)(j)
        seqMat(j)(i) := !seqMat(j)(i)
      }
    }
    when (reset.asBool) {
      for (j <- 0 until size) {seqMat(i)(j) := {if (i > j) true.B else false.B}}
    }
  }
  val readySelect = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    readySelect(i) := state(i) === VALID && allSrcReady(i) && 
         seqMat(i).zip(allSrcReady).map({case (pre, rdy) => pre && rdy}).reduce(_ || _) === false.B
  }
  assert(PopCount(readySelect) <= 1.U, "Error: amount of bit 1 of readySelect in issue queue must <= 1")
  
  io.out.bits := Mux1H(readySelect, iq)
  io.out.valid := readySelect.reduce(_ || _) && !io.flush
  for (i <- 0 until size) {
    when (deqPtr.value === i.U && state(i) === ISSUED) {state(deqPtr.value) := IDLE}
    when (io.out.fire && readySelect(i)) {state(i) := Mux(deqPtr.value === i.U, IDLE, ISSUED)}
    when (io.flush) {state(i) := IDLE}
  }
  deqPtr := Mux(io.flush, 0.U.asTypeOf(new IQPtr), 
            Mux(state(deqPtr.value) === ISSUED || readySelect(deqPtr.value), deqPtr + 1.U, deqPtr))
}

object VerilogIQ1 extends App {
  emitVerilog(new VArithIQ_O1(32, 2), Array("--target-dir", "generated"))
}
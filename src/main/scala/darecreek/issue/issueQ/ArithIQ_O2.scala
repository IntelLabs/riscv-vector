/**
  * Out-of-order arith issue queue. 2-way issues.
  * 
  * Constraint: so far all inputs of io.in share one 'ready'. Only support 1 output
  */

package darecreek

import chisel3._
import chisel3.util._
import utils._

class VArithIQ_O2(size: Int, nEnq: Int) extends Module with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val in = Vec(nEnq, Flipped(Decoupled(new VExpdUOp)))
    // from busy table
    val fromBusyTable = Vec(nEnq, Vec(4, Input(Bool()))) // true: busy
    // wakeups from RF write ports
    val vRFWriteback = Vec(nVRFWritePorts, Flipped(ValidIO(UInt(VPRegIdxWidth.W))))
    val out = Vec(2, ValidIO(new Bundle {
          val uop = new VExpdUOp
          val fuType = UInt(3.W)
        }))
    val outRdy = Input(Vec(5, Bool()))  // amount of FUs
    // val out = Decoupled(new VExpdUOp)
    val flush = Input(Bool())
  })

  val iq = Reg(Vec(size, new VExpdUOp)) 
  val IDLE = 0.U(2.W)
  val VALID = 1.U(2.W)
  val ISSUED = 2.U(2.W)
  val state = RegInit(VecInit(Seq.fill(size)(IDLE)))
  val srcReady = RegInit(VecInit(Seq.fill(size)(VecInit(Seq.fill(4)(false.B)))))
  val fuReady = RegInit(VecInit(Seq.fill(size)(false.B)))
  val fuType = Reg(Vec(size, UInt(3.W)))

  class IQPtr extends CircularQueuePtr[IQPtr](size)
  val deqPtr = RegInit(0.U.asTypeOf(new IQPtr))
  val enqPtr = RegInit(VecInit((0 until nEnq).map(_.U.asTypeOf(new IQPtr))))
  val validCount = distanceBetween(enqPtr(0), deqPtr)

  def wakeupFromRFWb(srcIdx: Int, uop: VExpdUOp): Bool = {
    Cat((0 until nVRFWritePorts) map { i => 
         io.vRFWriteback(i).valid && io.vRFWriteback(i).bits === uop.psrc(srcIdx)
      }).orR
  }
  def fuTypeGen(ctrl: VCtrl): UInt = {
    Mux1H(Seq(
      ctrl.alu  -> 0.U(3.W),
      ctrl.mul  -> 1.U(3.W),
      ctrl.fp   -> 2.U(3.W),
      ctrl.div  -> 3.U(3.W),
      (ctrl.mask || ctrl.redu || ctrl.perm)  -> 4.U(3.W),
    ))
  }

  //---- Src operands ready ----
  val allSrcReady = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    for (j <- 0 until 4) {
      srcReady(i)(j) := Mux(state(i) === IDLE, false.B, wakeupFromRFWb(j, iq(i)) || srcReady(i)(j))
    }
    allSrcReady(i) := (srcReady(i).zip(iq(i).psrcVal) map {case (rdy, v) => rdy || !v}).reduce(_ && _) && state(i) === VALID
  }

  //---- FU ready ----
  for (i <- 0 until size) {
    val fuIsReady = Mux1H(Seq(
      iq(i).ctrl.alu -> io.outRdy(0),
      iq(i).ctrl.mul -> io.outRdy(1),
      iq(i).ctrl.fp  -> io.outRdy(2),
      iq(i).ctrl.div -> io.outRdy(3),
      (iq(i).ctrl.mask || iq(i).ctrl.redu || iq(i).ctrl.perm) -> io.outRdy(4),
    ))
    fuReady(i) := state(i) === VALID && fuIsReady
  }
  val srcFuReady = allSrcReady zip fuReady map {case (x,y) => x && y}

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
      fuType(enqPtr(enqOffset).value) := fuTypeGen(io.in(i).bits.ctrl)
    }
    enqPtr(i) := Mux(io.flush, i.U.asTypeOf(new IQPtr), enqPtr(i) + numEnqFire)
  }

  /** 
    * Select two issue entries
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

  // Find all precedent and src/fu ready entries (a matrix)
  val preSrcFuReady = Wire(Vec(size, Vec(size, Bool())))
  for (i <- 0 until size) {
    preSrcFuReady(i) := seqMat(i) zip srcFuReady map {case (pre, rdy) => pre && rdy}
  }

  // Find how many different types of fu are in precedent and src/fu ready entries
  //   First return value: 00: none,  01: only 1 type of fu,  10: more than 1 type of fu
  //   Second return value: the fu type if the first return is 01
  def fu_types_in_preSrcFuReady(ready: Seq[Bool], fu: Seq[UInt], size: Int): (UInt, UInt) = {
    val fuType_amount = Wire(UInt(2.W))
    val fuType_value = Wire(UInt(3.W))
    if (size == 1) {
      fuType_amount := Mux(ready(0), 1.U, 0.U)
      fuType_value := fu(0)
    } else {
      val highHalf = fu_types_in_preSrcFuReady(ready.drop(size/2), fu.drop(size/2), size/2)
      val lowHalf = fu_types_in_preSrcFuReady(ready.take(size/2), fu.take(size/2), size/2)
      when (highHalf._1 === 1.U && lowHalf._1 === 1.U) {
        fuType_amount := Mux(highHalf._2 === lowHalf._2, 1.U, 2.U)
      }.elsewhen(!(highHalf._1)(1) && !(lowHalf._1)(1) && (highHalf._1)(0) =/= (lowHalf._1)(0)) {
        fuType_amount := 1.U
      }.elsewhen (highHalf._1 === 0.U && lowHalf._1 === 0.U) {
        fuType_amount := 0.U
      }.otherwise {
        fuType_amount := 2.U
      }
      fuType_value := Mux((lowHalf._2)(0), lowHalf._2, highHalf._2)
    }
    (fuType_amount, fuType_value)
  }

  // Generate first and second issue entries
  val firstIssue = Wire(Vec(size, Bool()))
  val secondIssue = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    firstIssue(i) := state(i) === VALID && srcFuReady(i) &&
                     fu_types_in_preSrcFuReady(preSrcFuReady(i), fuType, size)._1 === 0.U
    secondIssue(i) := state(i) === VALID && srcFuReady(i) &&
                     fu_types_in_preSrcFuReady(preSrcFuReady(i), fuType, size)._1 === 1.U &&
                     fu_types_in_preSrcFuReady(preSrcFuReady(i), fuType, size)._2 =/= fuType(i)
  }
  assert(PopCount(firstIssue) <= 1.U, "Error: number of firstIssue hits must <= 1")
  assert(PopCount(secondIssue) <= 1.U, "Error: number of secondIssue hits must <= 1")
  assert(firstIssue.asUInt =/= secondIssue.asUInt, "Error: two issue entries must be different")

  io.out(0).valid := firstIssue.reduce(_ || _) && !io.flush
  io.out(0).bits.uop := Mux1H(firstIssue, iq)
  io.out(0).bits.fuType := Mux1H(firstIssue, fuType)
  io.out(1).valid := secondIssue.reduce(_ || _) && !io.flush
  io.out(1).bits.uop := Mux1H(secondIssue, iq)
  io.out(1).bits.fuType := Mux1H(secondIssue, fuType)

  /** 
    * Deq (& state change to ISSUED after issue)
    */
  //---- Case of deqPtr increased by 2 ----
  val case_of_deqPtr_inc_2 = firstIssue(deqPtr.value) && secondIssue((deqPtr + 1.U).value) ||
        firstIssue(deqPtr.value) && !secondIssue((deqPtr + 1.U).value) && state((deqPtr + 1.U).value) === ISSUED ||
        !firstIssue(deqPtr.value) && firstIssue((deqPtr + 1.U).value) && state(deqPtr.value) === ISSUED ||
        !firstIssue(deqPtr.value) && !firstIssue((deqPtr + 1.U).value) && state(deqPtr.value) === ISSUED && state((deqPtr + 1.U).value) === ISSUED
  when (io.flush) {
    deqPtr := 0.U.asTypeOf(new IQPtr)
  }.elsewhen (state(deqPtr.value) === ISSUED || state((deqPtr + 1.U).value) === ISSUED || state((deqPtr + 2.U).value) === ISSUED) {
    deqPtr := deqPtr + 3.U
  }.elsewhen (case_of_deqPtr_inc_2) {
    deqPtr := deqPtr + 2.U
  }.elsewhen (state(deqPtr.value) === ISSUED || firstIssue(deqPtr.value)) {
    deqPtr := deqPtr + 1.U
  }.otherwise {
    deqPtr := deqPtr
  }
  
  when (state(deqPtr.value) === ISSUED) {state(deqPtr.value) := IDLE}
  for (i <- 0 until size) {
    when (deqPtr.value === i.U && state(i) === ISSUED ||
          (deqPtr + 1.U).value === i.U && state(i) === ISSUED && state(deqPtr.value) === ISSUED) {
      state(deqPtr.value) := IDLE
    }
    when (secondIssue(i)) {state(i) := Mux((deqPtr + 1.U).value === i.U, IDLE, ISSUED)}
    when (firstIssue(i)) {state(i) := Mux(deqPtr.value === i.U || (deqPtr + 1.U).value === i.U && state(deqPtr.value) === ISSUED, IDLE, ISSUED)}
    when (io.flush) {state(i) := IDLE}
  }
}

object VerilogIQ2 extends App {
  emitVerilog(new VArithIQ_O2(32, 2), Array("--target-dir", "generated"))
}
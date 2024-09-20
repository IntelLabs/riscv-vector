package gpc.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import VectorParam._
import utility._

class VerCSR(implicit p: Parameters) extends CoreBundle()(p) {
  val  mstatusWr= UInt((NRET*xLen).W)
  val  mepcWr= UInt((NRET*xLen).W)
  val  mtvalWr= UInt((NRET*xLen).W)
  val  mtvecWr= UInt((NRET*xLen).W)
  val  mcauseWr= UInt((NRET*xLen).W)
  val  mipWr= UInt((NRET*xLen).W)
  val  mieWr= UInt((NRET*xLen).W)
  val  mscratchWr= UInt((NRET*xLen).W)
  val  midelegWr= UInt((NRET*xLen).W)
  val  medelegWr= UInt((NRET*xLen).W)
  val  minstretWr= UInt((NRET*xLen).W)
  val  sstatusWr= UInt((NRET*xLen).W)
  val  sepcWr= UInt((NRET*xLen).W)
  val  stvalWr= UInt((NRET*xLen).W)
  val  stvecWr= UInt((NRET*xLen).W)
  val  scauseWr= UInt((NRET*xLen).W)
  val  satpWr= UInt((NRET*xLen).W)
  val  sscratchWr= UInt((NRET*xLen).W)
  val  vtypeWr= UInt((NRET*xLen).W)
  val  vcsrWr= UInt((NRET*xLen).W)
  val  vlWr= UInt((NRET*xLen).W)
  val  vstartWr= UInt((NRET*xLen).W)
}

class VerOutIO(implicit p: Parameters) extends CoreBundle()(p) {
  val commit_valid = Output(UInt(NRET.W))
  val commit_currPc = Output(UInt((NRET*xLen).W))
  val commit_insn = Output(UInt((NRET*32).W))

  val sim_halt = Output(Bool())

  val trap_valid = Output(Bool())
  val trap_code = Output(UInt((xLen).W))

  val reg_gpr= Output(UInt((NRET*31*xLen).W))
  val reg_fpr= Output(UInt((NRET*32*fLen).W))
  val reg_vpr= Output(UInt((NRET*32*vLen).W))

  val csr = Output(new VerCSR)
}

class ROBEnq(implicit p: Parameters) extends CoreBundle()(p) {
  val pc = UInt(xLen.W)
  val insn = UInt(32.W)
  val int = Bool() // Integer RF
  val fp = Bool() // Floating-point RF
  val vec = Bool() // Vector RF
  val vsb_id = UInt(bVScoreboardId.W)
  val waddr = UInt(5.W)
  val wdata = UInt(xLen.W) // int/fp, wdata of enq is not available for long-latency instrn
  //TODO - add csr
}

class ROBWb(implicit p: Parameters) extends CoreBundle()(p) {
  val int = Bool() // Integer RF
  val fp = Bool() // Floating-point RF
  val waddr = UInt(5.W)
  val wdata = UInt(xLen.W) // int/fp, wb is only for long-latency instrn
}

class VerInIO(implicit p: Parameters) extends CoreBundle()(p) {
  val swap = Input(Bool())
  val rob_enq = Input(Vec(NRET, ValidIO(new ROBEnq)))
  val rob_wb = Input(Vec(NRET, ValidIO(new ROBWb))) // int/fp, wb is only for long-latency instrn
  //TODO - add other signals such as csr and vector wb
}

class ROBEntry(implicit p: Parameters) extends CoreBundle()(p) {
  val valid = Bool()
  val pc = UInt(xLen.W)
  val insn = UInt(32.W)
  val int = Bool() // Integer
  val fp = Bool() // Floating-point
  val vec = Bool() // Vector
  val vsb_id = UInt(bVScoreboardId.W)
  val waddr = UInt(5.W)
  val wdata = UInt(xLen.W) // int/fp, wdata of enq is not available for long-latency instrn
  val ready_to_commit = Bool()
  //TODO - add csr and vector wdata
}

class UvmVerification(implicit p:Parameters) extends CoreModule{
  val io = IO(new Bundle {
    val uvm_in = new VerInIO
    val uvm_out = new VerOutIO
  })

  val ROBSize = 64
  val debugROB = Reg(Vec(ROBSize, new ROBEntry))
  class robPtr extends CircularQueuePtr[robPtr](ROBSize)
  val enqPtrROB = RegInit(0.U.asTypeOf(new robPtr))
  val deqPtrROB = RegInit(0.U.asTypeOf(new robPtr))
  when (reset.asBool) {
    debugROB.foreach(_.valid := false.B)
  }

  // Enq of ROB
  val swapEnq = io.uvm_in.swap
  val rob_enq_swapped = Wire(Vec(2, new ROBEnq))
  val rob_enq_swapped_valid = Wire(Vec(2, Bool()))
  rob_enq_swapped(0) := Mux(swapEnq, io.uvm_in.rob_enq(1).bits, io.uvm_in.rob_enq(0).bits)
  rob_enq_swapped(1) := Mux(swapEnq, io.uvm_in.rob_enq(0).bits, io.uvm_in.rob_enq(1).bits)
  rob_enq_swapped_valid(0) := Mux(swapEnq, io.uvm_in.rob_enq(1).valid, io.uvm_in.rob_enq(0).valid)
  rob_enq_swapped_valid(1) := Mux(swapEnq, io.uvm_in.rob_enq(0).valid, io.uvm_in.rob_enq(1).valid)
  when (rob_enq_swapped_valid(0)) {
    debugROB(enqPtrROB.value).valid := true.B
    debugROB(enqPtrROB.value).pc := rob_enq_swapped(0).pc
    debugROB(enqPtrROB.value).insn := rob_enq_swapped(0).insn
    debugROB(enqPtrROB.value).int := rob_enq_swapped(0).int
    debugROB(enqPtrROB.value).fp := rob_enq_swapped(0).fp
    debugROB(enqPtrROB.value).vec := rob_enq_swapped(0).vec
    debugROB(enqPtrROB.value).vsb_id := rob_enq_swapped(0).vsb_id
    debugROB(enqPtrROB.value).waddr := rob_enq_swapped(0).waddr
    debugROB(enqPtrROB.value).wdata := rob_enq_swapped(0).wdata
    debugROB(enqPtrROB.value).ready_to_commit := true.B //FIXME - 
  }
  when (rob_enq_swapped_valid(0) && rob_enq_swapped_valid(1)) {
    debugROB((enqPtrROB + 1.U).value).valid := true.B
    debugROB((enqPtrROB + 1.U).value).pc := rob_enq_swapped(1).pc
    debugROB((enqPtrROB + 1.U).value).insn := rob_enq_swapped(1).insn
    debugROB((enqPtrROB + 1.U).value).int := rob_enq_swapped(1).int
    debugROB((enqPtrROB + 1.U).value).fp := rob_enq_swapped(1).fp
    debugROB((enqPtrROB + 1.U).value).vec := rob_enq_swapped(1).vec
    debugROB((enqPtrROB + 1.U).value).vsb_id := rob_enq_swapped(1).vsb_id
    debugROB((enqPtrROB + 1.U).value).waddr := rob_enq_swapped(1).waddr
    debugROB((enqPtrROB + 1.U).value).wdata := rob_enq_swapped(1).wdata
    debugROB((enqPtrROB + 1.U).value).ready_to_commit := true.B //FIXME - 
  }
  when (rob_enq_swapped_valid(0) && rob_enq_swapped_valid(1)) {
    enqPtrROB := enqPtrROB + 2.U
  }.elsewhen (rob_enq_swapped_valid(0)) {
    enqPtrROB := enqPtrROB + 1.U
  }

  // Wb of ROB
  //TODO - 

  // Commit of ROB
  val commit_valids = Wire(Vec(2, Bool()))
  commit_valids(0) := debugROB(deqPtrROB.value).valid && debugROB(deqPtrROB.value).ready_to_commit
  commit_valids(1) := commit_valids(0) &&
                      debugROB((deqPtrROB + 1.U).value).valid && debugROB((deqPtrROB + 1.U).value).ready_to_commit
  when (commit_valids(0) && commit_valids(1)) {
    deqPtrROB := deqPtrROB + 2.U
    debugROB(deqPtrROB.value).valid := false.B
    debugROB((deqPtrROB + 1.U).value).valid := false.B
  }.elsewhen (commit_valids(0)) {
    deqPtrROB := deqPtrROB + 1.U
    debugROB(deqPtrROB.value).valid := false.B
  }
  val commit_bits = Wire(Vec(2, new ROBEntry))
  commit_bits(0) := debugROB(deqPtrROB.value)
  commit_bits(1) := debugROB((deqPtrROB + 1.U).value)

  /**
    * Emulated Integer Register File
    */
  val emul_int_RF = RegInit(VecInit(Seq.fill(32)(0.U(xLen.W))))
  val emul_int_RF_next = Wire(Vec(2, Vec(32, UInt(xLen.W))))
  emul_int_RF_next(0) := emul_int_RF
  when (commit_valids(0)) {
    emul_int_RF_next(0)(commit_bits(0).waddr) := commit_bits(0).wdata
  }
  emul_int_RF_next(1) := emul_int_RF_next(0)
  when (commit_valids(1)) {
    emul_int_RF_next(1)(commit_bits(1).waddr) := commit_bits(1).wdata
  }
  emul_int_RF := emul_int_RF_next(1)

  /**
    * Final verification interface
    */
  io.uvm_out := DontCare
  
  io.uvm_out.commit_valid := commit_valids.asUInt
  io.uvm_out.commit_currPc := Cat(commit_bits.map(_.pc).reverse)
  io.uvm_out.commit_insn := Cat(commit_bits.map(_.insn).reverse)
  io.uvm_out.reg_gpr := Cat(emul_int_RF_next.map(rf => Cat(rf.tail.reverse)).reverse)

}
package gpc.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import VectorParam._

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
  val int = Input(Bool()) // Integer RF
  val fp = Input(Bool()) // Floating-point RF
  val vec = Input(Bool()) // Vector RF
  val vsb_id = UInt(bVScoreboardId.W)
  val waddr = UInt(5.W)
  val wdata = UInt(xLen.W) // int/fp, wdata of enq is not available for long-latency instrn
  //TODO - add csr
}

class ROBWb(implicit p: Parameters) extends CoreBundle()(p) {
  val int = Input(Bool()) // Integer RF
  val fp = Input(Bool()) // Floating-point RF
  val waddr = UInt(5.W)
  val wdata = UInt(xLen.W) // int/fp, wb is only for long-latency instrn
}

class VerInIO(implicit p: Parameters) extends CoreBundle()(p) {
  val rob_enq = Input(Vec(NRET, ValidIO(new ROBEnq)))
  val rob_wb = Input(Vec(NRET, ValidIO(new ROBWb))) // int/fp, wb is only for long-latency instrn
  //TODO - add other signals such as csr and vector wb
}

class ROBEntry(implicit p: Parameters) extends CoreBundle()(p) {
  val int = Input(Bool()) // Integer
  val fp = Input(Bool()) // Floating-point
  val vec = Input(Bool()) // Vector
  val vsb_id = UInt(bVScoreboardId.W)
  val waddr = UInt(5.W)
  val wdata = UInt(xLen.W) // int/fp, wdata of enq is not available for long-latency instrn
  //TODO - add csr and vector wdata
}

class UvmVerification(implicit p:Parameters) extends CoreModule{
  val io = IO(new Bundle {
    val uvm_in = new VerInIO
    val uvm_out = new VerOutIO
  })
  
  io.uvm_out := DontCare
}
package darecreek.exu.vfu.fp

import chisel3._
import chisel3.util._
// import darecreek.{LaneFUInput, LaneFUOutput}
import darecreek.exu.vfu.{LaneFUInput, LaneFUOutput}
import org.chipsalliance.cde.config.Parameters
import darecreek.exu.vfu._

abstract class VFPUBaseModule(implicit p: Parameters) extends Module with HasVFuParameters

// a module that has decoupled interfaces
abstract class VFPUSubModule(implicit p: Parameters) extends VFPUBaseModule with HasVFuParameters {
  val io = IO(new LaneFloatFUIO)
  def invert_sign(x: UInt, len: Int) = {
    Cat(
      !x(len-1), x(len-2, 0)
    )
  }
  def empty_fflags = 0.U(5.W)
  def empty_fflags_n(n: Int) = Seq.fill(n)(0.U(5.W))
  // mask(x) == 1 means active
  def isActive(id: Int) = !io.in.bits.prestart(id) & !io.in.bits.tail(id) & (
    io.in.bits.uop.ctrl.vm | io.in.bits.mask(id)
  )

  io.out.bits.vxsat := false.B
}

// a module that has decoupled interfaces
abstract class VFPUDivSubModule(implicit p: Parameters) extends VFPUBaseModule with HasVFuParameters {
  val io = IO(new LaneFloatDivFUIO)
  def invert_sign(x: UInt, len: Int) = {
    Cat(
      !x(len-1), x(len-2, 0)
    )
  }
  def empty_fflags = 0.U(5.W)
  def empty_fflags_n(n: Int) = Seq.fill(n)(0.U(5.W))
  // mask(x) == 1 means active
  def isActive(id: Int) = !io.in.bits.prestart(id) & !io.in.bits.tail(id) & (
    io.in.bits.uop.ctrl.vm | io.in.bits.mask(id)
  )

  io.out.bits.vxsat := false.B
}

abstract class VFPUPipelineModule(implicit p: Parameters)
  extends VFPUSubModule
    with HasPipelineReg

trait HasPipelineReg {
  this: VFPUSubModule =>

  def latency: Int

  require(latency > 0)

  val validVec = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  // true is not ready?
  val rdyVec = (Array.fill(latency - 1)(Wire(Bool())) :+ io.out.ready) :+ WireInit(true.B)
  val uopVec = io.in.bits.uop +: Array.fill(latency)(Reg(new VFPUOp))

  // if flush(0), valid 0 will not given, so set flushVec(0) to false.B
  // val flushVec = Array.fill(latency+1)(WireInit(false.B)) // FIXME: implement flush logic
  val flushVec = validVec.zip(uopVec).map(x => x._1 && x._2.sysUop.robIdx.needFlush(io.redirect))

  for (i <- 0 until latency - 1) {
    rdyVec(i) := !validVec(i + 1) || rdyVec(i + 1)
  }

  for (i <- 1 to latency) {
    when(regEnable(i)){
      validVec(i) := validVec(i - 1)
      uopVec(i) := uopVec(i - 1)
    }.elsewhen(flushVec(i) || rdyVec(i)){
      validVec(i) := false.B
    }
  }

  io.in.ready := rdyVec(0)
  io.out.valid := validVec.takeRight(2).head
  io.out.bits.uop := uopVec.takeRight(2).head

  def regEnable(i: Int): Bool = validVec(i - 1) && rdyVec(i - 1) && !flushVec(i - 1)

  def PipelineReg[TT <: Data](i: Int)(next: TT) = RegEnable(
    next,
    regEnable(i)
  )

  def S1Reg[TT <: Data](next: TT): TT = PipelineReg[TT](1)(next)

  def S2Reg[TT <: Data](next: TT): TT = PipelineReg[TT](2)(next)

  def S3Reg[TT <: Data](next: TT): TT = PipelineReg[TT](3)(next)

  def S4Reg[TT <: Data](next: TT): TT = PipelineReg[TT](4)(next)

  def S5Reg[TT <: Data](next: TT): TT = PipelineReg[TT](5)(next)
}

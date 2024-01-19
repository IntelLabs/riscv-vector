package darecreek

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.vmask._
import darecreek.exu.vfucore.{VFuModule, VFuParamsKey, VFuParameters}

class VMaskDC extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VExuInput))
    val out = Decoupled(new VCrossExuOut)
  })

  implicit val p = Parameters.empty.alterPartial({
    case VFuParamsKey => VFuParameters(VLEN = 256)
  })
  val vmask = Module(new VMask()(p))

  vmask.io.in.bits.uop := io.in.bits.uop
  vmask.io.in.bits.vs1 := Cat(io.in.bits.vSrc(0).reverse)
  vmask.io.in.bits.vs2 := Cat(io.in.bits.vSrc(1).reverse)
  vmask.io.in.bits.oldVd := Cat(io.in.bits.vSrc(2).reverse)
  vmask.io.in.bits.mask := Cat(io.in.bits.vSrc(3).reverse)
  vmask.io.in.bits.rs1 := io.in.bits.rs1
  vmask.io.in.valid := io.in.valid
  vmask.io.out.ready := io.out.ready
  io.in.ready := vmask.io.in.ready
  io.out.valid := vmask.io.out.valid

  io.out.bits.uop := vmask.io.out.bits.uop
  io.out.bits.vd := VecInit(Seq.tabulate(NLanes)(i => (vmask.io.out.bits.vd) ((i + 1) * LaneWidth - 1, i * LaneWidth)))
  io.out.bits.fflags := 0.U

}

object VerilogMaskDC extends App {
  println("Generating the VPU Mask hardware")
  emitVerilog(new VMaskDC(), Array("--target-dir", "generated"))
}


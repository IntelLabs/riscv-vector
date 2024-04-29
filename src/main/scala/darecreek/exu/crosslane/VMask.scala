package darecreek.exu.crosslane.vmask

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.vmask._
import darecreek.exu.vfucore.{VFuModule, VFuParamsKey, VFuParameters}
import darecreek._

class VMaskDC(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VExuInput))
    val out = Decoupled(new VCrossExuOut)
  })

  val vmask = Module(new VMask()(p))

  val uop = io.in.bits.uop
  val funct6 = uop.ctrl.funct6
  val funct3 = uop.ctrl.funct3
  val uopIdx = io.in.bits.uop.expdIdx
  val uopEnd = io.in.bits.uop.expdEnd
  val vs1_index = uop.ctrl.lsrc(0)
  val vlmul = uop.info.vlmul
  val fire = io.in.fire

  val vcpop_m = (funct6 === "b010000".U) && (funct3 === "b010".U) && (vs1_index === "b10000".U)

  val cpop_busy = RegInit(false.B)
  val vlmul_reg = RegInit(0.U(2.W))
  val cpop_uopIdx = RegInit(0.U(3.W))

  when(vcpop_m && fire && ((vlmul === 1.U) || (vlmul === 2.U) || (vlmul === 3.U))) {
    cpop_busy := true.B
    vlmul_reg := Mux((vlmul > 3.U), 0.U, vlmul(1, 0))
  }.elsewhen(vmask.io.in.valid && vmask.io.in.ready && (cpop_uopIdx === ((1.U << vlmul_reg) - 1.U))) {
    cpop_busy := false.B
  }

  cpop_uopIdx := 0.U
  when((vcpop_m && fire && ((vlmul === 1.U) || (vlmul === 2.U) || (vlmul === 3.U))) ||
    (cpop_busy && vmask.io.in.valid && vmask.io.in.ready)) {
    cpop_uopIdx := cpop_uopIdx + 1.U
  }

  val uop_reg = Reg(new VExpdUOp)
  val vs2_reg = RegInit(0.U(VLEN.W))
  val old_vd_reg = RegInit(0.U(VLEN.W))
  val mask_reg = RegInit(0.U(VLEN.W))

  when(vcpop_m && fire && (uopIdx === 0.U)) {
    uop_reg := io.in.bits.uop
    vs2_reg := Cat(io.in.bits.vSrc(1).reverse)
    old_vd_reg := Cat(io.in.bits.vSrc(2).reverse)
    mask_reg := Cat(io.in.bits.vSrc(3).reverse)
  }

  vmask.io.in.bits.uop := Mux(cpop_busy, uop_reg, io.in.bits.uop)
  vmask.io.in.bits.vs1 := Cat(io.in.bits.vSrc(0).reverse)
  vmask.io.in.bits.vs2 := Mux(cpop_busy, vs2_reg, Cat(io.in.bits.vSrc(1).reverse))
  vmask.io.in.bits.oldVd := Mux(cpop_busy, old_vd_reg, Cat(io.in.bits.vSrc(2).reverse))
  vmask.io.in.bits.mask := Mux(cpop_busy, mask_reg, Cat(io.in.bits.vSrc(3).reverse))
  vmask.io.in.bits.rs1 := io.in.bits.rs1
  vmask.io.in.valid := io.in.valid || cpop_busy
  vmask.io.out.ready := io.out.ready
  io.in.ready := vmask.io.in.ready && !cpop_busy
  io.out.valid := vmask.io.out.valid

  vmask.io.in.bits.uop.expdIdx := Mux(cpop_busy, cpop_uopIdx, io.in.bits.uop.expdIdx)
  vmask.io.in.bits.uop.expdEnd :=
    Mux(vcpop_m && fire, uopEnd && ((vlmul === 0.U) || (vlmul > 3.U)),
      Mux(cpop_busy,
        ((vmask.io.in.bits.uop.expdIdx === 1.U) && (vmask.io.in.bits.uop.info.vlmul === 1.U)) ||
          ((vmask.io.in.bits.uop.expdIdx === 3.U) && (vmask.io.in.bits.uop.info.vlmul === 2.U)) ||
          ((vmask.io.in.bits.uop.expdIdx === 7.U) && (vmask.io.in.bits.uop.info.vlmul === 3.U)), io.in.bits.uop.expdEnd))

  io.out.bits.uop := vmask.io.out.bits.uop
  io.out.bits.vd := VecInit(Seq.tabulate(NLanes)(i => (vmask.io.out.bits.vd) ((i + 1) * LaneWidth - 1, i * LaneWidth)))
  io.out.bits.fflags := 0.U

}

object VerilogVMaskDC extends App {
  println("Generating hardware")
  val p = Parameters.empty
  emitVerilog(new VMaskDC()(p.alterPartial({ case VFuParamsKey =>
    VFuParameters(VLEN = 256)
  })), Array("--target-dir", "generated",
    "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}



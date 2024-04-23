package darecreek.exu.crosslane.perm

import chisel3._
import chisel3.util._
import darecreek.exu.vfucore._
import darecreek.exu.vfucore.perm._
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.fp.VFPU
import darecreek.exu.vfucoreconfig.{VUop, Redirect}
import darecreek.exu.crosslane.PermRdRF
import darecreek._

class Permutation(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VExuInput))
    val redirect = Input(new Redirect)
    val out = Decoupled(new VCrossExuOut)
    val perm = new PermRdRF
  })

  val uop = io.in.bits.uop
  val funct6 = uop.ctrl.funct6
  val funct3 = uop.ctrl.funct3
  val vsew = uop.info.vsew
  val uopIdx = io.in.bits.uop.expdIdx
  val psrc = io.in.bits.uop.psrc
  val fire = io.in.fire

  val perm = Module(new PermutationCore()(p))

  val vrgather16 = (funct6 === "b001110".U) && (funct3 === "b000".U)
  val vrgather16_sew8 = vrgather16 && (vsew === 0.U)

  val vs1_preg_idx = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val vs2_preg_idx = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val old_vd_preg_idx = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val mask_preg_idx = RegInit(0.U(8.W))
  val uop_valid = RegInit(false.B)
  val perm_busy = perm.io.out.perm_busy
  val uop_reg = Reg(new VUop)
  val rs1_reg = Reg(UInt(XLEN.W))

  val q = Module(new Queue(new VExpdUOp, 8))
  q.io.enq.bits := uop
  q.io.enq.valid := (fire && !vrgather16_sew8) || (fire && vrgather16_sew8 && uopIdx(0))

  val uop_out = q.io.deq.bits
  q.io.deq.ready := perm.io.out.wb_vld

  when(fire) {
    vs2_preg_idx(uopIdx) := psrc(1)
    when(vrgather16_sew8) {
      vs2_preg_idx(uopIdx(2, 1)) := psrc(1)
    }
  }

  when(fire) {
    uop_reg := uop
    rs1_reg := io.in.bits.rs1
    vs1_preg_idx(uopIdx) := psrc(0)
    old_vd_preg_idx(uopIdx) := psrc(2)
    mask_preg_idx := psrc(3)
  }

  uop_valid := false.B
  when(fire && uop.expdEnd) {
    uop_valid := true.B
  }

  perm.io.in.uop := uop_reg
  perm.io.in.rs1 := rs1_reg
  perm.io.in.vs1_preg_idx := vs1_preg_idx
  perm.io.in.vs2_preg_idx := vs2_preg_idx
  perm.io.in.old_vd_preg_idx := old_vd_preg_idx
  perm.io.in.mask_preg_idx := mask_preg_idx
  perm.io.in.uop_valid := uop_valid
  perm.io.in.rdata := io.perm.rdata
  perm.io.in.rvalid := io.perm.rvalid
  perm.io.redirect := io.redirect

  io.perm.rd_en := perm.io.out.rd_en
  io.perm.rd_preg_idx := perm.io.out.rd_preg_idx
  io.out.bits.uop := uop_out
  io.out.valid := perm.io.out.wb_vld
  io.out.bits.vd := VecInit(Seq.tabulate(NLanes)(i => (perm.io.out.wb_data)((i + 1) * LaneWidth - 1, i * LaneWidth)))
  io.out.bits.fflags := 0.U

  io.in.ready := !(uop_valid | perm_busy)
}

object VerilogPerm extends App {
  println("Generating hardware")
  val p = Parameters.empty
  emitVerilog(new Permutation()(p.alterPartial({ case VFuParamsKey =>
    VFuParameters(VLEN = 256)
  })), Array("--target-dir", "generated",
    "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}


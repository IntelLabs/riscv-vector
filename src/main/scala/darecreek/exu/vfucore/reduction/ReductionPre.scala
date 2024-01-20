package darecreek.exu.vfucore.reduction

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
// import darecreek.exu.vfucore._
import chipsalliance.rocketchip.config._
import darecreek._

class ReductionPre extends Module {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new VExuInput))
    val vd_reg = Input(UInt(VLEN.W))
    val vs2m_bits_widen = Output(UInt((2 * VLEN).W))
    val vs1_zero = Output(UInt(64.W))
    val vs1_zero_logical = Output(UInt(64.W))
  })

  val vd_reg = io.vd_reg
  val widen = io.in.bits.uop.ctrl.widen
  val widen2 = io.in.bits.uop.ctrl.widen2
  val vl = io.in.bits.uop.info.vl
  val vstart = io.in.bits.uop.info.vstart
  val in_vstart_gte_vl = vstart >= vl
  val narrow_to_1 = io.in.bits.uop.ctrl.narrow_to_1 & !in_vstart_gte_vl
  val vxrm = io.in.bits.uop.info.vxrm
  val frm = io.in.bits.uop.info.frm

  val vlenbWidth = log2Up(vlenb) + 1
  val uop = io.in.bits.uop
  val vs1 = Cat(io.in.bits.vSrc(0).reverse)
  val vs2 = Cat(io.in.bits.vSrc(1).reverse)
  val old_vd = Cat(io.in.bits.vSrc(2).reverse)
  val vmask = Cat(io.in.bits.vSrc(3).reverse)
  val rs1 = io.in.bits.rs1
  val funct6 = uop.ctrl.funct6
  val funct3 = uop.ctrl.funct3
  val imme = uop.ctrl.lsrc(0)
  val vs1_index = uop.ctrl.lsrc(0)
  val vs2_index = uop.ctrl.lsrc(1)
  val narrow = io.in.bits.uop.ctrl.narrow
  val expdLen = uop.expdLen
  val uopIdx = uop.expdIdx
  val uopEnd = uop.expdEnd
  val vm = uop.ctrl.vm
  val vlmul = uop.info.vlmul
  val vsew = uop.info.vsew
  val ma = uop.info.ma
  val ta = uop.info.ta
  val fire = io.in.fire

  val vredsum_vs = (funct6 === "b000000".U) && (funct3 === "b010".U)
  val vredmax_vs = (funct6 === "b000111".U) && (funct3 === "b010".U)
  val vredmaxu_vs = (funct6 === "b000110".U) && (funct3 === "b010".U)
  val vredmin_vs = (funct6 === "b000101".U) && (funct3 === "b010".U)
  val vredminu_vs = (funct6 === "b000100".U) && (funct3 === "b010".U)
  val vredand_vs = (funct6 === "b000001".U) && (funct3 === "b010".U)
  val vredor_vs = (funct6 === "b000010".U) && (funct3 === "b010".U)
  val vredxor_vs = (funct6 === "b000011".U) && (funct3 === "b010".U)
  val vwredsum_vs = (funct6 === "b110001".U) && (funct3 === "b000".U)
  val vwredsumu_vs = (funct6 === "b110000".U) && (funct3 === "b000".U)
  val vfredosum_vs = ((funct6 === "b000011".U) && (funct3 === "b001".U)) || ((funct6 === "b000001".U) && (funct3 === "b001".U))
  // val vfredusum_vs = (funct6 === "b000001".U) && (funct3 === "b001".U)
  val vfredusum_vs = false.B
  val vfredmax_vs = (funct6 === "b000111".U) && (funct3 === "b001".U)
  val vfredmin_vs = (funct6 === "b000101".U) && (funct3 === "b001".U)
  val vfwredosum_vs = ((funct6 === "b110011".U) && (funct3 === "b001".U)) || ((funct6 === "b110001".U) && (funct3 === "b001".U))
  // val vfwredusum_vs = (funct6 === "b110001".U) && (funct3 === "b001".U)
  val vfwredusum_vs = false.B


  val signed = vredmax_vs || vredmin_vs || vwredsum_vs
  val is_max = vredmax_vs || vredmaxu_vs
  val is_min = vredmin_vs || vredminu_vs

  val vd_vsew = Mux(widen | widen2, vsew + 1.U, vsew)
  val eew = SewOH(vsew)
  val eewVd = SewOH(vd_vsew)
  val vsew_bytes = Mux1H(eew.oneHot, Seq(1.U(4.W), 2.U(4.W), 4.U(4.W), 8.U(4.W)))
  val vd_vsew_bytes = Mux1H(eewVd.oneHot, Seq(1.U(4.W), 2.U(4.W), 4.U(4.W), 8.U(4.W)))
  val vd_vsew_bits = Mux1H(eewVd.oneHot, Seq(8.U(7.W), 16.U(7.W), 32.U(7.W), 64.U(7.W)))
  val vlRemainBytes = Wire(UInt(10.W))
  val vd_mask = (~0.U(VLEN.W))
  val vd_mask_vl = Wire(UInt(VLEN.W))
  val vmask_vl = Wire(UInt(VLEN.W))
  val vmask_uop = MaskExtract(vmask_vl, uopIdx, eew)
  val vmask_16b = MaskReorg.splash(vmask_uop, eew, vlenb)

  val vl_reg = RegEnable(vl, 0.U, fire)
  vd_mask_vl := vd_mask >> (VLEN.U - vl)
  vmask_vl := vmask & vd_mask_vl
  vlRemainBytes := Mux((vl << vsew) >= Cat(uopIdx, 0.U(log2Up(vlenb).W)), (vl << vsew) - Cat(uopIdx, 0.U(log2Up(vlenb).W)), 0.U)
  val reg_fire = RegNext(fire)
  val reg_widen = RegEnable(widen, false.B, fire)
  val reg_vsew = RegEnable(vsew, 0.U, fire)
  val reg_vd_vsew = RegEnable(vd_vsew, 0.U, fire)
  val reg_eew = SewOH(reg_vsew)
  val reg_eewVd = SewOH(reg_vd_vsew)
  val vs1_reg = RegEnable(vs1, 0.U, fire)
  val old_vd_reg = RegEnable(old_vd, 0.U, fire)
  val reg_signed = RegEnable(signed, false.B, fire)
  val ta_reg = RegEnable(ta, false.B, fire)
  val reg_vredsum_vs = RegEnable(vredsum_vs, false.B, fire)
  val reg_vredmax_vs = RegEnable(vredmax_vs, false.B, fire)
  val reg_vredmaxu_vs = RegEnable(vredmaxu_vs, false.B, fire)
  val reg_vredmin_vs = RegEnable(vredmin_vs, false.B, fire)
  val reg_vredminu_vs = RegEnable(vredminu_vs, false.B, fire)
  val reg_vredand_vs = RegEnable(vredand_vs, false.B, fire)
  val reg_vredor_vs = RegEnable(vredor_vs, false.B, fire)
  val reg_vredxor_vs = RegEnable(vredxor_vs, false.B, fire)
  val reg_vwredsum_vs = RegEnable(vwredsum_vs, false.B, fire)
  val reg_vwredsumu_vs = RegEnable(vwredsumu_vs, false.B, fire)
  val reg_is_max = RegEnable(is_max, false.B, fire)
  val reg_is_min = RegEnable(is_min, false.B, fire)
  val reg_uopIdx = RegEnable(uopIdx, 0.U, fire)
  val vd_vsew_bits_reg = RegEnable(vd_vsew_bits, 0.U, fire)

  val vl_reg2 = RegEnable(vl_reg, 0.U, reg_fire)
  val reg2_fire = RegNext(reg_fire)
  val old_vd_reg2 = RegEnable(old_vd_reg, 0.U, reg_fire)
  val ta_reg2 = RegEnable(ta_reg, false.B, reg_fire)
  val reg2_vredsum_vs = RegEnable(reg_vredsum_vs, false.B, reg_fire)
  val reg2_vredmax_vs = RegEnable(reg_vredmax_vs, false.B, reg_fire)
  val reg2_vredmaxu_vs = RegEnable(reg_vredmaxu_vs, false.B, reg_fire)
  val reg2_vredmin_vs = RegEnable(reg_vredmin_vs, false.B, reg_fire)
  val reg2_vredminu_vs = RegEnable(reg_vredminu_vs, false.B, reg_fire)
  val reg2_vredand_vs = RegEnable(reg_vredand_vs, false.B, reg_fire)
  val reg2_vredor_vs = RegEnable(reg_vredor_vs, false.B, reg_fire)
  val reg2_vredxor_vs = RegEnable(reg_vredxor_vs, false.B, reg_fire)
  val reg2_vwredsum_vs = RegEnable(reg_vwredsum_vs, false.B, reg_fire)
  val reg2_vwredsumu_vs = RegEnable(reg_vwredsumu_vs, false.B, reg_fire)
  val reg2_is_max = RegEnable(reg_is_max, false.B, reg_fire)
  val reg2_is_min = RegEnable(reg_is_min, false.B, reg_fire)
  val reg2_uopIdx = RegEnable(reg_uopIdx, 0.U, reg_fire)
  val reg2_signed = RegEnable(reg_signed, false.B, reg_fire)
  val reg2_vd_vsew = RegEnable(reg_vd_vsew, 0.U, reg_fire)
  val reg2_eewVd = SewOH(reg2_vd_vsew)
  val vd_vsew_bits_reg2 = RegEnable(vd_vsew_bits_reg, 0.U, reg_fire)

  val vl_reg3 = RegEnable(vl_reg2, 0.U, reg2_fire)
  val old_vd_reg3 = RegEnable(old_vd_reg2, 0.U, reg2_fire)
  val ta_reg3 = RegEnable(ta_reg2, false.B, reg2_fire)
  val reg3_vredsum_vs = RegEnable(reg2_vredsum_vs, false.B, reg2_fire)
  val reg3_vredmax_vs = RegEnable(reg2_vredmax_vs, false.B, reg2_fire)
  val reg3_vredmaxu_vs = RegEnable(reg2_vredmaxu_vs, false.B, reg2_fire)
  val reg3_vredmin_vs = RegEnable(reg2_vredmin_vs, false.B, reg2_fire)
  val reg3_vredminu_vs = RegEnable(reg2_vredminu_vs, false.B, reg2_fire)
  val reg3_vredand_vs = RegEnable(reg2_vredand_vs, false.B, reg2_fire)
  val reg3_vredor_vs = RegEnable(reg2_vredor_vs, false.B, reg2_fire)
  val reg3_vredxor_vs = RegEnable(reg2_vredxor_vs, false.B, reg2_fire)
  val reg3_vwredsum_vs = RegEnable(reg2_vwredsum_vs, false.B, reg2_fire)
  val reg3_vwredsumu_vs = RegEnable(reg2_vwredsumu_vs, false.B, reg2_fire)
  val reg3_is_max = RegEnable(reg2_is_max, false.B, reg2_fire)
  val reg3_is_min = RegEnable(reg2_is_min, false.B, reg2_fire)
  val reg3_uopIdx = RegEnable(reg2_uopIdx, 0.U, reg2_fire)
  val reg3_vd_vsew = RegEnable(reg2_vd_vsew, 0.U, reg2_fire)
  val reg3_eewVd = SewOH(reg3_vd_vsew)
  val vd_vsew_bits_reg3 = RegEnable(vd_vsew_bits_reg2, 0.U, reg2_fire)

  val vs1_zero = Wire(UInt(64.W))
  val vs1_zero_logical = Wire(UInt(64.W))
  val red_zero = Wire(UInt(64.W))
  val red_zero_logical = Wire(UInt(64.W))
  val vs10_zero = RegInit(0.U(64.W))
  val vs10_zero_logical = RegInit(0.U(64.W))

  def zero(w: Int) = 0.U(w.W)

  def umax(w: Int) = ~(0.U(w.W))

  def smax(w: Int) = Cat(0.U(1.W), ~(0.U((w - 1).W)))

  def smin(w: Int) = Cat(1.U(1.W), 0.U((w - 1).W))

  val ele8 = Wire(UInt(8.W))
  val ele16 = Wire(UInt(16.W))
  val ele32 = Wire(UInt(32.W))
  val ele64 = Wire(UInt(64.W))

  ele8 := 0.U
  when(fire) {
    when(vredmax_vs) {
      ele8 := smin(8)
    }.elsewhen(vredmin_vs) {
      ele8 := smax(8)
    }.elsewhen(vredminu_vs || vredand_vs) {
      ele8 := umax(8)
    }
  }

  ele16 := 0.U
  when(fire) {
    when(vredmax_vs) {
      ele16 := smin(16)
    }.elsewhen(vredmin_vs) {
      ele16 := smax(16)
    }.elsewhen(vredminu_vs || vredand_vs) {
      ele16 := umax(16)
    }
  }

  ele32 := 0.U
  when(fire) {
    when(vredmax_vs) {
      ele32 := smin(32)
    }.elsewhen(vredmin_vs) {
      ele32 := smax(32)
    }.elsewhen(vredminu_vs || vredand_vs) {
      ele32 := umax(32)
    }
  }

  ele64 := 0.U
  when(fire) {
    when(vredmax_vs) {
      ele64 := smin(64)
    }.elsewhen(vredmin_vs) {
      ele64 := smax(64)
    }.elsewhen(vredminu_vs || vredand_vs) {
      ele64 := umax(64)
    }
  }

  val vs2_bytes = VecInit(Seq.tabulate(vlenb)(i => vs2((i + 1) * 8 - 1, i * 8)))
  val vs2m_bytes_sew8 = Wire(Vec(vlenb, UInt(8.W)))
  val vs2m_bytes_sew16 = Wire(Vec(vlenb, UInt(8.W)))
  val vs2m_bytes_sew32 = Wire(Vec(vlenb, UInt(8.W)))
  val vs2m_bytes_sew64 = Wire(Vec(vlenb, UInt(8.W)))
  val vs2m_bits = Wire(UInt((2 * VLEN).W))
  val reg_vs2m_bits = RegEnable(vs2m_bits, 0.U, fire)
  val vs2m_bits_sew8 = Cat(vs2m_bytes_sew8.reverse)
  val vs2m_bits_sew16 = Cat(vs2m_bytes_sew16.reverse)
  val vs2m_bits_sew32 = Cat(vs2m_bytes_sew32.reverse)
  val vs2m_bits_sew64 = Cat(vs2m_bytes_sew64.reverse)
  val reg_vs2m_bits_sew8 = RegEnable(vs2m_bits_sew8, 0.U, fire)
  val reg_vs2m_bits_sew16 = RegEnable(vs2m_bits_sew16, 0.U, fire)
  val reg_vs2m_bits_sew32 = RegEnable(vs2m_bits_sew32, 0.U, fire)
  val reg_vs2m_bits_sew64 = RegEnable(vs2m_bits_sew64, 0.U, fire)

  for (i <- 0 until vlenb) {
    vs2m_bytes_sew8(i) := vs2_bytes(i)
    when(((!vm && !vmask_16b(i)) || (i.U >= vlRemainBytes))) {
      vs2m_bytes_sew8(i) := ele8
    }
  }

  for (i <- 0 until vlenb / 2) {
    for (j <- 0 until 2) {
      vs2m_bytes_sew16(2 * i + j) := vs2_bytes(2 * i + j)
      when(((!vm && !vmask_16b(2 * i + j)) || ((Cat(i.U, 0.U(1.W)) + j.U) >= vlRemainBytes))) {
        vs2m_bytes_sew16(2 * i + j) := ele16(8 * (j + 1) - 1, 8 * j)
      }
    }
  }

  for (i <- 0 until vlenb / 4) {
    for (j <- 0 until 4) {
      vs2m_bytes_sew32(4 * i + j) := vs2_bytes(4 * i + j)
      when(((!vm && !vmask_16b(4 * i + j)) || ((Cat(i.U, 0.U(2.W)) + j.U) >= vlRemainBytes))) {
        vs2m_bytes_sew32(4 * i + j) := ele32(8 * (j + 1) - 1, 8 * j)
      }
    }
  }

  for (i <- 0 until vlenb / 8) {
    for (j <- 0 until 8) {
      vs2m_bytes_sew64(8 * i + j) := vs2_bytes(8 * i + j)
      when(((!vm && !vmask_16b(8 * i + j)) || ((Cat(i.U, 0.U(3.W)) + j.U) >= vlRemainBytes))) {
        vs2m_bytes_sew64(8 * i + j) := ele64(8 * (j + 1) - 1, 8 * j)
      }
    }
  }

  vs2m_bits := vs2m_bits_sew8
  when(reg_vsew === 1.U) {
    vs2m_bits := vs2m_bits_sew16
  }.elsewhen(reg_vsew === 2.U) {
    vs2m_bits := vs2m_bits_sew32
  }.elsewhen(reg_vsew === 3.U) {
    vs2m_bits := vs2m_bits_sew64
  }


  val vs2m_bits_widen = Wire(UInt((2 * VLEN).W))
  val reg_vs2m_bits_widen = RegEnable(vs2m_bits_widen, 0.U, fire)


  // Widen
  val vs_hi_widen = Mux1H(eew.oneHot(2, 0), Seq(8, 16, 32).map(sew =>
    Cat(UIntSplit(vs2m_bits(VLEN - 1, VLEN / 2), sew).map(BitsExtend(_, 2 * sew, signed)).reverse)))
  val vs_lo_widen = Mux1H(eew.oneHot(2, 0), Seq(8, 16, 32).map(sew =>
    Cat(UIntSplit(vs2m_bits(VLEN / 2 - 1, 0), sew).map(BitsExtend(_, 2 * sew, signed)).reverse)))
  vs2m_bits_widen := Mux(widen, Cat(vs_hi_widen, vs_lo_widen), Cat(0.U(VLEN.W), vs2m_bits))

  vs10_zero := Mux1H(reg_eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 0.U), vs1_reg(n - 1, 0))) :+ vs1_reg(63, 0))

  vs10_zero_logical := Mux1H(reg_eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 0.U), vs1_reg(n - 1, 0))) :+ vs1_reg(63, 0))
  when(vredand_vs) {
    vs10_zero_logical := Mux1H(reg_eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 1.U), vs1_reg(n - 1, 0))) :+ vs1_reg(63, 0))
  }

  red_zero := Mux1H(reg3_eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 0.U), vd_reg(n - 1, 0))) :+ vd_reg(63, 0))

  red_zero_logical := Mux1H(reg3_eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 0.U), vd_reg(n - 1, 0))) :+ vd_reg(63, 0))
  when(reg3_vredand_vs) {
    red_zero_logical := Mux1H(reg3_eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 1.U), vd_reg(n - 1, 0))) :+ vd_reg(63, 0))
  }

  vs1_zero := Mux(reg2_uopIdx === 0.U, vs10_zero, red_zero)
  vs1_zero_logical := Mux(reg2_uopIdx === 0.U, vs10_zero_logical, red_zero_logical)

  io.vs2m_bits_widen := reg_vs2m_bits_widen
  io.vs1_zero := vs1_zero
  io.vs1_zero_logical := vs1_zero_logical
}

object MaskExtract {
  def VLEN = 256

  def apply(vmask: UInt, uopIdx: UInt, sew: SewOH) = {
    val extracted = Wire(UInt((VLEN / 8).W))
    extracted := Mux1H(Seq.tabulate(8)(uopIdx === _.U),
      Seq.tabulate(8)(idx => Mux1H(sew.oneHot, Seq(VLEN / 8, VLEN / 16, VLEN / 32, VLEN / 64).map(stride =>
        vmask((idx + 1) * stride - 1, idx * stride)))))
    extracted
  }
}

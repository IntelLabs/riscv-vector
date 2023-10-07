package darecreek.exu.vfu.reduction

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import darecreek.exu.vfu._
// import darecreek.exu.vfu.VFUParam._
import org.chipsalliance.cde.config._

class Reduction(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new VFuInput))
    val out = ValidIO(new VAluOutput)
  })

  val funct6 = io.in.bits.uop.ctrl.funct6
  val funct3 = io.in.bits.uop.ctrl.funct3
  val vm = io.in.bits.uop.ctrl.vm
  val vs1_imm = io.in.bits.uop.ctrl.vs1_imm
  val ma = io.in.bits.uop.info.ma
  val ta = io.in.bits.uop.info.ta
  val vsew = io.in.bits.uop.info.vsew
  val vlmul = io.in.bits.uop.info.vlmul
  val vl = io.in.bits.uop.info.vl
  val vstart = io.in.bits.uop.info.vstart
  val uopIdx = io.in.bits.uop.uopIdx
  val uopEnd = io.in.bits.uop.uopEnd
  val vs1 = io.in.bits.vs1
  val vs2 = io.in.bits.vs2
  val rs1 = io.in.bits.rs1
  val old_vd = io.in.bits.oldVd
  val vmask = io.in.bits.mask
  val fire = io.in.valid

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


  val vsew_plus1 = Wire(UInt(3.W))
  vsew_plus1 := Cat(0.U(1.W), ~vsew) + 1.U
  val signed = vredmax_vs || vredmin_vs || vwredsum_vs
  val widen = vwredsum_vs || vwredsumu_vs
  val is_max = vredmax_vs || vredmaxu_vs
  val is_min = vredmin_vs || vredminu_vs
  val vsew_bytes = 1.U << vsew
  val vsew_bits = 8.U << vsew
  val vd_vsew = Mux(widen, vsew + 1.U, vsew)
  val vd_vsew_bytes = 1.U << vd_vsew
  val vd_vsew_bits = 8.U << vd_vsew
  val vlRemainBytes = Wire(UInt(8.W))
  val eew = SewOH(vsew)
  val eewVd = SewOH(vd_vsew)
  val vd_mask = (~0.U(VLEN.W))
  val vd_mask_vl = Wire(UInt(VLEN.W))
  val vmask_vl = Wire(UInt(VLEN.W))
  val vmask_uop = MaskExtract(vmask_vl, uopIdx, eew)
  val vmask_16b = MaskReorg.splash(vmask_uop, eew)
  vd_mask_vl := vd_mask >> (VLEN.U - vl)
  vmask_vl := vmask & vd_mask_vl
  vlRemainBytes := Mux((vl << vsew) >= Cat(uopIdx, 0.U(4.W)), (vl << vsew) - Cat(uopIdx, 0.U(4.W)), 0.U)
  val vd_reg = RegInit(0.U(128.W))
  val old_vd_reg = RegEnable(old_vd, 0.U, fire)
  val signed_reg = RegEnable(signed, false.B, fire)
  val vd_vsew_reg = RegEnable(vd_vsew, 0.U, fire)
  val ta_reg = RegEnable(ta, false.B, fire)
  val vl_reg = RegEnable(vl, 0.U, fire)
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
  val vs1_zero = Wire(UInt(64.W))
  val vs1_zero_logical = Wire(UInt(64.W))
  val vd_logical = Wire(Vec(4, UInt(64.W)))
  val sum_vd = Wire(UInt(64.W))
  val max_vd = Wire(UInt(64.W))
  val logical_vd = Cat(0.U((VLEN - 64).W), vd_logical(~vd_vsew(1, 0)))
  val red_vd = Wire(UInt(VLEN.W))
  val vs1_mux = Mux(uopIdx === 0.U, vs1, red_vd)

  val vd_vsew_bits_reg = 8.U << vd_vsew_reg
  val eewVd_reg = SewOH(vd_vsew_reg)
  val reg_vred_logical = reg_vredand_vs || reg_vredor_vs || reg_vredxor_vs

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

  val vs2_bytes = VecInit(Seq.tabulate(VLENB)(i => vs2((i + 1) * 8 - 1, i * 8)))
  val vs2m_bytes_sew8 = Wire(Vec(VLENB, UInt(8.W)))
  val vs2m_bytes_sew16 = Wire(Vec(VLENB, UInt(8.W)))
  val vs2m_bytes_sew32 = Wire(Vec(VLENB, UInt(8.W)))
  val vs2m_bytes_sew64 = Wire(Vec(VLENB, UInt(8.W)))
  val vs2m_bits = Wire(UInt((2 * VLEN).W))
  val vs2m_bits_sew8 = Cat(vs2m_bytes_sew8.reverse)
  val vs2m_bits_sew16 = Cat(vs2m_bytes_sew16.reverse)
  val vs2m_bits_sew32 = Cat(vs2m_bytes_sew32.reverse)
  val vs2m_bits_sew64 = Cat(vs2m_bytes_sew64.reverse)

  for (i <- 0 until VLENB) {
    vs2m_bytes_sew8(i) := vs2_bytes(i)
    when(((!vm && !vmask_16b(i)) || (i.U >= vlRemainBytes))) {
      vs2m_bytes_sew8(i) := ele8
    }
  }

  for (i <- 0 until VLENB / 2) {
    for (j <- 0 until 2) {
      vs2m_bytes_sew16(2 * i + j) := vs2_bytes(2 * i + j)
      when(((!vm && !vmask_16b(2 * i + j)) || ((Cat(i.U, 0.U(1.W)) + j.U) >= vlRemainBytes))) {
        vs2m_bytes_sew16(2 * i + j) := ele16(8 * (j + 1) - 1, 8 * j)
      }
    }
  }

  for (i <- 0 until VLENB / 4) {
    for (j <- 0 until 4) {
      vs2m_bytes_sew32(4 * i + j) := vs2_bytes(4 * i + j)
      when(((!vm && !vmask_16b(4 * i + j)) || ((Cat(i.U, 0.U(2.W)) + j.U) >= vlRemainBytes))) {
        vs2m_bytes_sew32(4 * i + j) := ele32(8 * (j + 1) - 1, 8 * j)
      }
    }
  }

  for (i <- 0 until VLENB / 8) {
    for (j <- 0 until 8) {
      vs2m_bytes_sew64(8 * i + j) := vs2_bytes(8 * i + j)
      when(((!vm && !vmask_16b(8 * i + j)) || ((Cat(i.U, 0.U(3.W)) + j.U) >= vlRemainBytes))) {
        vs2m_bytes_sew64(8 * i + j) := ele64(8 * (j + 1) - 1, 8 * j)
      }
    }
  }

  vs2m_bits := vs2m_bits_sew8
  when(vsew === 1.U) {
    vs2m_bits := vs2m_bits_sew16
  }.elsewhen(vsew === 2.U) {
    vs2m_bits := vs2m_bits_sew32
  }.elsewhen(vsew === 3.U) {
    vs2m_bits := vs2m_bits_sew64
  }

  val vs2m_bits_widen = Wire(UInt((2 * VLEN).W))

  // Widen
  val vs_hi_widen = Mux1H(eew.oneHot(2, 0), Seq(8, 16, 32).map(sew =>
    Cat(UIntSplit(vs2m_bits(127, 64), sew).map(BitsExtend(_, 2 * sew, signed)).reverse)))
  val vs_lo_widen = Mux1H(eew.oneHot(2, 0), Seq(8, 16, 32).map(sew =>
    Cat(UIntSplit(vs2m_bits(63, 0), sew).map(BitsExtend(_, 2 * sew, signed)).reverse)))
  vs2m_bits_widen := Mux(widen, Cat(vs_hi_widen, vs_lo_widen), Cat(0.U(VLEN.W), vs2m_bits))

  vs1_zero := Mux1H(eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 0.U), vs1_mux(n - 1, 0))) :+ vs1_mux(63, 0))

  vs1_zero_logical := Mux1H(eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 0.U), vs1_mux(n - 1, 0))) :+ vs1_mux(63, 0))
  when(vredand_vs) {
    vs1_zero_logical := Mux1H(eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 1.U), vs1_mux(n - 1, 0))) :+ vs1_mux(63, 0))
  }

  for (i <- 0 until 4) {
    vd_logical(i) := 0.U
  }

  when(vredand_vs) {
    vd_logical(0) := vs1_zero_logical & vs2m_bits(127, 64) & vs2m_bits(63, 0)
  }.elsewhen(vredor_vs) {
    vd_logical(0) := vs1_zero_logical | vs2m_bits(127, 64) | vs2m_bits(63, 0)
  }.elsewhen(vredxor_vs) {
    vd_logical(0) := vs1_zero_logical ^ vs2m_bits(127, 64) ^ vs2m_bits(63, 0)
  }

  when(vredand_vs) {
    vd_logical(1) := vd_logical(0)(63, 32) & vd_logical(0)(31, 0)
  }.elsewhen(vredor_vs) {
    vd_logical(1) := vd_logical(0)(63, 32) | vd_logical(0)(31, 0)
  }.elsewhen(vredxor_vs) {
    vd_logical(1) := vd_logical(0)(63, 32) ^ vd_logical(0)(31, 0)
  }

  when(vredand_vs) {
    vd_logical(2) := vd_logical(1)(31, 16) & vd_logical(1)(15, 0)
  }.elsewhen(vredor_vs) {
    vd_logical(2) := vd_logical(1)(31, 16) | vd_logical(1)(15, 0)
  }.elsewhen(vredxor_vs) {
    vd_logical(2) := vd_logical(1)(31, 16) ^ vd_logical(1)(15, 0)
  }

  when(vredand_vs) {
    vd_logical(3) := vd_logical(2)(15, 8) & vd_logical(2)(7, 0)
  }.elsewhen(vredor_vs) {
    vd_logical(3) := vd_logical(2)(15, 8) | vd_logical(2)(7, 0)
  }.elsewhen(vredxor_vs) {
    vd_logical(3) := vd_logical(2)(15, 8) ^ vd_logical(2)(7, 0)
  }

  // sew64 sum
  val sum_sew64 = Wire(Vec(2, UInt(64.W)))
  val carry_sew64 = Wire(Vec(2, UInt(64.W)))
  val sum_add_sew64 = Wire(UInt(64.W))
  val vd_sew64 = Wire(UInt(64.W))

  val csa_3to2_sew64_0 = Module(new CSA3to2(width = 64))
  csa_3to2_sew64_0.io.in_a := vs2m_bits_widen(63, 0)
  csa_3to2_sew64_0.io.in_b := vs2m_bits_widen(127, 64)
  csa_3to2_sew64_0.io.in_c := vs2m_bits_widen(191, 128)
  sum_sew64(0) := csa_3to2_sew64_0.io.out_sum
  carry_sew64(0) := csa_3to2_sew64_0.io.out_car
  sum_add_sew64 := vs2m_bits_widen(255, 192) + vs1_zero(63, 0)

  val csa_3to2_sew64_1 = Module(new CSA3to2(width = 64))
  csa_3to2_sew64_1.io.in_a := sum_sew64(0)
  csa_3to2_sew64_1.io.in_b := carry_sew64(0)
  csa_3to2_sew64_1.io.in_c := sum_add_sew64
  sum_sew64(1) := csa_3to2_sew64_1.io.out_sum
  carry_sew64(1) := csa_3to2_sew64_1.io.out_car
  vd_sew64 := vd_reg(127, 64) + vd_reg(63, 0)

  // sew32 sum
  val sum0_sew32 = Wire(Vec(3, UInt(32.W)))
  val carry0_sew32 = Wire(Vec(3, UInt(32.W)))
  val sum1_sew32 = Wire(Vec(2, UInt(32.W)))
  val carry1_sew32 = Wire(Vec(2, UInt(32.W)))
  val sum2_sew32 = Wire(UInt(32.W))
  val carry2_sew32 = Wire(UInt(32.W))
  val vd_sew32 = Wire(UInt(32.W))

  val in0_sew32 = Cat(vs1_zero(31, 0), vs2m_bits_widen(255, 0))
  val in1_sew32 = Cat(Cat(sum0_sew32.reverse), Cat(carry0_sew32.reverse))
  val in2_sew32 = Cat(Cat(sum1_sew32.reverse), Cat(carry1_sew32.reverse))

  for (i <- 0 until 3) {
    val csa_3to2_sew32 = Module(new CSA3to2(width = 32))
    csa_3to2_sew32.io.in_a := in0_sew32(96 * i + 31, 96 * i + 0)
    csa_3to2_sew32.io.in_b := in0_sew32(96 * i + 63, 96 * i + 32)
    csa_3to2_sew32.io.in_c := in0_sew32(96 * i + 95, 96 * i + 64)
    sum0_sew32(i) := csa_3to2_sew32.io.out_sum
    carry0_sew32(i) := csa_3to2_sew32.io.out_car
  }

  for (i <- 0 until 2) {
    val csa_3to2_sew32 = Module(new CSA3to2(width = 32))
    csa_3to2_sew32.io.in_a := in1_sew32(96 * i + 31, 96 * i + 0)
    csa_3to2_sew32.io.in_b := in1_sew32(96 * i + 63, 96 * i + 32)
    csa_3to2_sew32.io.in_c := in1_sew32(96 * i + 95, 96 * i + 64)
    sum1_sew32(i) := csa_3to2_sew32.io.out_sum
    carry1_sew32(i) := csa_3to2_sew32.io.out_car
  }

  val csa_4to2_sew32 = Module(new CSA4to2(width = 32))
  csa_4to2_sew32.io.in_a := in2_sew32(31, 0)
  csa_4to2_sew32.io.in_b := in2_sew32(63, 32)
  csa_4to2_sew32.io.in_c := in2_sew32(95, 64)
  csa_4to2_sew32.io.in_d := in2_sew32(127, 96)
  sum2_sew32 := csa_4to2_sew32.io.out_sum
  carry2_sew32 := csa_4to2_sew32.io.out_car
  vd_sew32 := vd_reg(63, 32) + vd_reg(31, 0)

  // sew16 sum
  val sum0_sew16 = Wire(Vec(4, UInt(16.W)))
  val carry0_sew16 = Wire(Vec(4, UInt(16.W)))
  val sum1_sew16 = Wire(Vec(3, UInt(16.W)))
  val carry1_sew16 = Wire(Vec(3, UInt(16.W)))
  val sum2_sew16 = Wire(Vec(2, UInt(16.W)))
  val carry2_sew16 = Wire(Vec(2, UInt(16.W)))
  val sum3_sew16 = Wire(UInt(16.W))
  val carry3_sew16 = Wire(UInt(16.W))
  val vd_sew16 = Wire(UInt(16.W))

  val in0_sew16 = vs2m_bits_widen
  val in1_sew16 = Cat(vs1_zero(15, 0), Cat(sum0_sew16.reverse), Cat(carry0_sew16.reverse))
  val in2_sew16 = Cat(Cat(sum1_sew16.reverse), Cat(carry1_sew16.reverse))
  val in3_sew16 = Cat(Cat(sum2_sew16.reverse), Cat(carry2_sew16.reverse))

  for (i <- 0 until 4) {
    val csa_4to2_sew16 = Module(new CSA4to2(width = 16))
    csa_4to2_sew16.io.in_a := in0_sew16(64 * i + 15, 64 * i + 0)
    csa_4to2_sew16.io.in_b := in0_sew16(64 * i + 31, 64 * i + 16)
    csa_4to2_sew16.io.in_c := in0_sew16(64 * i + 47, 64 * i + 32)
    csa_4to2_sew16.io.in_d := in0_sew16(64 * i + 63, 64 * i + 48)
    sum0_sew16(i) := csa_4to2_sew16.io.out_sum
    carry0_sew16(i) := csa_4to2_sew16.io.out_car
  }

  for (i <- 0 until 3) {
    val csa_3to2_sew16 = Module(new CSA3to2(width = 16))
    csa_3to2_sew16.io.in_a := in1_sew16(48 * i + 15, 48 * i + 0)
    csa_3to2_sew16.io.in_b := in1_sew16(48 * i + 31, 48 * i + 16)
    csa_3to2_sew16.io.in_c := in1_sew16(48 * i + 47, 48 * i + 32)
    sum1_sew16(i) := csa_3to2_sew16.io.out_sum
    carry1_sew16(i) := csa_3to2_sew16.io.out_car
  }

  for (i <- 0 until 2) {
    val csa_3to2_sew16 = Module(new CSA3to2(width = 16))
    csa_3to2_sew16.io.in_a := in2_sew16(48 * i + 15, 48 * i + 0)
    csa_3to2_sew16.io.in_b := in2_sew16(48 * i + 31, 48 * i + 16)
    csa_3to2_sew16.io.in_c := in2_sew16(48 * i + 47, 48 * i + 32)
    sum2_sew16(i) := csa_3to2_sew16.io.out_sum
    carry2_sew16(i) := csa_3to2_sew16.io.out_car
  }

  val csa_4to2_sew16 = Module(new CSA4to2(width = 16))
  csa_4to2_sew16.io.in_a := in3_sew16(15, 0)
  csa_4to2_sew16.io.in_b := in3_sew16(31, 16)
  csa_4to2_sew16.io.in_c := in3_sew16(47, 32)
  csa_4to2_sew16.io.in_d := in3_sew16(63, 48)
  sum3_sew16 := csa_4to2_sew16.io.out_sum
  carry3_sew16 := csa_4to2_sew16.io.out_car
  vd_sew16 := vd_reg(31, 16) + vd_reg(15, 0)

  // sew8 sum
  val sum0_sew8 = Wire(Vec(4, UInt(8.W)))
  val carry0_sew8 = Wire(Vec(4, UInt(8.W)))
  val sum1_sew8 = Wire(Vec(3, UInt(8.W)))
  val carry1_sew8 = Wire(Vec(3, UInt(8.W)))
  val sum2_sew8 = Wire(Vec(2, UInt(8.W)))
  val carry2_sew8 = Wire(Vec(2, UInt(8.W)))
  val sum3_sew8 = Wire(UInt(8.W))
  val carry3_sew8 = Wire(UInt(8.W))
  val vd_sew8 = Wire(UInt(8.W))

  val in0_sew8 = vs2m_bits_sew8(127, 0)
  val in1_sew8 = Cat(vs1_zero(7, 0), Cat(sum0_sew8.reverse), Cat(carry0_sew8.reverse))
  val in2_sew8 = Cat(Cat(sum1_sew8.reverse), Cat(carry1_sew8.reverse))
  val in3_sew8 = Cat(Cat(sum2_sew8.reverse), Cat(carry2_sew8.reverse))

  for (i <- 0 until 4) {
    val csa_4to2_sew8 = Module(new CSA4to2(width = 8))
    csa_4to2_sew8.io.in_a := in0_sew8(32 * i + 7, 32 * i + 0)
    csa_4to2_sew8.io.in_b := in0_sew8(32 * i + 15, 32 * i + 8)
    csa_4to2_sew8.io.in_c := in0_sew8(32 * i + 23, 32 * i + 16)
    csa_4to2_sew8.io.in_d := in0_sew8(32 * i + 31, 32 * i + 24)
    sum0_sew8(i) := csa_4to2_sew8.io.out_sum
    carry0_sew8(i) := csa_4to2_sew8.io.out_car
  }

  for (i <- 0 until 3) {
    val csa_3to2_sew8 = Module(new CSA3to2(width = 8))
    csa_3to2_sew8.io.in_a := in1_sew8(24 * i + 7, 24 * i + 0)
    csa_3to2_sew8.io.in_b := in1_sew8(24 * i + 15, 24 * i + 8)
    csa_3to2_sew8.io.in_c := in1_sew8(24 * i + 23, 24 * i + 16)
    sum1_sew8(i) := csa_3to2_sew8.io.out_sum
    carry1_sew8(i) := csa_3to2_sew8.io.out_car
  }

  for (i <- 0 until 2) {
    val csa_3to2_sew8 = Module(new CSA3to2(width = 8))
    csa_3to2_sew8.io.in_a := in2_sew8(24 * i + 7, 24 * i + 0)
    csa_3to2_sew8.io.in_b := in2_sew8(24 * i + 15, 24 * i + 8)
    csa_3to2_sew8.io.in_c := in2_sew8(24 * i + 23, 24 * i + 16)
    sum2_sew8(i) := csa_3to2_sew8.io.out_sum
    carry2_sew8(i) := csa_3to2_sew8.io.out_car
  }

  val csa_4to2_sew8 = Module(new CSA4to2(width = 8))
  csa_4to2_sew8.io.in_a := in3_sew8(7, 0)
  csa_4to2_sew8.io.in_b := in3_sew8(15, 8)
  csa_4to2_sew8.io.in_c := in3_sew8(23, 16)
  csa_4to2_sew8.io.in_d := in3_sew8(31, 24)
  sum3_sew8 := csa_4to2_sew8.io.out_sum
  carry3_sew8 := csa_4to2_sew8.io.out_car
  vd_sew8 := vd_reg(15, 8) + vd_reg(7, 0)

  // sew64 max/min
  val vd_max_sew64 = Wire(UInt(64.W))

  val compare_3to1_sew64 = Module(new compare_3to1(w = 64))
  compare_3to1_sew64.io.a := vs2m_bits_sew64(63, 0)
  compare_3to1_sew64.io.b := vs2m_bits_sew64(127, 64)
  compare_3to1_sew64.io.c := vs1_zero
  compare_3to1_sew64.io.max := is_max
  compare_3to1_sew64.io.signed := signed
  vd_max_sew64 := compare_3to1_sew64.io.d

  // sew32 max/min
  val vd0_max_sew32 = Wire(Vec(2, UInt(32.W)))
  val vd1_max_sew32 = Wire(UInt(32.W))

  val compare_3to1_sew32 = Module(new compare_3to1(w = 32))
  compare_3to1_sew32.io.a := vs2m_bits_sew32(31, 0)
  compare_3to1_sew32.io.b := vs2m_bits_sew32(63, 32)
  compare_3to1_sew32.io.c := vs2m_bits_sew32(95, 64)
  compare_3to1_sew32.io.max := is_max
  compare_3to1_sew32.io.signed := signed
  vd0_max_sew32(0) := compare_3to1_sew32.io.d

  val compare0_2to1_sew32 = Module(new compare_2to1(w = 32))
  compare0_2to1_sew32.io.a := vs2m_bits_sew32(127, 96)
  compare0_2to1_sew32.io.b := vs1_zero(31, 0)
  compare0_2to1_sew32.io.max := is_max
  compare0_2to1_sew32.io.signed := signed
  vd0_max_sew32(1) := compare0_2to1_sew32.io.c

  val compare1_2to1_sew32 = Module(new compare_2to1(w = 32))
  compare1_2to1_sew32.io.a := vd_reg(31, 0)
  compare1_2to1_sew32.io.b := vd_reg(63, 32)
  compare1_2to1_sew32.io.max := reg_is_max
  compare1_2to1_sew32.io.signed := signed_reg
  vd1_max_sew32 := compare1_2to1_sew32.io.c

  // sew16 max/min
  val vd0_max_sew16 = Wire(Vec(3, UInt(16.W)))
  val vd1_max_sew16 = Wire(UInt(16.W))
  val in0_max_sew16 = Cat(vs1_zero(15, 0), vs2m_bits_sew16(127, 0))

  for (i <- 0 until 3) {
    val compare_3to1_sew16 = Module(new compare_3to1(w = 16))
    compare_3to1_sew16.io.a := in0_max_sew16(48 * i + 15, 48 * i + 0)
    compare_3to1_sew16.io.b := in0_max_sew16(48 * i + 31, 48 * i + 16)
    compare_3to1_sew16.io.c := in0_max_sew16(48 * i + 47, 48 * i + 32)
    compare_3to1_sew16.io.max := is_max
    compare_3to1_sew16.io.signed := signed
    vd0_max_sew16(i) := compare_3to1_sew16.io.d
  }

  val compare1_3to1_sew16 = Module(new compare_3to1(w = 16))
  compare1_3to1_sew16.io.a := vd_reg(15, 0)
  compare1_3to1_sew16.io.b := vd_reg(31, 16)
  compare1_3to1_sew16.io.c := vd_reg(47, 32)
  compare1_3to1_sew16.io.max := reg_is_max
  compare1_3to1_sew16.io.signed := signed_reg
  vd1_max_sew16 := compare1_3to1_sew16.io.d

  // sew8 max/min
  val vd0_max_sew8 = Wire(Vec(6, UInt(8.W)))
  val vd1_max_sew8 = Wire(Vec(2, UInt(8.W)))
  val vd2_max_sew8 = Wire(UInt(8.W))
  val in2_max_sew8 = Cat(vd1_max_sew8.reverse)

  for (i <- 0 until 5) {
    val compare_3to1_sew8 = Module(new compare_3to1(w = 8))
    compare_3to1_sew8.io.a := vs2m_bits_sew8(24 * i + 7, 24 * i + 0)
    compare_3to1_sew8.io.b := vs2m_bits_sew8(24 * i + 15, 24 * i + 8)
    compare_3to1_sew8.io.c := vs2m_bits_sew8(24 * i + 23, 24 * i + 16)
    compare_3to1_sew8.io.max := is_max
    compare_3to1_sew8.io.signed := signed
    vd0_max_sew8(i) := compare_3to1_sew8.io.d
  }

  val compare0_2to1_sew8 = Module(new compare_2to1(w = 8))
  compare0_2to1_sew8.io.a := vs2m_bits_sew8(127, 120)
  compare0_2to1_sew8.io.b := vs1_zero(7, 0)
  compare0_2to1_sew8.io.max := is_max
  compare0_2to1_sew8.io.signed := signed
  vd0_max_sew8(5) := compare0_2to1_sew8.io.c

  for (i <- 0 until 2) {
    val compare_3to1_sew8 = Module(new compare_3to1(w = 8))
    compare_3to1_sew8.io.a := vd_reg(24 * i + 7, 24 * i + 0)
    compare_3to1_sew8.io.b := vd_reg(24 * i + 15, 24 * i + 8)
    compare_3to1_sew8.io.c := vd_reg(24 * i + 23, 24 * i + 16)
    compare_3to1_sew8.io.max := reg_is_max
    compare_3to1_sew8.io.signed := signed_reg
    vd1_max_sew8(i) := compare_3to1_sew8.io.d
  }

  val compare1_2to1_sew8 = Module(new compare_2to1(w = 8))
  compare1_2to1_sew8.io.a := in2_max_sew8(15, 8)
  compare1_2to1_sew8.io.b := in2_max_sew8(7, 0)
  compare1_2to1_sew8.io.max := reg_is_max
  compare1_2to1_sew8.io.signed := signed_reg
  vd2_max_sew8 := compare1_2to1_sew8.io.c

  sum_vd := vd_sew64
  when(vd_vsew_reg === 0.U) {
    sum_vd := vd_sew8
  }.elsewhen(vd_vsew_reg === 1.U) {
    sum_vd := vd_sew16
  }.elsewhen(vd_vsew_reg === 2.U) {
    sum_vd := vd_sew32
  }

  max_vd := vd_reg
  when(vd_vsew_reg === 0.U) {
    max_vd := vd2_max_sew8
  }.elsewhen(vd_vsew_reg === 1.U) {
    max_vd := vd1_max_sew16
  }.elsewhen(vd_vsew_reg === 2.U) {
    max_vd := vd1_max_sew32
  }

  red_vd := sum_vd
  when(reg_vredand_vs || reg_vredor_vs || reg_vredxor_vs) {
    red_vd := vd_reg
  }.elsewhen(reg_is_max || reg_is_min) {
    red_vd := max_vd
  }

  val red_vd_tail_one = (vd_mask << vd_vsew_bits_reg) | (red_vd & (vd_mask >> (VLEN.U - vd_vsew_bits_reg)))
  val red_vd_tail_vd = (old_vd_reg & (vd_mask << vd_vsew_bits_reg)) | (red_vd & (vd_mask >> (VLEN.U - vd_vsew_bits_reg)))

  val red_vd_tail = Mux(vl_reg === 0.U, old_vd_reg, Mux(ta_reg, red_vd_tail_one, red_vd_tail_vd))

  when(fire) {
    when(vredand_vs || vredor_vs || vredxor_vs) {
      vd_reg := logical_vd
    }.elsewhen(is_max || is_min) {
      when(vd_vsew === 0.U) {
        vd_reg := Cat(vd0_max_sew8.reverse)
      }.elsewhen(vd_vsew === 1.U) {
        vd_reg := Cat(vd0_max_sew16.reverse)
      }.elsewhen(vd_vsew === 2.U) {
        vd_reg := Cat(vd0_max_sew32.reverse)
      }.elsewhen(vd_vsew === 3.U) {
        vd_reg := vd_max_sew64
      }
    }.otherwise {
      when(vd_vsew === 0.U) {
        vd_reg := Cat(sum3_sew8, carry3_sew8)
      }.elsewhen(vd_vsew === 1.U) {
        vd_reg := Cat(sum3_sew16, carry3_sew16)
      }.elsewhen(vd_vsew === 2.U) {
        vd_reg := Cat(sum2_sew32, carry2_sew32)
      }.elsewhen(vd_vsew === 3.U) {
        vd_reg := Cat(sum_sew64(1), carry_sew64(1))
      }
    }
  }

  io.out.valid := RegNext(uopEnd && io.in.valid)
  io.out.bits.vd := red_vd_tail
  io.out.bits.vxsat := false.B
}

class Adder_xb(w: Int) extends Module {
  val io = IO(new Bundle() {
    val in1 = Input(UInt(w.W))
    val in2 = Input(UInt(w.W))
    val cin = Input(UInt(1.W))
    val cout = Output(UInt(1.W))
  })

  private val bits = Cat(0.U(1.W), io.in1, io.cin) + Cat(0.U(1.W), io.in2, io.cin)
  io.cout := bits(w + 1)
}

class compare_2to1(w: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt(w.W))
    val b = Input(UInt(w.W))
    val max = Input(Bool())
    val signed = Input(Bool())
    val c = Output(UInt(w.W))
  })

  // a-b
  val b_inv = ~io.b
  val cout = Wire(Bool())
  val less = Wire(Bool())

  val adder_xb = Module(new Adder_xb(w = w))
  adder_xb.io.in1 := b_inv
  adder_xb.io.in2 := io.a
  adder_xb.io.cin := 1.U
  cout := adder_xb.io.cout
  less := Mux(io.signed, io.a(w - 1) ^ b_inv(w - 1) ^ cout, !cout)
  io.c := Mux(less === io.max, io.b, io.a)
}

class compare_3to1(w: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt(w.W))
    val b = Input(UInt(w.W))
    val c = Input(UInt(w.W))
    val max = Input(Bool())
    val signed = Input(Bool())
    val d = Output(UInt(w.W))
  })

  // a-b, a-c, b-c
  val vs_hi = Cat(io.a, io.a, io.b)
  val vs_lo = Cat(io.b, io.c, io.c)
  val vs_lo_inv = ~vs_lo
  val cout = Wire(Vec(3, Bool()))
  val less = Wire(Vec(3, Bool()))

  for (i <- 0 until 3) {
    val adder_xb = Module(new Adder_xb(w = w))
    adder_xb.io.in1 := vs_lo_inv(w * (i + 1) - 1, w * i)
    adder_xb.io.in2 := vs_hi(w * (i + 1) - 1, w * i)
    adder_xb.io.cin := 1.U
    cout(i) := adder_xb.io.cout
    less(i) := Mux(io.signed, vs_hi(w * (i + 1) - 1) ^ vs_lo_inv(w * (i + 1) - 1) ^ cout(i), !cout(i))
  }

  io.d := 0.U
  when((less(2) && less(1) && !io.max) || (!less(2) && !less(1) && io.max)) {
    io.d := io.a
  }.elsewhen((!less(2) && less(0) && !io.max) || (less(2) && !less(0) && io.max)) {
    io.d := io.b
  }.elsewhen((!less(1) && !less(0) && !io.max) || (less(1) && less(0) && io.max)) {
    io.d := io.c
  }
}


class CSA3to2(width: Int) extends Module {
  val io = IO(new Bundle() {
    val in_a = Input(UInt(width.W))
    val in_b = Input(UInt(width.W))
    val in_c = Input(UInt(width.W))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))

  })
  io.out_sum := io.in_a ^ io.in_b ^ io.in_c
  io.out_car := Cat(((io.in_a & io.in_b) | (io.in_a & io.in_c) | (io.in_b & io.in_c)) (width - 2, 0), 0.U)


}

class CSA4to2(width: Int) extends Module {
  val io = IO(new Bundle() {
    val in_a = Input(UInt(width.W))
    val in_b = Input(UInt(width.W))
    val in_c = Input(UInt(width.W))
    val in_d = Input(UInt(width.W))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))

  })
  val cout_vec = Wire(Vec(width, UInt(1.W)))
  val sum_vec = Wire(Vec(width, UInt(1.W)))
  val carry_vec = Wire(Vec(width, UInt(1.W)))
  val cin_0 = 0.U
  for (i <- 0 until width) {
    cout_vec(i) := Mux(io.in_a(i) ^ io.in_b(i), io.in_c(i), io.in_a(i))
    if (i == 0) {
      sum_vec(i) := io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i)
      carry_vec(i) := Mux(io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i), cin_0, io.in_d(i))
    }
    else {
      sum_vec(i) := io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i) ^ cout_vec(i - 1)
      carry_vec(i) := Mux(io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i), cout_vec(i - 1), io.in_d(i))
    }
  }

  val sum_temp_vec = Wire(Vec(width, UInt(1.W)))
  val carry_temp_vec = Wire(Vec(width, UInt(1.W)))
  carry_temp_vec(0) := 0.U
  sum_temp_vec(0) := sum_vec(0)
  for (i <- 1 until width) {
    if (i % 2 == 1) {
      carry_temp_vec(i) := sum_vec(i)
      sum_temp_vec(i) := carry_vec(i - 1)
    }
    else {
      carry_temp_vec(i) := carry_vec(i - 1)
      sum_temp_vec(i) := sum_vec(i)
    }
  }

  io.out_sum := sum_temp_vec.asUInt
  io.out_car := carry_temp_vec.asUInt
}

// object VerilogRed extends App {
//   println("Generating the VPU Reduction hardware")
//   emitVerilog(new Reduction(), Array("--target-dir", "build/vifu"))
// }

import xiangshan._
object Main extends App {
  println("Generating hardware")
  val p = Parameters.empty.alterPartial({case XSCoreParamsKey => XSCoreParameters()})
  emitVerilog(new Reduction()(p.alterPartial({case VFuParamsKey => VFuParameters()})), Array("--target-dir", "generated",
              "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}


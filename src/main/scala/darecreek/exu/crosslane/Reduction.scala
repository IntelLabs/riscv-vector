package darecreek.exu.crosslane.reduction

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.reduction._
import darecreek.exu.vfucore.{VFuModule, VFuParamsKey, VFuParameters}
import darecreek._
import darecreek.exu.vfucoreconfig.{VUop, Redirect}

class Reduction(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VExuInput))
    val redirect = Input(new Redirect)
    val out = Decoupled(new VCrossExuOut)
  })

  def latency = 3

  val validVec = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  val rdyVec = Array.fill(latency)(Wire(Bool())) :+ io.out.ready
  val uopVec = io.in.bits.uop +: Array.fill(latency)(Reg(new VExpdUOp))
  // val flushVec = Array.fill(latency + 1)(WireInit(false.B)) // FIXME: implement flush logic
  // val flushVec = validVec.zip(uopVec).map(x => x._1 && x._2.vRobIdx.needFlush(io.redirect))  //XS
  val flushVec = validVec.zip(uopVec).map(x => x._1 && io.redirect.needFlush(
                   x._2.asTypeOf(new darecreek.exu.vfucoreconfig.VUop).robIdx))

  for (i <- 0 until latency) {
    rdyVec(i) := !validVec(i + 1) || rdyVec(i + 1)
  }

  for (i <- 1 to latency) {
    when(regEnable(i)) {
      validVec(i) := validVec(i - 1)
      uopVec(i) := uopVec(i - 1)
    }.elsewhen(flushVec(i) || rdyVec(i)) {
      validVec(i) := false.B
    }
  }


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
  val vlRemainBytes = Wire(UInt(8.W))
  val vd_mask = (~0.U(VLEN.W))
  val vd_mask_vl = Wire(UInt(VLEN.W))
  val vmask_vl = Wire(UInt(VLEN.W))
  val vmask_uop = MaskExtract(vmask_vl, uopIdx, eew)
  val vmask_16b = MaskReorg.splash(vmask_uop, eew, vlenb)

  val vl_reg = RegEnable(vl, 0.U, fire)
  vd_mask_vl := vd_mask >> (VLEN.U - vl)
  vmask_vl := vmask & vd_mask_vl
  vlRemainBytes := Mux((vl << vsew) >= Cat(uopIdx, 0.U(4.W)), (vl << vsew) - Cat(uopIdx, 0.U(4.W)), 0.U)
  val reg_fire = RegNext(fire)
  val reg2_fire = RegNext(reg_fire)
  val reg_widen = RegEnable(widen, false.B, fire)
  val reg_vsew = RegEnable(vsew, 0.U, fire)
  val reg_vd_vsew = RegEnable(vd_vsew, 0.U, fire)
  val reg_eew = SewOH(reg_vsew)
  val reg_eewVd = SewOH(reg_vd_vsew)
  val vd_reg = RegInit(0.U(128.W))
  val vs1_reg = RegEnable(vs1, 0.U, fire)
  val vl_reg2 = RegEnable(vl_reg, 0.U, reg_fire)
  val vl_reg3 = RegEnable(vl_reg2, 0.U, reg2_fire)
  val old_vd_reg = RegEnable(old_vd, 0.U, fire)
  val old_vd_reg2 = RegEnable(old_vd_reg, 0.U, reg_fire)
  val old_vd_reg3 = RegEnable(old_vd_reg2, 0.U, reg2_fire)
  val reg_signed = RegEnable(signed, false.B, fire)
  val ta_reg = RegEnable(ta, false.B, fire)
  val ta_reg2 = RegEnable(ta_reg, false.B, reg_fire)
  val ta_reg3 = RegEnable(ta_reg2, false.B, reg2_fire)
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

  val reg2_signed = RegEnable(reg_signed, false.B, reg_fire)
  val reg2_vd_vsew = RegEnable(reg_vd_vsew, 0.U, reg_fire)
  val reg3_vd_vsew = RegEnable(reg2_vd_vsew, 0.U, reg2_fire)
  val reg2_eewVd = SewOH(reg2_vd_vsew)
  val reg3_eewVd = SewOH(reg3_vd_vsew)

  val vs10_zero = RegInit(0.U(64.W))
  val vs10_zero_logical = RegInit(0.U(64.W))
  val vd_logical = Wire(UInt(64.W))
  val logical_vd = Cat(0.U((VLEN - 64).W), vd_logical)

  val vd_vsew_bits_reg = RegEnable(vd_vsew_bits, 0.U, fire)
  val vd_vsew_bits_reg2 = RegEnable(vd_vsew_bits_reg, 0.U, reg_fire)
  val vd_vsew_bits_reg3 = RegEnable(vd_vsew_bits_reg2, 0.U, reg2_fire)

  val vs2m_bits_widen = Wire(UInt((2 * VLEN).W))
  val vs1_zero = Wire(UInt(64.W))
  val vs1_zero_logical = Wire(UInt(64.W))

  val redpre = Module(new ReductionPre)
  redpre.io.in.bits := io.in.bits
  redpre.io.vd_reg := vd_reg
  redpre.io.in.valid := fire
  vs2m_bits_widen := redpre.io.vs2m_bits_widen
  vs1_zero := redpre.io.vs1_zero
  vs1_zero_logical := redpre.io.vs1_zero_logical

  val rs_vd = Wire(Vec(VLEN / 128, UInt(128.W)))
  val rs = Seq.fill(VLEN / 128)(Module(new ReductionSlice))
  for (i <- 0 until NLanes / 2) {
    rs(i).io.funct6 := uopVec(1).ctrl.funct6
    rs(i).io.funct3 := uopVec(1).ctrl.funct3
    rs(i).io.vsew := uopVec(1).info.vsew
    rs(i).io.vs2 := Mux(widen, vs2m_bits_widen((i + 1) * VLEN - 1, i * VLEN), Cat(0.U((VLEN / 2).W), vs2m_bits_widen((i + 1) * VLEN / 2 - 1, i * VLEN / 2)))
    rs_vd(i) := rs(i).io.vd
  }

  vd_logical := 0.U

  when(reg2_vredand_vs) {
    vd_logical := vs1_zero_logical & rs_vd(0)(63, 0) & rs_vd(1)(63, 0)
  }.elsewhen(reg2_vredor_vs) {
    vd_logical := vs1_zero_logical | rs_vd(0)(63, 0) | rs_vd(1)(63, 0)
  }.elsewhen(reg2_vredxor_vs) {
    vd_logical := vs1_zero_logical ^ rs_vd(0)(63, 0) ^ rs_vd(1)(63, 0)
  }

  // sew64 sum
  val sum_sew64 = Wire(Vec(2, UInt(64.W)))
  val carry_sew64 = Wire(Vec(2, UInt(64.W)))
  val sum_add_sew64 = Wire(UInt(64.W))
  val in0_sew64 = Cat(vs1_zero, Cat(rs_vd.reverse))
  val vd_sew64 = Wire(UInt(64.W))

  val csa_3to2_sew64_0 = Module(new CSA3to2(width = 64))
  csa_3to2_sew64_0.io.in_a := in0_sew64(63, 0)
  csa_3to2_sew64_0.io.in_b := in0_sew64(127, 64)
  csa_3to2_sew64_0.io.in_c := in0_sew64(191, 128)
  sum_sew64(0) := csa_3to2_sew64_0.io.out_sum
  carry_sew64(0) := csa_3to2_sew64_0.io.out_car
  sum_add_sew64 := in0_sew64(255, 192) + in0_sew64(319, 256)

  val csa_3to2_sew64_1 = Module(new CSA3to2(width = 64))
  csa_3to2_sew64_1.io.in_a := sum_sew64(0)
  csa_3to2_sew64_1.io.in_b := carry_sew64(0)
  csa_3to2_sew64_1.io.in_c := sum_add_sew64
  sum_sew64(1) := csa_3to2_sew64_1.io.out_sum
  carry_sew64(1) := csa_3to2_sew64_1.io.out_car
  vd_sew64 := sum_sew64(1) + carry_sew64(1)

  // sew32 sum
  val sum_sew32 = Wire(Vec(2, UInt(32.W)))
  val carry_sew32 = Wire(Vec(2, UInt(32.W)))
  val sum_add_sew32 = Wire(UInt(32.W))
  val in0_sew32 = Cat(vs1_zero(31, 0), rs_vd(0)(63, 0), rs_vd(1)(63, 0))
  val vd_sew32 = Wire(UInt(32.W))

  val csa_3to2_sew32_0 = Module(new CSA3to2(width = 32))
  csa_3to2_sew32_0.io.in_a := in0_sew32(31, 0)
  csa_3to2_sew32_0.io.in_b := in0_sew32(63, 32)
  csa_3to2_sew32_0.io.in_c := in0_sew32(95, 64)
  sum_sew32(0) := csa_3to2_sew32_0.io.out_sum
  carry_sew32(0) := csa_3to2_sew32_0.io.out_car
  sum_add_sew32 := in0_sew32(127, 96) + in0_sew32(159, 128)

  val csa_3to2_sew32_1 = Module(new CSA3to2(width = 32))
  csa_3to2_sew32_1.io.in_a := sum_sew32(0)
  csa_3to2_sew32_1.io.in_b := carry_sew32(0)
  csa_3to2_sew32_1.io.in_c := sum_add_sew32
  sum_sew32(1) := csa_3to2_sew32_1.io.out_sum
  carry_sew32(1) := csa_3to2_sew32_1.io.out_car
  vd_sew32 := sum_sew32(1) + carry_sew32(1)

  // sew16 sum
  val sum_sew16 = Wire(Vec(2, UInt(16.W)))
  val carry_sew16 = Wire(Vec(2, UInt(16.W)))
  val sum_add_sew16 = Wire(UInt(16.W))
  val in0_sew16 = Cat(vs1_zero(15, 0), rs_vd(0)(31, 0), rs_vd(1)(31, 0))
  val vd_sew16 = Wire(UInt(16.W))

  val csa_3to2_sew16_0 = Module(new CSA3to2(width = 16))
  csa_3to2_sew16_0.io.in_a := in0_sew16(15, 0)
  csa_3to2_sew16_0.io.in_b := in0_sew16(31, 16)
  csa_3to2_sew16_0.io.in_c := in0_sew16(47, 32)
  sum_sew16(0) := csa_3to2_sew16_0.io.out_sum
  carry_sew16(0) := csa_3to2_sew16_0.io.out_car
  sum_add_sew16 := in0_sew16(63, 48) + in0_sew16(79, 64)

  val csa_3to2_sew16_1 = Module(new CSA3to2(width = 16))
  csa_3to2_sew16_1.io.in_a := sum_sew16(0)
  csa_3to2_sew16_1.io.in_b := carry_sew16(0)
  csa_3to2_sew16_1.io.in_c := sum_add_sew16
  sum_sew16(1) := csa_3to2_sew16_1.io.out_sum
  carry_sew16(1) := csa_3to2_sew16_1.io.out_car
  vd_sew16 := sum_sew16(1) + carry_sew16(1)

  // sew8 sum
  val sum_sew8 = Wire(Vec(2, UInt(8.W)))
  val carry_sew8 = Wire(Vec(2, UInt(8.W)))
  val sum_add_sew8 = Wire(UInt(8.W))
  val in0_sew8 = Cat(vs1_zero(7, 0), rs_vd(0)(15, 0), rs_vd(1)(15, 0))
  val vd_sew8 = Wire(UInt(8.W))

  val csa_3to2_sew8_0 = Module(new CSA3to2(width = 8))
  csa_3to2_sew8_0.io.in_a := in0_sew8(7, 0)
  csa_3to2_sew8_0.io.in_b := in0_sew8(15, 8)
  csa_3to2_sew8_0.io.in_c := in0_sew8(23, 16)
  sum_sew8(0) := csa_3to2_sew8_0.io.out_sum
  carry_sew8(0) := csa_3to2_sew8_0.io.out_car
  sum_add_sew8 := in0_sew8(31, 24) + in0_sew8(39, 32)

  val csa_3to2_sew8_1 = Module(new CSA3to2(width = 8))
  csa_3to2_sew8_1.io.in_a := sum_sew8(0)
  csa_3to2_sew8_1.io.in_b := carry_sew8(0)
  csa_3to2_sew8_1.io.in_c := sum_add_sew8
  sum_sew8(1) := csa_3to2_sew8_1.io.out_sum
  carry_sew8(1) := csa_3to2_sew8_1.io.out_car
  vd_sew8 := sum_sew8(1) + carry_sew8(1)

  // sew64 max/min
  val in0_max_sew64 = Cat(vs1_zero, rs_vd(0)(63, 0), rs_vd(1)(63, 0))
  val vd_max_sew64 = Wire(UInt(64.W))

  val compare_3to1_sew64 = Module(new compare_3to1(w = 64))
  compare_3to1_sew64.io.a := in0_max_sew64(63, 0)
  compare_3to1_sew64.io.b := in0_max_sew64(127, 64)
  compare_3to1_sew64.io.c := in0_max_sew64(191, 128)
  compare_3to1_sew64.io.max := reg2_is_max
  compare_3to1_sew64.io.signed := reg2_signed
  vd_max_sew64 := compare_3to1_sew64.io.d

  // sew32 max/min
  val in0_max_sew32 = Cat(vs1_zero(31, 0), rs_vd(0)(31, 0), rs_vd(1)(31, 0))
  val vd_max_sew32 = Wire(UInt(32.W))

  val compare_3to1_sew32 = Module(new compare_3to1(w = 32))
  compare_3to1_sew32.io.a := in0_max_sew32(31, 0)
  compare_3to1_sew32.io.b := in0_max_sew32(63, 32)
  compare_3to1_sew32.io.c := in0_max_sew32(95, 64)
  compare_3to1_sew32.io.max := reg2_is_max
  compare_3to1_sew32.io.signed := reg2_signed
  vd_max_sew32 := compare_3to1_sew32.io.d

  // sew16 max/min
  val in0_max_sew16 = Cat(vs1_zero(15, 0), rs_vd(0)(15, 0), rs_vd(1)(15, 0))
  val vd_max_sew16 = Wire(UInt(16.W))

  val compare_3to1_sew16 = Module(new compare_3to1(w = 16))
  compare_3to1_sew16.io.a := in0_max_sew16(15, 0)
  compare_3to1_sew16.io.b := in0_max_sew16(31, 16)
  compare_3to1_sew16.io.c := in0_max_sew16(47, 32)
  compare_3to1_sew16.io.max := reg2_is_max
  compare_3to1_sew16.io.signed := reg2_signed
  vd_max_sew16 := compare_3to1_sew16.io.d

  // sew8 max/min
  val in0_max_sew8 = Cat(vs1_zero(7, 0), rs_vd(0)(7, 0), rs_vd(1)(7, 0))
  val vd_max_sew8 = Wire(UInt(8.W))

  val compare_3to1_sew8 = Module(new compare_3to1(w = 8))
  compare_3to1_sew8.io.a := in0_max_sew8(7, 0)
  compare_3to1_sew8.io.b := in0_max_sew8(15, 8)
  compare_3to1_sew8.io.c := in0_max_sew8(23, 16)
  compare_3to1_sew8.io.max := reg2_is_max
  compare_3to1_sew8.io.signed := reg2_signed
  vd_max_sew8 := compare_3to1_sew8.io.d

  val red_vd_tail_one = (vd_mask << vd_vsew_bits_reg3) | (vd_reg & (vd_mask >> (VLEN.U - vd_vsew_bits_reg3)))
  val red_vd_tail_vd = (old_vd_reg3 & (vd_mask << vd_vsew_bits_reg3)) | (vd_reg & (vd_mask >> (VLEN.U - vd_vsew_bits_reg3)))

  val red_vd_tail = Mux(vl_reg3 === 0.U, old_vd_reg3, Mux(ta_reg3, red_vd_tail_one, red_vd_tail_vd))

  when(reg2_fire) {
    when(reg2_vredand_vs || reg2_vredor_vs || reg2_vredxor_vs) {
      vd_reg := logical_vd
    }.elsewhen(reg2_is_max || reg2_is_min) {
      when(reg2_vd_vsew === 0.U) {
        vd_reg := vd_max_sew8
      }.elsewhen(reg2_vd_vsew === 1.U) {
        vd_reg := vd_max_sew16
      }.elsewhen(reg2_vd_vsew === 2.U) {
        vd_reg := vd_max_sew32
      }.elsewhen(reg2_vd_vsew === 3.U) {
        vd_reg := vd_max_sew64
      }
    }.otherwise {
      when(reg2_vd_vsew === 0.U) {
        vd_reg := vd_sew8
      }.elsewhen(reg2_vd_vsew === 1.U) {
        vd_reg := vd_sew16
      }.elsewhen(reg2_vd_vsew === 2.U) {
        vd_reg := vd_sew32
      }.elsewhen(reg2_vd_vsew === 3.U) {
        vd_reg := vd_sew64
      }
    }
  }


  io.in.ready := rdyVec(0)
  io.out.valid := uopVec.last.expdEnd && validVec.last
  io.out.bits.uop := uopVec.last
  io.out.bits.vd := VecInit(Seq.tabulate(NLanes)(i => (red_vd_tail) ((i + 1) * LaneWidth - 1, i * LaneWidth)))
  io.out.bits.fflags := 0.U

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

// object MaskReorg {
//   // sew = 8: unchanged, sew = 16: 00000000abcdefgh -> aabbccddeeffgghh, ...
//   def splash(bits: UInt, sew: SewOH): UInt = {
//     Mux1H(sew.oneHot, Seq(1, 2, 4, 8).map(k => Cat(bits(vlenb / k - 1, 0).asBools.map(Fill(k, _)).reverse)))
//   }
//   // // sew = 8: unchanged, sew = 16: 00000000abcdefgh -> 0000abcd0000efgh, ...
//   // def apply(bits: UInt, sew: SewOH): UInt = {
//   //   Mux1H(sew.oneHot, Seq(1,2,4,8).map(k => Cat(UIntSplit(bits(16/k -1, 0), 2).map(_ | 0.U(8.W)).reverse)))
//   // }
// }

object VerilogRed extends App {
  println("Generating hardware")
  val p = Parameters.empty
  emitVerilog(new Reduction()(p.alterPartial({ case VFuParamsKey =>
    VFuParameters(VLEN = 256)
  })), Array("--target-dir", "generated",
    "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}
package darecreek.exu.vfucore.reduction

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import darecreek.exu.vfucore._
import chipsalliance.rocketchip.config._

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

class ReductionSlice extends Module {
  val io = IO(new Bundle {
    val pipe_enable = Input(Bool())
    val funct6 = Input(UInt(6.W))
    val funct3 = Input(UInt(3.W))
    val vsew = Input(UInt(3.W))
    val vs2 = Input(UInt(256.W))
    val vd = Output(UInt(128.W))
  })

  val funct6 = io.funct6
  val funct3 = io.funct3
  val vsew = io.vsew
  val vs2 = io.vs2
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
  val vfredosum_vs = (funct6 === "b000011".U) && (funct3 === "b001".U)
  val vfredusum_vs = (funct6 === "b000001".U) && (funct3 === "b001".U)
  val vfredmax_vs = (funct6 === "b000111".U) && (funct3 === "b001".U)
  val vfredmin_vs = (funct6 === "b000101".U) && (funct3 === "b001".U)
  val vfwredosum_vs = (funct6 === "b110011".U) && (funct3 === "b001".U)
  val vfwredusum_vs = (funct6 === "b110001".U) && (funct3 === "b001".U)

  val signed = vredmax_vs || vredmin_vs || vwredsum_vs
  val widen = vwredsum_vs || vwredsumu_vs
  val is_max = vredmax_vs || vredmaxu_vs
  val is_min = vredmin_vs || vredminu_vs

  val vd_vsew = Mux(widen, vsew + 1.U, vsew)
  val vd_reg = RegInit(0.U(128.W))

  val vd_logical = Wire(Vec(4, UInt(64.W)))


  val logical_vd = Cat(0.U((128 - 64).W), vd_logical(~vd_vsew(1, 0)))


  for (i <- 0 until 4) {
    vd_logical(i) := 0.U
  }

  when(vredand_vs) {
    vd_logical(0) := vs2(127, 64) & vs2(63, 0)
  }.elsewhen(vredor_vs) {
    vd_logical(0) := vs2(127, 64) | vs2(63, 0)
  }.elsewhen(vredxor_vs) {
    vd_logical(0) := vs2(127, 64) ^ vs2(63, 0)
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
  val sum_sew64 = Wire(UInt(64.W))
  val carry_sew64 = Wire(UInt(64.W))
  val vd_sew64 = Wire(UInt(128.W))

  val csa_4to2_sew64 = Module(new CSA4to2(width = 64))
  csa_4to2_sew64.io.in_a := vs2(63, 0)
  csa_4to2_sew64.io.in_b := vs2(127, 64)
  csa_4to2_sew64.io.in_c := vs2(191, 128)
  csa_4to2_sew64.io.in_d := vs2(255, 192)
  sum_sew64 := csa_4to2_sew64.io.out_sum
  carry_sew64 := csa_4to2_sew64.io.out_car
  vd_sew64 := Cat(sum_sew64, carry_sew64)

  // sew32 sum
  val sum0_sew32 = Wire(Vec(2, UInt(32.W)))
  val carry0_sew32 = Wire(Vec(2, UInt(32.W)))
  val sum1_sew32 = Wire(UInt(32.W))
  val carry1_sew32 = Wire(UInt(32.W))
  val vd_sew32 = Wire(UInt(64.W))

  val in1_sew32 = Cat(Cat(sum0_sew32.reverse), Cat(carry0_sew32.reverse))

  for (i <- 0 until 2) {
    val csa_4to2_sew32 = Module(new CSA4to2(width = 32))
    csa_4to2_sew32.io.in_a := vs2(128 * i + 31, 128 * i + 0)
    csa_4to2_sew32.io.in_b := vs2(128 * i + 63, 128 * i + 32)
    csa_4to2_sew32.io.in_c := vs2(128 * i + 95, 128 * i + 64)
    csa_4to2_sew32.io.in_d := vs2(128 * i + 127, 128 * i + 96)
    sum0_sew32(i) := csa_4to2_sew32.io.out_sum
    carry0_sew32(i) := csa_4to2_sew32.io.out_car
  }

  val csa_4to2_sew32 = Module(new CSA4to2(width = 32))
  csa_4to2_sew32.io.in_a := in1_sew32(31, 0)
  csa_4to2_sew32.io.in_b := in1_sew32(63, 32)
  csa_4to2_sew32.io.in_c := in1_sew32(95, 64)
  csa_4to2_sew32.io.in_d := in1_sew32(127, 96)
  sum1_sew32 := csa_4to2_sew32.io.out_sum
  carry1_sew32 := csa_4to2_sew32.io.out_car
  vd_sew32 := Cat(sum1_sew32, carry1_sew32)

  // sew16 sum
  val sum0_sew16 = Wire(Vec(4, UInt(16.W)))
  val carry0_sew16 = Wire(Vec(4, UInt(16.W)))
  val sum1_sew16 = Wire(Vec(2, UInt(16.W)))
  val carry1_sew16 = Wire(Vec(2, UInt(16.W)))
  val sum2_sew16 = Wire(UInt(16.W))
  val carry2_sew16 = Wire(UInt(16.W))
  val vd_sew16 = Wire(UInt(32.W))

  val in0_sew16 = vs2
  val in1_sew16 = Cat(Cat(sum0_sew16.reverse), Cat(carry0_sew16.reverse))
  val in2_sew16 = Cat(Cat(sum1_sew16.reverse), Cat(carry1_sew16.reverse))
  val in3_sew16 = Cat(sum2_sew16, carry2_sew16)

  for (i <- 0 until 4) {
    val csa_4to2_sew16 = Module(new CSA4to2(width = 16))
    csa_4to2_sew16.io.in_a := in0_sew16(64 * i + 15, 64 * i + 0)
    csa_4to2_sew16.io.in_b := in0_sew16(64 * i + 31, 64 * i + 16)
    csa_4to2_sew16.io.in_c := in0_sew16(64 * i + 47, 64 * i + 32)
    csa_4to2_sew16.io.in_d := in0_sew16(64 * i + 63, 64 * i + 48)
    sum0_sew16(i) := csa_4to2_sew16.io.out_sum
    carry0_sew16(i) := csa_4to2_sew16.io.out_car
  }

  for (i <- 0 until 2) {
    val csa_4to2_sew16 = Module(new CSA4to2(width = 16))
    csa_4to2_sew16.io.in_a := in1_sew16(64 * i + 15, 64 * i + 0)
    csa_4to2_sew16.io.in_b := in1_sew16(64 * i + 31, 64 * i + 16)
    csa_4to2_sew16.io.in_c := in1_sew16(64 * i + 47, 64 * i + 32)
    csa_4to2_sew16.io.in_d := in1_sew16(64 * i + 63, 64 * i + 48)
    sum1_sew16(i) := csa_4to2_sew16.io.out_sum
    carry1_sew16(i) := csa_4to2_sew16.io.out_car
  }

  val csa_4to2_sew16 = Module(new CSA4to2(width = 16))
  csa_4to2_sew16.io.in_a := in2_sew16(15, 0)
  csa_4to2_sew16.io.in_b := in2_sew16(31, 16)
  csa_4to2_sew16.io.in_c := in2_sew16(47, 32)
  csa_4to2_sew16.io.in_d := in2_sew16(63, 48)
  sum2_sew16 := csa_4to2_sew16.io.out_sum
  carry2_sew16 := csa_4to2_sew16.io.out_car

  vd_sew16 := Cat(sum2_sew16, carry2_sew16)

  // sew8 sum
  val sum0_sew8 = Wire(Vec(4, UInt(8.W)))
  val carry0_sew8 = Wire(Vec(4, UInt(8.W)))
  val sum1_sew8 = Wire(Vec(2, UInt(8.W)))
  val carry1_sew8 = Wire(Vec(2, UInt(8.W)))
  val sum2_sew8 = Wire(UInt(8.W))
  val carry2_sew8 = Wire(UInt(8.W))
  val vd_sew8 = Wire(UInt(16.W))

  val in0_sew8 = vs2(127, 0)
  val in1_sew8 = Cat(Cat(sum0_sew8.reverse), Cat(carry0_sew8.reverse))
  val in2_sew8 = Cat(Cat(sum1_sew8.reverse), Cat(carry1_sew8.reverse))

  for (i <- 0 until 4) {
    val csa_4to2_sew8 = Module(new CSA4to2(width = 8))
    csa_4to2_sew8.io.in_a := in0_sew8(32 * i + 7, 32 * i + 0)
    csa_4to2_sew8.io.in_b := in0_sew8(32 * i + 15, 32 * i + 8)
    csa_4to2_sew8.io.in_c := in0_sew8(32 * i + 23, 32 * i + 16)
    csa_4to2_sew8.io.in_d := in0_sew8(32 * i + 31, 32 * i + 24)
    sum0_sew8(i) := csa_4to2_sew8.io.out_sum
    carry0_sew8(i) := csa_4to2_sew8.io.out_car
  }

  for (i <- 0 until 2) {
    val csa_4to2_sew8 = Module(new CSA4to2(width = 8))
    csa_4to2_sew8.io.in_a := in1_sew8(32 * i + 7, 32 * i + 0)
    csa_4to2_sew8.io.in_b := in1_sew8(32 * i + 15, 32 * i + 8)
    csa_4to2_sew8.io.in_c := in1_sew8(32 * i + 23, 32 * i + 16)
    csa_4to2_sew8.io.in_d := in1_sew8(32 * i + 31, 32 * i + 24)
    sum1_sew8(i) := csa_4to2_sew8.io.out_sum
    carry1_sew8(i) := csa_4to2_sew8.io.out_car
  }

  val csa_4to2_sew8 = Module(new CSA4to2(width = 8))
  csa_4to2_sew8.io.in_a := in2_sew8(7, 0)
  csa_4to2_sew8.io.in_b := in2_sew8(15, 8)
  csa_4to2_sew8.io.in_c := in2_sew8(23, 16)
  csa_4to2_sew8.io.in_d := in2_sew8(31, 24)
  sum2_sew8 := csa_4to2_sew8.io.out_sum
  carry2_sew8 := csa_4to2_sew8.io.out_car
  vd_sew8 := Cat(sum2_sew8, carry2_sew8)

  // sew64 max/min
  val vd_max_sew64 = Wire(UInt(64.W))

  val compare_2to1_sew64 = Module(new compare_2to1(w = 64))
  compare_2to1_sew64.io.a := vs2(63, 0)
  compare_2to1_sew64.io.b := vs2(127, 64)
  compare_2to1_sew64.io.max := is_max
  compare_2to1_sew64.io.signed := signed
  vd_max_sew64 := compare_2to1_sew64.io.c

  // sew32 max/min
  val vd0_max_sew32 = Wire(Vec(2, UInt(32.W)))
  val vd1_max_sew32 = Wire(UInt(32.W))

  for (i <- 0 until 2) {
    val compare_2to1_sew32 = Module(new compare_2to1(w = 32))
    compare_2to1_sew32.io.a := vs2(64 * i + 31, 64 * i + 0)
    compare_2to1_sew32.io.b := vs2(64 * i + 63, 64 * i + 32)
    compare_2to1_sew32.io.max := is_max
    compare_2to1_sew32.io.signed := signed
    vd0_max_sew32(i) := compare_2to1_sew32.io.c
  }

  val compare_2to1_sew32 = Module(new compare_2to1(w = 32))
  compare_2to1_sew32.io.a := vd0_max_sew32(0)
  compare_2to1_sew32.io.b := vd0_max_sew32(1)
  compare_2to1_sew32.io.max := is_max
  compare_2to1_sew32.io.signed := signed
  vd1_max_sew32 := compare_2to1_sew32.io.c

  // sew16 max/min
  val vd0_max_sew16 = Wire(Vec(3, UInt(16.W)))
  val vd1_max_sew16 = Wire(UInt(16.W))
  val in0_max_sew16 = vs2(127, 0)

  for (i <- 0 until 2) {
    val compare_3to1_sew16 = Module(new compare_3to1(w = 16))
    compare_3to1_sew16.io.a := in0_max_sew16(48 * i + 15, 48 * i + 0)
    compare_3to1_sew16.io.b := in0_max_sew16(48 * i + 31, 48 * i + 16)
    compare_3to1_sew16.io.c := in0_max_sew16(48 * i + 47, 48 * i + 32)
    compare_3to1_sew16.io.max := is_max
    compare_3to1_sew16.io.signed := signed
    vd0_max_sew16(i) := compare_3to1_sew16.io.d
  }

  val compare_2to1_sew16 = Module(new compare_2to1(w = 16))
  compare_2to1_sew16.io.a := vs2(111, 96)
  compare_2to1_sew16.io.b := vs2(127, 112)
  compare_2to1_sew16.io.max := is_max
  compare_2to1_sew16.io.signed := signed
  vd0_max_sew16(2) := compare_2to1_sew16.io.c

  val compare1_3to1_sew16 = Module(new compare_3to1(w = 16))
  compare1_3to1_sew16.io.a := vd0_max_sew16(0)
  compare1_3to1_sew16.io.b := vd0_max_sew16(1)
  compare1_3to1_sew16.io.c := vd0_max_sew16(2)
  compare1_3to1_sew16.io.max := is_max
  compare1_3to1_sew16.io.signed := signed
  vd1_max_sew16 := compare1_3to1_sew16.io.d

  // sew8 max/min
  val vd0_max_sew8 = Wire(Vec(5, UInt(8.W)))
  val vd1_max_sew8 = Wire(Vec(2, UInt(8.W)))
  val vd2_max_sew8 = Wire(UInt(8.W))
  val in1_max_sew8 = Cat(vs2(127, 120), Cat(vd0_max_sew8.reverse))
  val in2_max_sew8 = Cat(vd1_max_sew8.reverse)

  for (i <- 0 until 5) {
    val compare_3to1_sew8 = Module(new compare_3to1(w = 8))
    compare_3to1_sew8.io.a := vs2(24 * i + 7, 24 * i + 0)
    compare_3to1_sew8.io.b := vs2(24 * i + 15, 24 * i + 8)
    compare_3to1_sew8.io.c := vs2(24 * i + 23, 24 * i + 16)
    compare_3to1_sew8.io.max := is_max
    compare_3to1_sew8.io.signed := signed
    vd0_max_sew8(i) := compare_3to1_sew8.io.d
  }

  for (i <- 0 until 2) {
    val compare_3to1_sew8 = Module(new compare_3to1(w = 8))
    compare_3to1_sew8.io.a := in1_max_sew8(24 * i + 7, 24 * i + 0)
    compare_3to1_sew8.io.b := in1_max_sew8(24 * i + 15, 24 * i + 8)
    compare_3to1_sew8.io.c := in1_max_sew8(24 * i + 23, 24 * i + 16)
    compare_3to1_sew8.io.max := is_max
    compare_3to1_sew8.io.signed := signed
    vd1_max_sew8(i) := compare_3to1_sew8.io.d
  }

  val compare1_2to1_sew8 = Module(new compare_2to1(w = 8))
  compare1_2to1_sew8.io.a := in2_max_sew8(15, 8)
  compare1_2to1_sew8.io.b := in2_max_sew8(7, 0)
  compare1_2to1_sew8.io.max := is_max
  compare1_2to1_sew8.io.signed := signed
  vd2_max_sew8 := compare1_2to1_sew8.io.c

  when(io.pipe_enable) {
    when(vredand_vs || vredor_vs || vredxor_vs) {
      vd_reg := logical_vd
    }.elsewhen(is_max || is_min) {
      when(vd_vsew === 0.U) {
        vd_reg := vd2_max_sew8
      }.elsewhen(vd_vsew === 1.U) {
        vd_reg := vd1_max_sew16
      }.elsewhen(vd_vsew === 2.U) {
        vd_reg := vd1_max_sew32
      }.elsewhen(vd_vsew === 3.U) {
        vd_reg := vd_max_sew64
      }
    }.otherwise {
      when(vd_vsew === 0.U) {
        vd_reg := vd_sew8
      }.elsewhen(vd_vsew === 1.U) {
        vd_reg := vd_sew16
      }.elsewhen(vd_vsew === 2.U) {
        vd_reg := vd_sew32
      }.elsewhen(vd_vsew === 3.U) {
        vd_reg := vd_sew64
      }
    }
  }
  io.vd := vd_reg
}


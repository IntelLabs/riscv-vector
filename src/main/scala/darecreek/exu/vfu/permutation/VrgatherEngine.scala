package darecreek.exu.vfu.perm

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
// import darecreek.exu.vfu.VFUParam._
import chipsalliance.rocketchip.config.Parameters

class VrgatherEngine(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val funct6 = Input(UInt(6.W))
    val funct3 = Input(UInt(3.W))
    val vm = Input(Bool())
    val ma = Input(Bool())
    val ta = Input(Bool())
    val vsew = Input(UInt(3.W))
    val vlmul = Input(UInt(3.W))
    val vl = Input(UInt(8.W))
    val first = Input(Bool())
    val update_vs2 = Input(Bool())
    val vs2_cnt = Input(UInt(3.W))
    val vd_idx = Input(UInt(3.W))
    val rs1 = Input(UInt(64.W))
    val vs1 = Input(UInt(128.W))
    val vs2 = Input(UInt(128.W))
    val old_vd = Input(UInt(128.W))
    val vmask = Input(UInt(128.W))
    val vd_reg = Input(UInt(128.W))

    val vrgather_vd = Output(UInt(128.W))
  })

  val funct6 = io.funct6
  val funct3 = io.funct3
  val vm = io.vm
  val ma = io.ma
  val ta = io.ta
  val vsew = io.vsew
  val vlmul = io.vlmul
  val vl = io.vl
  val first = io.first
  val update_vs2 = io.update_vs2
  val vs2_cnt = io.vs2_cnt
  val vd_idx = io.vd_idx
  val rs1 = io.rs1
  val vs1 = io.vs1
  val vs2 = io.vs2
  val old_vd = io.old_vd
  val vmask = io.vmask
  val vd_reg = io.vd_reg

  val vrgather_vv = (funct6 === "b001100".U) && (funct3 === "b000".U)
  val vrgather16 = (funct6 === "b001110".U) && (funct3 === "b000".U)
  val vrgather_vx = (funct6 === "b001100".U) && (funct3 === "b100".U)
  val vrgather_vi = (funct6 === "b001100".U) && (funct3 === "b011".U)
  val vrgather_vxi = vrgather_vx || vrgather_vi
  val vrgather16_sew8 = vrgather16 && (vsew === 0.U)
  val vrgather16_sew32 = vrgather16 && (vsew === 2.U)
  val vrgather16_sew64 = vrgather16 && (vsew === 3.U)
  val vrgather = vrgather_vv || vrgather_vxi || vrgather16

  val eew = SewOH(vsew)
  val vd_mask = (~0.U(VLEN.W))
  val vd_mask_vl = Wire(UInt(VLEN.W))
  val vmask_vl = Wire(UInt(VLEN.W))
  val vmask_uop = MaskExtract(vmask_vl, Mux(vrgather16_sew8, vd_idx(2, 1), vd_idx), eew)
  val vmask_16b = MaskReorg.splash(vmask_uop, eew)
  vd_mask_vl := vd_mask >> (VLEN.U - vl)
  vmask_vl := vmask & vd_mask_vl

  val vs1_vsew = Mux(vrgather16, 1.U, vsew)
  val vrgather_byte_sel = Wire(Vec(VLENB, UInt(72.W)))
  val vrgather_vd = Wire(Vec(VLENB, UInt(8.W)))

  val rs1_bytes = VecInit(Seq.tabulate(8)(i => rs1((i + 1) * 8 - 1, i * 8)))
  val vs1_bytes = VecInit(Seq.tabulate(VLENB)(i => vs1((i + 1) * 8 - 1, i * 8)))
  val vs2_bytes = VecInit(Seq.tabulate(VLENB)(i => vs2((i + 1) * 8 - 1, i * 8)))
  val old_vd_bytes = VecInit(Seq.tabulate(VLENB)(i => old_vd((i + 1) * 8 - 1, i * 8)))
  val vd_reg_bytes = VecInit(Seq.tabulate(VLENB)(i => vd_reg((i + 1) * 8 - 1, i * 8)))

  val vs2_cnt_plus1 = Wire(UInt(4.W))
  val vlmax_bytes = Wire(UInt(5.W))
  vs2_cnt_plus1 := Cat(0.U(1.W), vs2_cnt) + 1.U

  vlmax_bytes := VLENB.U
  when(vlmul === 5.U) {
    vlmax_bytes := (VLENB / 8).U
  }.elsewhen(vlmul === 6.U) {
    vlmax_bytes := (VLENB / 4).U
  }.elsewhen(vlmul === 7.U) {
    vlmax_bytes := (VLENB / 2).U
  }

  val vs2_min = Mux(update_vs2, Cat(vs2_cnt, 0.U(4.W)), "hff".U)
  val vs2_max = Mux(update_vs2, Mux(vlmul > 3.U, vlmax_bytes, Cat(vs2_cnt_plus1, 0.U(4.W))), "hff".U)

  for (i <- 0 until VLENB) {
    vrgather_byte_sel(i) := 0.U
    vrgather_vd(i) := Mux(ma, "hff".U, old_vd(i * 8 + 7, i * 8))
  }

  for (i <- 0 until VLENB / 2) {
    vrgather_byte_sel(i) := 0.U
    when(vrgather_vxi) {
      vrgather_byte_sel(i) := rs1
      when(vsew === 1.U) {
        vrgather_byte_sel(i) := Cat(rs1, 0.U(1.W)) + (i % 2).U
      }.elsewhen(vsew === 2.U) {
        vrgather_byte_sel(i) := Cat(rs1, 0.U(2.W)) + (i % 4).U
      }.elsewhen(vsew === 3.U) {
        vrgather_byte_sel(i) := Cat(rs1, 0.U(3.W)) + (i % 8).U
      }
    }.otherwise {
      when(vs1_vsew === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(vs1_vsew === 1.U) {
        when((vsew === 0.U) && !vd_idx(0)) {
          vrgather_byte_sel(i) := vs1((i + 1) * 16 - 1, i * 16)
        }.elsewhen(vsew === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + (i % 2).U
        }.elsewhen(vsew === 2.U) {
          when(vd_idx(0).asBool) {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + (i % 4).U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + (i % 4).U
          }
        }.elsewhen(vsew === 3.U) {
          when(vd_idx(1, 0) === 0.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + (i % 8).U
          }.elsewhen(vd_idx(1, 0) === 1.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + (i % 8).U
          }.elsewhen(vd_idx(1, 0) === 2.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 4) * 16 - 1, (i / 8 + 4) * 16), 0.U(3.W)) + (i % 8).U
          }.elsewhen(vd_idx(1, 0) === 3.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 6) * 16 - 1, (i / 8 + 6) * 16), 0.U(3.W)) + (i % 8).U
          }
        }
      }.elsewhen(vs1_vsew === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 32 - 1, i / 4 * 32), 0.U(2.W)) + (i % 4).U
      }.elsewhen(vs1_vsew === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 64 - 1, i / 8 * 64), 0.U(3.W)) + (i % 8).U
      }
    }
  }

  for (i <- (VLENB / 2) until VLENB) {
    vrgather_byte_sel(i) := 0.U
    when(vrgather_vxi) {
      vrgather_byte_sel(i) := rs1
      when(vsew === 1.U) {
        vrgather_byte_sel(i) := Cat(rs1, 0.U(1.W)) + (i % 2).U
      }.elsewhen(vsew === 2.U) {
        vrgather_byte_sel(i) := Cat(rs1, 0.U(2.W)) + (i % 4).U
      }.elsewhen(vsew === 3.U) {
        vrgather_byte_sel(i) := Cat(rs1, 0.U(3.W)) + (i % 8).U
      }
    }.otherwise {
      when(vs1_vsew === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(vs1_vsew === 1.U) {
        when((vsew === 0.U) && vd_idx(0)) {
          vrgather_byte_sel(i) := vs1((i + 1 - VLENB / 2) * 16 - 1, (i - VLENB / 2) * 16)
        }.elsewhen(vsew === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + (i % 2).U
        }.elsewhen(vsew === 2.U) {
          when(vd_idx(0).asBool) {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + (i % 4).U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + (i % 4).U
          }
        }.elsewhen(vsew === 3.U) {
          when(vd_idx(1, 0) === 0.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + (i % 8).U
          }.elsewhen(vd_idx(1, 0) === 1.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + (i % 8).U
          }.elsewhen(vd_idx(1, 0) === 2.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 4) * 16 - 1, (i / 8 + 4) * 16), 0.U(3.W)) + (i % 8).U
          }.elsewhen(vd_idx(1, 0) === 3.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 6) * 16 - 1, (i / 8 + 6) * 16), 0.U(3.W)) + (i % 8).U
          }
        }
      }.elsewhen(vs1_vsew === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 32 - 1, i / 4 * 32), 0.U(2.W)) + (i % 4).U
      }.elsewhen(vs1_vsew === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 64 - 1, i / 8 * 64), 0.U(3.W)) + (i % 8).U
      }
    }
  }

  when(vrgather && !vrgather16_sew8 && update_vs2) {
    for (i <- 0 until VLENB) {
      vrgather_vd(i) := Mux(first, old_vd_bytes(i), vd_reg_bytes(i))
      when(vmask_16b(i).asBool | vm) {
        when((vrgather_byte_sel(i) >= vs2_min) && (vrgather_byte_sel(i) < vs2_max)) {
          vrgather_vd(i) := vs2_bytes(vrgather_byte_sel(i) - vs2_min)
        }.elsewhen(first) {
          vrgather_vd(i) := 0.U
        }
      }.otherwise {
        vrgather_vd(i) := Mux(ma, "hff".U, old_vd_bytes(i))
      }
    }
  }.elsewhen(vrgather16_sew8 && update_vs2 && !vd_idx(0)) {
    for (i <- 0 until VLENB) {
      vrgather_vd(i) := Mux(first, old_vd_bytes(i), vd_reg_bytes(i))
    }

    for (i <- 0 until VLENB / 2) {
      when(vmask_16b(i).asBool | vm) {
        when((vrgather_byte_sel(i) >= vs2_min) && (vrgather_byte_sel(i) < vs2_max)) {
          vrgather_vd(i) := vs2_bytes(vrgather_byte_sel(i) - vs2_min)
        }.elsewhen(first) {
          vrgather_vd(i) := 0.U
        }
      }.otherwise {
        vrgather_vd(i) := Mux(ma, "hff".U, old_vd_bytes(i))
      }
    }
  }.elsewhen(vrgather16_sew8 && update_vs2 && vd_idx(0)) {
    for (i <- 0 until VLENB) {
      vrgather_vd(i) := vd_reg_bytes(i)
    }

    for (i <- VLENB / 2 until VLENB) {
      when(vmask_16b(i).asBool | vm) {
        when((vrgather_byte_sel(i) >= vs2_min) && (vrgather_byte_sel(i) < vs2_max)) {
          vrgather_vd(i) := vs2_bytes(vrgather_byte_sel(i) - vs2_min)
        }.elsewhen(first) {
          vrgather_vd(i) := 0.U
        }
      }.otherwise {
        vrgather_vd(i) := Mux(ma, "hff".U, old_vd_bytes(i))
      }
    }
  }

  io.vrgather_vd := Cat(vrgather_vd.reverse)
}

// object VerilogVrgather extends App {
//   println("Generating the VPU Vrgather hardware")
//   emitVerilog(new VrgatherEngine(), Array("--target-dir", "build/vifu"))

// }



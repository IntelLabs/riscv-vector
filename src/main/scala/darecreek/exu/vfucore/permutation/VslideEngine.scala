package darecreek.exu.vfucore.perm

import chisel3._
import chisel3.util._
import darecreek.exu.vfucore._
import chipsalliance.rocketchip.config.Parameters
import darecreek.exu.vfucore.perm._

class VslideEngine(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val funct6 = Input(UInt(6.W))
    val funct3 = Input(UInt(3.W))
    val vm = Input(Bool())
    val ma = Input(Bool())
    val vsew = Input(UInt(3.W))
    val vlmul = Input(UInt(3.W))
    val vl = Input(UInt(bVL.W))
    val vd_idx = Input(UInt(3.W))
    val rs1 = Input(UInt(XLEN.W))
    val vs2_hi = Input(UInt(VLEN.W))
    val vs2_lo = Input(UInt(VLEN.W))
    val old_vd = Input(UInt(VLEN.W))
    val vmask = Input(UInt(VLEN.W))

    val vslideup_vd = Output(UInt(VLEN.W))
    val vslidedn_vd = Output(UInt(VLEN.W))
    val vslide1up_vd = Output(UInt(VLEN.W))
    val vslide1dn_vd = Output(UInt(VLEN.W))
  })

  val funct6 = io.funct6
  val funct3 = io.funct3
  val vm = io.vm
  val ma = io.ma
  val vsew = io.vsew
  val vlmul = io.vlmul
  val vl = io.vl
  val vd_idx = io.vd_idx
  val rs1 = io.rs1
  val vs2_hi = io.vs2_hi
  val vs2_lo = io.vs2_lo
  val old_vd = io.old_vd
  val vmask = io.vmask

  val vslideup_vx = (funct6 === "b001110".U) && (funct3 === "b100".U)
  val vslideup_vi = (funct6 === "b001110".U) && (funct3 === "b011".U)
  val vslideup = vslideup_vx || vslideup_vi
  val vslidedn_vx = (funct6 === "b001111".U) && (funct3 === "b100".U)
  val vslidedn_vi = (funct6 === "b001111".U) && (funct3 === "b011".U)
  val vslidedn = vslidedn_vx || vslidedn_vi
  val vslide1up_vx = (funct6 === "b001110".U) && (funct3 === "b110".U)
  val vfslide1up_vf = (funct6 === "b001110".U) && (funct3 === "b101".U)
  val vslide1up = vslide1up_vx || vfslide1up_vf
  val vslide1dn_vx = (funct6 === "b001111".U) && (funct3 === "b110".U)
  val vfslide1dn_vf = (funct6 === "b001111".U) && (funct3 === "b101".U)
  val vslide1dn = vslide1dn_vx || vfslide1dn_vf
  val vslide = vslideup || vslidedn || vslide1up || vslide1dn

  val rd_vlmul = Wire(UInt(3.W))
  val vlmax_bytes = Wire(UInt((vlenbWidth + 1).W))
  val vl_bytes = vl << vsew
  val vl_idx = Wire(UInt(3.W))
  rd_vlmul := (1.U << Mux(vlmul(2), 0.U, Cat(0.U(1.W), vlmul(1, 0)))) - 1.U
  vl_idx := vl_bytes(bVL + 6, log2Up(VLEN) - 3) + vl_bytes(log2Up(VLEN) - 4, 0).orR - 1.U

  vlmax_bytes := VLENB.U
  when(vlmul === 5.U) {
    vlmax_bytes := (VLENB / 8).U
  }.elsewhen(vlmul === 6.U) {
    vlmax_bytes := (VLENB / 4).U
  }.elsewhen(vlmul === 7.U) {
    vlmax_bytes := (VLENB / 2).U
  }

  val vslideup_vd = Wire(Vec(VLENB, UInt(8.W)))
  val vslidedn_vd = Wire(Vec(VLENB, UInt(8.W)))
  val vslide1up_vd = Wire(Vec(VLENB, UInt(8.W)))
  val vslide1dn_vd = Wire(Vec(VLENB, UInt(8.W)))
  val vsew_bytes = Wire(UInt(4.W))
  val vmask_byte_strb = Wire(Vec(VLENB, UInt(1.W)))

  vsew_bytes := 1.U << vsew(1, 0)

  val vslide_ele = Mux(vslide1up || vslide1dn, 1.U, rs1)
  val vslide_bytes = vslide_ele << vsew

  val vslide_lo_valid = Mux(vslideup || vslide1up, vslide_bytes(70, (log2Up(VLEN) - 3)) +& 1.U <= vd_idx, (vslidedn || vslide1dn) && (vd_idx +& vslide_bytes(70, (log2Up(VLEN) - 3)) <= rd_vlmul))
  val vslide_hi_valid = Mux(vslideup || vslide1up, vslide_bytes(70, (log2Up(VLEN) - 3)) <= vd_idx, (vslidedn || vslide1dn) && (vd_idx +& vslide_bytes(70, (log2Up(VLEN) - 3)) +& 1.U <= rd_vlmul))

  val rs1_bytes = VecInit(Seq.tabulate(VLENB)(i => Cat(0.U((VLEN - 64).W), rs1)((i + 1) * 8 - 1, i * 8)))
  val old_vd_bytes = VecInit(Seq.tabulate(VLENB)(i => old_vd((i + 1) * 8 - 1, i * 8)))
  val vs2_hi_bytes = VecInit(Seq.tabulate(VLENB)(i => vs2_hi((i + 1) * 8 - 1, i * 8)))
  val vs2_lo_bytes = VecInit(Seq.tabulate(VLENB)(i => vs2_lo((i + 1) * 8 - 1, i * 8)))

  val eew = SewOH(vsew)
  val vlRemainBytes = Mux((vl << vsew) >= Cat(vd_idx, 0.U((log2Up(VLEN) - 3).W)), (vl << vsew) - Cat(vd_idx, 0.U((log2Up(VLEN) - 3).W)), 0.U)

  val mask_uop = MaskExtract(vmask, vd_idx, eew, VLEN)

  for (i <- 0 until VLENB) {
    when(i.U < vlRemainBytes) {
      vmask_byte_strb(i) := mask_uop(i) | vm
      when(vsew === 1.U(3.W)) {
        vmask_byte_strb(i) := mask_uop(i / 2) | vm
      }.elsewhen(vsew === 2.U(3.W)) {
        vmask_byte_strb(i) := mask_uop(i / 4) | vm
      }.elsewhen(vsew === 3.U(3.W)) {
        vmask_byte_strb(i) := mask_uop(i / 8) | vm
      }
    }.otherwise {
      vmask_byte_strb(i) := 0.U
    }
  }

  for (i <- 0 until VLENB) {
    vslideup_vd(i) := 0.U
  }

  // vslide
  val vslide_offset = vslide_bytes(log2Up(VLEN) - 4, 0)
  // slideoffset, unchange, old_vd
  when(!vslide_hi_valid && !vslide_lo_valid) {
    for (i <- 0 until VLENB) {
      vslideup_vd(i) := old_vd_bytes(i)
    }
  }.elsewhen(vslide_hi_valid && !vslide_lo_valid) { // first old_vd & vs2
    for (i <- 0 until VLENB) {
      when(i.U < vslide_offset) { //old_vd
        vslideup_vd(i) := old_vd_bytes(i)
      }.otherwise { // vs2
        when(vmask_byte_strb(i).asBool) {
          vslideup_vd(i) := vs2_hi_bytes(i.U - vslide_offset)
        }.otherwise {
          vslideup_vd(i) := Mux(ma, "hff".U, old_vd_bytes(i))
        }
      }
    }
  }.elsewhen(vslide_hi_valid && vslide_lo_valid) { // vs2(i) & vs2(i-1)
    for (i <- 0 until VLENB) {
      when(vmask_byte_strb(i).asBool) {
        when(i.U < vslide_offset) { // vs2(i-1)
          vslideup_vd(i) := vs2_lo_bytes(VLENB.U - vslide_offset +& i.U)
        }.otherwise { // vs2(i)
          vslideup_vd(i) := vs2_hi_bytes(i.U - vslide_offset)
        }
      }.otherwise { // MA
        vslideup_vd(i) := Mux(ma, "hff".U, old_vd_bytes(i))
      }
    }
  }

  for (i <- 0 until VLENB) {
    when(vmask_byte_strb(i).asBool) {
      when((i.U < (VLENB.U - vslide_offset)) && vslide_lo_valid && ((i.U + vslide_offset) < vlmax_bytes)) {
        vslidedn_vd(i) := vs2_lo_bytes(i.U +& vslide_offset)
      }.elsewhen((i.U >= (VLENB.U - vslide_offset)) && vslide_hi_valid) {
        vslidedn_vd(i) := vs2_hi_bytes(i.U +& vslide_offset - VLENB.U)
      }.otherwise(
        vslidedn_vd(i) := 0.U
      )
    }.otherwise {
      vslidedn_vd(i) := Mux(ma, "hff".U, old_vd_bytes(i))
    }
  }

  vslide1up_vd := vslideup_vd
  when(vd_idx === 0.U) {
    for (i <- 0 until VLENB) {
      when(i.U < vsew_bytes) {
        when(vmask_byte_strb(i).asBool) {
          vslide1up_vd(i) := rs1_bytes(i)
        }.otherwise {
          vslide1up_vd(i) := Mux(ma, "hff".U, old_vd_bytes(i))
        }
      }
    }
  }

  vslide1dn_vd := vslidedn_vd
  when(vd_idx === vl_idx) {
    for (i <- 0 until VLENB) {
      when((i.U >= (vlRemainBytes - vsew_bytes)) && vmask_byte_strb(i).asBool) {
        vslide1dn_vd(i) := rs1_bytes(i.U +& vsew_bytes - vlRemainBytes)
      }
    }
  }

  io.vslideup_vd := Cat(vslideup_vd.reverse)
  io.vslidedn_vd := Cat(vslidedn_vd.reverse)
  io.vslide1up_vd := Cat(vslide1up_vd.reverse)
  io.vslide1dn_vd := Cat(vslide1dn_vd.reverse)
}

// object MaskExtract {
//   def VLEN = 256
//
//   def apply(vmask: UInt, uopIdx: UInt, sew: SewOH) = {
//     val extracted = Wire(UInt((VLEN / 8).W))
//     extracted := Mux1H(Seq.tabulate(8)(uopIdx === _.U),
//       Seq.tabulate(8)(idx => Mux1H(sew.oneHot, Seq(VLEN / 8, VLEN / 16, VLEN / 32, VLEN / 64).map(stride =>
//         vmask((idx + 1) * stride - 1, idx * stride)))))
//     extracted
//   }
// }

//  object MaskReorg {
//    // sew = 8: unchanged, sew = 16: 00000000abcdefgh -> aabbccddeeffgghh, ...
//
//    def vlenb = 256 / 8
//
//    def splash(bits: UInt, sew: SewOH): UInt = {
//      Mux1H(sew.oneHot, Seq(1, 2, 4, 8).map(k => Cat(bits(vlenb / k - 1, 0).asBools.map(Fill(k, _)).reverse)))
//    }
//    // // sew = 8: unchanged, sew = 16: 00000000abcdefgh -> 0000abcd0000efgh, ...
//    // def apply(bits: UInt, sew: SewOH): UInt = {
//    //   Mux1H(sew.oneHot, Seq(1,2,4,8).map(k => Cat(UIntSplit(bits(16/k -1, 0), 2).map(_ | 0.U(8.W)).reverse)))
//    // }
//  }

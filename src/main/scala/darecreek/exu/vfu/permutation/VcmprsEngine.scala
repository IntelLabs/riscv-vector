package darecreek.exu.vfu.perm

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
// import darecreek.exu.vfu.VFUParam._
import org.chipsalliance.cde.config.Parameters

class VcmprsEngine(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val funct6 = Input(UInt(6.W))
    val funct3 = Input(UInt(3.W))
    val vm = Input(Bool())
    val ma = Input(Bool())
    val ta = Input(Bool())
    val vsew = Input(UInt(3.W))
    val vlmul = Input(UInt(3.W))
    val vl = Input(UInt(8.W))
    val vs_idx = Input(UInt(3.W))
    val vd_idx = Input(UInt(3.W))
    val vs2 = Input(UInt(128.W))
    val old_vd = Input(UInt(128.W))
    val vmask = Input(UInt(128.W))
    val vd_reg = Input(UInt(128.W))
    val update_vs_idx = Input(Bool())
    val cmprs_rd_wb = Input(Bool())
    val cmprs_rd_old_vd = Input(Bool())
    val calc_done = Input(Bool())
    val flush = Input(Bool())

    val cmprs_vd = Output(UInt(128.W))
  })

  val funct6 = io.funct6
  val funct3 = io.funct3
  val vm = io.vm
  val ma = io.ma
  val ta = io.ta
  val vsew = io.vsew
  val vlmul = io.vlmul
  val vl = io.vl
  val vs_idx = io.vs_idx
  val vd_idx = io.vd_idx
  val vs2 = io.vs2
  val old_vd = io.old_vd
  val vmask = io.vmask
  val vd_reg = io.vd_reg
  val update_vs_idx = io.update_vs_idx
  val cmprs_rd_wb = io.cmprs_rd_wb
  val cmprs_rd_old_vd = io.cmprs_rd_old_vd
  val calc_done = io.calc_done
  val flush = io.flush

  val vcompress = (funct6 === "b010111".U) && (funct3 === "b010".U)
  val eew = SewOH(vsew)

  val cmprs_read = update_vs_idx || cmprs_rd_wb || cmprs_rd_old_vd
  val old_vd_bytes = VecInit(Seq.tabulate(VLENB)(i => old_vd((i + 1) * 8 - 1, i * 8)))
  val vs2_bytes = VecInit(Seq.tabulate(VLENB)(i => vs2((i + 1) * 8 - 1, i * 8)))

  val base = Wire(UInt(7.W))
  val vd_mask = (~0.U(VLEN.W))
  val vd_mask_vl = Wire(UInt(VLEN.W))
  val vmask_vl = Wire(UInt(VLEN.W))
  val vmask_uop = MaskExtract(vmask_vl, vs_idx, eew)
  val vmask_16b = Mux(cmprs_rd_old_vd, 0.U, MaskReorg.splash(vmask_uop, eew))
  val current_vs_ones_sum = Wire(UInt(5.W))
  val current_uop_ones_sum = Wire(Vec(VLENB, UInt(5.W)))
  val current_ones_sum = Wire(Vec(VLENB, UInt(8.W)))
  val ones_sum = RegInit(0.U(8.W))
  val cmprs_vd = Wire(Vec(VLENB, UInt(8.W)))
  val res_idx = Wire(Vec(VLENB, UInt(8.W)))
  val res_valid = Wire(Vec(VLENB, Bool()))

  vd_mask_vl := vd_mask >> (VLEN.U - vl)
  vmask_vl := vmask & vd_mask_vl
  current_vs_ones_sum := PopCount(vmask_16b)

  when(flush) {
    ones_sum := 0.U
  }.elsewhen(cmprs_rd_old_vd || calc_done) {
    ones_sum := 0.U
  }.elsewhen(update_vs_idx) {
    ones_sum := ones_sum + current_vs_ones_sum
  }

  base := Cat(vd_idx, 0.U(4.W))

  for (i <- 0 until VLENB) {
    current_uop_ones_sum(i) := 0.U
    current_ones_sum(i) := ones_sum
    when(update_vs_idx || cmprs_rd_wb) {
      current_uop_ones_sum(i) := PopCount(vmask_16b(i, 0))
      current_ones_sum(i) := ones_sum + current_uop_ones_sum(i)
    }
  }

  for (i <- 0 until VLENB) {
    cmprs_vd(i) := vd_reg(i * 8 + 7, i * 8)
    res_idx(i) := 0.U
    res_valid(i) := false.B
  }

  for (i <- 0 until VLENB) {
    when(cmprs_read) {
      when(cmprs_rd_old_vd && (i.U >= ones_sum(3, 0))) {
        cmprs_vd(i) := Mux(ta, "hff".U, old_vd_bytes(i))
      }.otherwise {
        res_idx(i) := current_ones_sum(i) - base - 1.U
        res_valid(i) := current_ones_sum(i) >= base + 1.U
        when(vmask_16b(i) && res_valid(i) && (res_idx(i) < VLENB.U)) {
          cmprs_vd(res_idx(i)) := vs2_bytes(i)
        }
      }
    }
  }

  io.cmprs_vd := Cat(cmprs_vd.reverse)
}

// object VerilogVcmprs extends App {
//   println("Generating the VPU Vcompress hardware")
//   emitVerilog(new VcmprsEngine(), Array("--target-dir", "build/vifu"))

// }



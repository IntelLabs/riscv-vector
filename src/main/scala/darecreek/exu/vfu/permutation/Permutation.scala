package darecreek.exu.vfu.perm

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
// import darecreek.exu.vfu.VFUParam._
import chipsalliance.rocketchip.config._
import xiangshan.Redirect
import darecreek.exu.vfu.fp.VFPU

class Permutation(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Input(new VPermInput)
    val redirect = Input(ValidIO(new Redirect))
    val out = Output(new VPermOutput)
  })

  val ctrl = io.in.uop.ctrl
  val funct6 = io.in.uop.ctrl.funct6
  val funct3 = io.in.uop.ctrl.funct3
  val vm = io.in.uop.ctrl.vm
  val vs1_imm = io.in.uop.ctrl.vs1_imm
  val vsew = io.in.uop.info.vsew
  val vs1_preg_idx = io.in.vs1_preg_idx
  val vs2_preg_idx = io.in.vs2_preg_idx
  val old_vd_preg_idx = io.in.old_vd_preg_idx
  val mask_preg_idx = io.in.mask_preg_idx
  val ta = io.in.uop.info.ta
  val ma = io.in.uop.info.ma
  val vstart = io.in.uop.info.vstart
  val vl = io.in.uop.info.vl
  val vlmul = io.in.uop.info.vlmul
  val uop_valid = io.in.uop_valid && !io.out.perm_busy
  val rdata = io.in.rdata
  val rvalid = io.in.rvalid

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
  val vrgather_vv = (funct6 === "b001100".U) && (funct3 === "b000".U)
  val vrgather16 = (funct6 === "b001110".U) && (funct3 === "b000".U)
  val vrgather_vx = (funct6 === "b001100".U) && (funct3 === "b100".U)
  val vrgather_vi = (funct6 === "b001100".U) && (funct3 === "b011".U)
  val vrgather_vxi = vrgather_vx || vrgather_vi
  val vcompress = (funct6 === "b010111".U) && (funct3 === "b010".U)
  val vrgather16_sew8 = vrgather16 && (vsew === 0.U)
  val vrgather16_sew32 = vrgather16 && (vsew === 2.U)
  val vrgather16_sew64 = vrgather16 && (vsew === 3.U)
  val vslide = vslideup || vslidedn || vslide1up || vslide1dn
  val vrgather = vrgather_vv || vrgather_vxi || vrgather16

  val perm_fp = vfslide1up_vf || vfslide1dn_vf
  val ftype = VFPU.getTypeTagFromVSEW(vsew)
  val rs1_fp = VFPU.unbox(io.in.rs1, ftype)
  val rs1 = Mux(perm_fp, rs1_fp, io.in.rs1)
  val rs1_imm = Mux(ctrl.vi, Cat(0.U(59.W), vs1_imm), rs1)

  val funct6_reg = RegInit(0.U(6.W))
  val funct3_reg = RegInit(0.U(3.W))
  val vsew_reg = RegInit(0.U(3.W))
  val mask = RegInit(0.U(128.W))
  val mask_valid = RegInit(false.B)
  val old_vd = RegInit(0.U(128.W))
  val vs_reg = RegInit(0.U(128.W))
  val rs1_reg = RegInit(0.U(64.W))
  val vs1_preg_idx_reg = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val vs2_preg_idx_reg = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val old_vd_preg_idx_reg = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val mask_preg_idx_reg = RegInit(0.U(8.W))
  val vm_reg = RegInit(false.B)
  val ta_reg = RegInit(false.B)
  val ma_reg = RegInit(false.B)
  val vstart_reg = RegInit(0.U(7.W))
  val vl_reg = RegInit(0.U(8.W))
  val rd_vlmul = RegInit(0.U(3.W))
  val vlmul_reg = RegInit(0.U(3.W))

  val vl_reg_bytes = vl_reg << vsew_reg
  val perm_busy = RegInit(false.B)
  val flush = RegInit(false.B)
  val in_robIdx = io.in.uop.sysUop.robIdx
  val currentRobIdx = RegEnable(in_robIdx, uop_valid)
  when(uop_valid) {
    flush := in_robIdx.needFlush(io.redirect)
  }.otherwise {
    flush := (flush || currentRobIdx.needFlush(io.redirect)) && perm_busy
  }

  val reg_vslideup_vx = (funct6_reg === "b001110".U) && (funct3_reg === "b100".U) && !flush
  val reg_vslideup_vi = (funct6_reg === "b001110".U) && (funct3_reg === "b011".U) && !flush
  val reg_vslideup = reg_vslideup_vx || reg_vslideup_vi && !flush
  val reg_vslidedn_vx = (funct6_reg === "b001111".U) && (funct3_reg === "b100".U) && !flush
  val reg_vslidedn_vi = (funct6_reg === "b001111".U) && (funct3_reg === "b011".U) && !flush
  val reg_vslidedn = reg_vslidedn_vx || reg_vslidedn_vi && !flush
  val reg_vslide1up_vx = (funct6_reg === "b001110".U) && (funct3_reg === "b110".U) && !flush
  val reg_vfslide1up_vf = (funct6_reg === "b001110".U) && (funct3_reg === "b101".U) && !flush
  val reg_vslide1up = reg_vslide1up_vx || reg_vfslide1up_vf && !flush
  val reg_vslide1dn_vx = (funct6_reg === "b001111".U) && (funct3_reg === "b110".U) && !flush
  val reg_vfslide1dn_vf = (funct6_reg === "b001111".U) && (funct3_reg === "b101".U) && !flush
  val reg_vslide1dn = reg_vslide1dn_vx || reg_vfslide1dn_vf && !flush
  val reg_vrgather_vv = (funct6_reg === "b001100".U) && (funct3_reg === "b000".U) && !flush
  val reg_vrgather16 = (funct6_reg === "b001110".U) && (funct3_reg === "b000".U) && !flush
  val reg_vrgather_vx = (funct6_reg === "b001100".U) && (funct3_reg === "b100".U) && !flush
  val reg_vrgather_vi = (funct6_reg === "b001100".U) && (funct3_reg === "b011".U) && !flush
  val reg_vrgather_vxi = reg_vrgather_vx || reg_vrgather_vi && !flush
  val reg_vcompress = (funct6_reg === "b010111".U) && (funct3_reg === "b010".U) && !flush
  val reg_vrgather16_sew8 = reg_vrgather16 && (vsew_reg === 0.U) && !flush
  val reg_vrgather16_sew32 = reg_vrgather16 && (vsew_reg === 2.U) && !flush
  val reg_vrgather16_sew64 = reg_vrgather16 && (vsew_reg === 3.U) && !flush
  val reg_vslide = reg_vslideup || reg_vslidedn || reg_vslide1up || reg_vslide1dn && !flush
  val reg_vrgather = reg_vrgather_vv || reg_vrgather_vxi || reg_vrgather16 && !flush
  val vs1_type = Mux(reg_vrgather16, 1.U, vsew_reg)

  val vslide_wb_vld = Wire(Bool())
  val cmprs_wb_vld = Wire(Bool())
  val wb_vld = Wire(Bool())
  val wb_idx = RegInit(0.U(3.W))

  val vlRemain = RegInit(0.U(8.W))
  val vlRemainBytes = vlRemain << vsew_reg
  val vd_mask = Wire(UInt(VLEN.W))
  vd_mask := (~0.U(VLEN.W))

  val vlRemain_reg = RegInit(0.U(8.W))
  when(flush) {
    vlRemain_reg := 0.U
  }.otherwise {
    vlRemain_reg := vlRemain
  }
  val vlRemainBytes_reg = vlRemain_reg << vsew_reg
  val tail_bytes = Mux((vlRemainBytes_reg >= VLENB.U), 0.U, VLENB.U - vlRemainBytes_reg)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = Wire(UInt(VLEN.W))
  vmask_tail_bits := vd_mask >> tail_bits
  val tail_old_vd = old_vd & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val tail_vd = Mux(ta_reg, tail_ones_vd, tail_old_vd)

  val vd_reg = RegInit(0.U(VLEN.W))

  val eew = SewOH(vsew_reg)

  val vsew_bytes = Wire(UInt(3.W))
  val vsew_shift = Wire(UInt(3.W))
  val vslideup_vd = Wire(UInt(128.W))
  val vslidedn_vd = Wire(UInt(128.W))
  val vslide1up_vd = Wire(UInt(128.W))
  val vslide1dn_vd = Wire(UInt(128.W))
  val cmprs_vd = Wire(UInt(128.W))
  val vrgather_vd = Wire(UInt(128.W))
  val perm_vd = Wire(UInt(VLEN.W))
  val perm_tail_mask_vd = Wire(UInt(VLEN.W))

  val vs_idx = RegInit(0.U(3.W))
  val rdata_vs_idx = RegInit(0.U(3.W))
  val rdata_wb_idx = RegInit(0.U(3.W))
  val vd_idx = RegInit(0.U(3.W))
  val update_vs_idx = Wire(Bool())
  val vslide_update_vs_idx = Wire(Bool())
  val cmprs_update_vs_idx = Wire(Bool())
  val vslide_ele = Mux(reg_vslide1up || reg_vslide1dn, 1.U, rs1_reg)
  val vslide_bytes = vslide_ele << vsew_reg

  val vslide_lo_valid = Mux(reg_vslideup || reg_vslide1up, vslide_bytes(70, 4) +& 1.U <= vs_idx, (reg_vslidedn || reg_vslide1dn) && (vs_idx +& vslide_bytes(70, 4) <= rd_vlmul))
  val vslide_hi_valid = Mux(reg_vslideup || reg_vslide1up, vslide_bytes(70, 4) <= vs_idx, (reg_vslidedn || reg_vslide1dn) && (vs_idx +& vslide_bytes(70, 4) +& 1.U <= rd_vlmul))
  val vslide_cnt_max = Wire(UInt(2.W))
  val vslide_rd_cnt = RegInit(0.U(2.W))
  val rd_mask_en = RegInit(false.B)
  val rd_vs_en = RegInit(false.B)
  val vslide_rd_preg_idx = Wire(UInt(8.W))
  val rdata_rd_mask_en = Wire(Bool())
  val rdata_wb_vld = Wire(Bool())
  val rdata_update_vs_idx = Wire(Bool())
  val rdata_cmprs_rd_wb = Wire(Bool())
  val rdata_cmprs_rd_old_vd = Wire(Bool())
  val rdata_vslide_lo_valid = Wire(Bool())
  val rdata_vslide_hi_valid = Wire(Bool())
  val rdata_vslide_rd_cnt = Wire(UInt(2.W))
  val rdata_vrgather_rd_cnt = Wire(UInt(4.W))

  val rec_done = uop_valid
  val rd_done = (wb_idx === rd_vlmul) && wb_vld
  val calc_done = RegInit(false.B)

  when(flush) {
    funct6_reg := 0.U
    funct3_reg := 0.U
    vsew_reg := 0.U
    rs1_reg := 0.U
    vs1_preg_idx_reg := VecInit(Seq.fill(8)(0.U(8.W)))
    vs2_preg_idx_reg := VecInit(Seq.fill(8)(0.U(8.W)))
    old_vd_preg_idx_reg := VecInit(Seq.fill(8)(0.U(8.W)))
    mask_preg_idx_reg := 0.U
    vm_reg := false.B
    ta_reg := false.B
    ma_reg := false.B
    vstart_reg := 0.U
    vl_reg := 0.U
    vlmul_reg := 0.U
    rd_vlmul := 0.U
  }.elsewhen(uop_valid) {
    funct6_reg := funct6
    funct3_reg := funct3
    vsew_reg := vsew
    rs1_reg := rs1_imm
    vs1_preg_idx_reg := vs1_preg_idx
    vs2_preg_idx_reg := vs2_preg_idx
    old_vd_preg_idx_reg := old_vd_preg_idx
    mask_preg_idx_reg := mask_preg_idx
    vm_reg := vm
    ta_reg := ta
    ma_reg := ma
    vstart_reg := vstart
    vl_reg := vl
    vlmul_reg := vlmul
    rd_vlmul := (1.U << Mux(vlmul(2), 0.U, Cat(0.U(1.W), vlmul(1, 0)))) - 1.U
  }

  // vslide read
  vslide_cnt_max := Cat(0.U(1.W), vslide_lo_valid) + Cat(0.U(1.W), vslide_hi_valid)
  when(flush) {
    vslide_rd_cnt := 0.U
  }.elsewhen(rd_vs_en && reg_vslide) {
    when(vslide_rd_cnt === vslide_cnt_max) {
      vslide_rd_cnt := 0.U
    }.otherwise {
      vslide_rd_cnt := vslide_rd_cnt + 1.U
    }
  }

  vslide_update_vs_idx := reg_vslide && (vslide_rd_cnt === vslide_cnt_max) && rd_vs_en
  vslide_wb_vld := vslide_update_vs_idx

  val rd_idx_lo = Wire(UInt(3.W))
  val rd_idx_hi = Wire(UInt(3.W))

  rd_idx_lo := 0.U
  rd_idx_hi := 0.U
  when(reg_vslideup || reg_vslide1up) {
    rd_idx_lo := vs_idx - vslide_bytes(6, 4) - 1.U
    rd_idx_hi := vs_idx - vslide_bytes(6, 4)
  }.elsewhen(reg_vslidedn || reg_vslide1dn) {
    rd_idx_lo := vs_idx + vslide_bytes(6, 4)
    rd_idx_hi := vs_idx + vslide_bytes(6, 4) + 1.U
  }

  vslide_rd_preg_idx := 0.U
  when(rd_mask_en) {
    vslide_rd_preg_idx := mask_preg_idx_reg
  }.elsewhen(reg_vslide && rd_vs_en) {
    when(vslide_rd_cnt === 0.U) {
      vslide_rd_preg_idx := old_vd_preg_idx_reg(vs_idx)
    }.elsewhen((vslide_rd_cnt === 1.U) && vslide_lo_valid) {
      vslide_rd_preg_idx := vs2_preg_idx_reg(rd_idx_lo)
    }.elsewhen((vslide_rd_cnt === 1.U) && vslide_hi_valid && !vslide_lo_valid) {
      vslide_rd_preg_idx := vs2_preg_idx_reg(rd_idx_hi)
    }.elsewhen(vslide_rd_cnt === 2.U) {
      vslide_rd_preg_idx := vs2_preg_idx_reg(rd_idx_hi)
    }
  }

  // vcompress read
  val vd_mask_vl = Wire(UInt(VLEN.W))
  val vmask_vl = RegInit(0.U(VLEN.W))
  val vmask_uop = MaskExtract(vmask_vl, vs_idx, eew)
  val vmask_16b = Mux(rd_vs_en, MaskReorg.splash(vmask_uop, eew), 0.U)
  val current_rd_vs_ones_sum = Wire(UInt(5.W))
  val rd_ones_sum = RegInit(0.U(8.W))
  val ones_sum = Wire(UInt(8.W))
  val cmprs_rd_wb = Wire(Bool())
  val cmprs_rd_resent_en = Wire(Bool())
  val cmprs_rd_old_vd = RegInit(false.B)
  val cmprs_rd_old_vd_idx = RegInit(0.U(3.W))
  val wb_idx_plus1 = Wire(UInt(4.W))
  val cmprs_rd_preg_idx = Wire(UInt(8.W))
  vd_mask_vl := vd_mask >> (VLEN.U - vl_reg)
  wb_idx_plus1 := Cat(0.U(1.W), wb_idx) + 1.U
  current_rd_vs_ones_sum := PopCount(vmask_16b)

  when(flush) {
    vmask_vl := 0.U
  }.otherwise {
    vmask_vl := mask & vd_mask_vl
  }

  cmprs_rd_wb := reg_vcompress && ((rd_ones_sum + current_rd_vs_ones_sum) >= Cat(wb_idx_plus1, 0.U(4.W)))
  cmprs_rd_resent_en := reg_vcompress && ((rd_ones_sum + current_rd_vs_ones_sum) > Cat(wb_idx_plus1, 0.U(4.W)))

  when(rd_done || flush || cmprs_rd_old_vd) {
    rd_ones_sum := 0.U
  }.elsewhen(cmprs_update_vs_idx) {
    rd_ones_sum := rd_ones_sum + current_rd_vs_ones_sum
  }

  when(flush || rd_done) {
    cmprs_rd_old_vd := false.B
  }.elsewhen(reg_vcompress && (update_vs_idx && (vs_idx === rd_vlmul)) && !rd_done) {
    cmprs_rd_old_vd := true.B
  }

  ones_sum := rd_ones_sum + current_rd_vs_ones_sum
  cmprs_rd_old_vd_idx := ones_sum(7, 4)
  when(flush) {
    cmprs_rd_old_vd_idx := 0.U
  }.elsewhen(cmprs_rd_old_vd) {
    when(cmprs_rd_old_vd_idx === rd_vlmul) {
      cmprs_rd_old_vd_idx := 0.U
    }.otherwise {
      cmprs_rd_old_vd_idx := cmprs_rd_old_vd_idx + 1.U
    }
  }

  cmprs_rd_preg_idx := 0.U
  when(rd_mask_en) {
    cmprs_rd_preg_idx := vs1_preg_idx_reg(0)
  }.elsewhen(reg_vcompress && rd_vs_en) {
    cmprs_rd_preg_idx := vs2_preg_idx_reg(vs_idx)
  }.elsewhen(cmprs_rd_old_vd) {
    cmprs_rd_preg_idx := old_vd_preg_idx_reg(cmprs_rd_old_vd_idx)
  }

  cmprs_update_vs_idx := reg_vcompress && rd_vs_en & !cmprs_rd_resent_en
  cmprs_wb_vld := cmprs_rd_wb || cmprs_rd_old_vd

  // vrgather read
  val vrgather_cnt_max = Wire(UInt(4.W))
  val vrgather_rd_cnt = RegInit(0.U(4.W))
  val vrgather_update_vs_idx = Wire(Bool())
  val vrgather_rd_preg_idx = Wire(UInt(8.W))
  val vrgather_wb_vld = Mux(reg_vrgather16_sew8 && (vlmul_reg < 4.U), vrgather_update_vs_idx && vs_idx(0), vrgather_update_vs_idx)

  vrgather_update_vs_idx := reg_vrgather && (vrgather_rd_cnt === vrgather_cnt_max) && rd_vs_en

  vrgather_cnt_max := 2.U + Cat(0.U(1.W), rd_vlmul)

  when(flush) {
    vrgather_rd_cnt := 0.U
  }.elsewhen(rd_vs_en && reg_vrgather) {
    when(vrgather_rd_cnt === vrgather_cnt_max) {
      vrgather_rd_cnt := 0.U
    }.otherwise {
      vrgather_rd_cnt := vrgather_rd_cnt + 1.U
    }
  }

  vrgather_rd_preg_idx := 0.U
  when(rd_mask_en) {
    vrgather_rd_preg_idx := mask_preg_idx_reg
  }.elsewhen(reg_vrgather && rd_vs_en) {
    when(vrgather_rd_cnt === 0.U) {
      vrgather_rd_preg_idx := old_vd_preg_idx_reg(vs_idx)
    }.elsewhen(vrgather_rd_cnt === 1.U) {
      when(reg_vrgather_vxi) {
        vrgather_rd_preg_idx := vs2_preg_idx_reg(vs_idx)
      }.otherwise {
        vrgather_rd_preg_idx := vs1_preg_idx_reg(vs_idx)
      }
    }.otherwise {
      vrgather_rd_preg_idx := vs2_preg_idx_reg(vrgather_rd_cnt - 2.U)
    }
  }

  val update_vd_idx = RegInit(false.B)
  when(flush) {
    update_vd_idx := false.B
  }.otherwise {
    update_vd_idx := rdata_wb_vld
  }
  update_vs_idx := vslide_update_vs_idx || cmprs_update_vs_idx || vrgather_update_vs_idx
  wb_vld := vslide_wb_vld || cmprs_wb_vld || vrgather_wb_vld
  val rd_preg_idx = Mux(reg_vcompress, cmprs_rd_preg_idx, Mux(reg_vslide, vslide_rd_preg_idx, vrgather_rd_preg_idx))

  rd_mask_en := false.B
  when(flush) {
    rd_mask_en := false.B
  }.elsewhen(uop_valid) {
    rd_mask_en := true.B
  }

  val vrgather16_sew8_rd_vlmul = Wire(UInt(3.W))
  vrgather16_sew8_rd_vlmul := Mux(reg_vrgather16_sew8 && (vlmul_reg < 4.U), Cat(Cat(0.U(1.W), rd_vlmul) + 1.U, 0.U(1.W)) - 1.U, rd_vlmul)
  when(flush) {
    rd_vs_en := false.B
  }.elsewhen(!reg_vcompress && rd_mask_en) {
    rd_vs_en := true.B
  }.elsewhen(reg_vcompress && mask_valid) {
    rd_vs_en := true.B
  }.elsewhen(update_vs_idx && (vs_idx === vrgather16_sew8_rd_vlmul)) {
    rd_vs_en := false.B
  }

  when(flush) {
    vs_idx := 0.U
  }.elsewhen(update_vs_idx) {
    when(vs_idx === vrgather16_sew8_rd_vlmul) {
      vs_idx := 0.U
    }.otherwise {
      vs_idx := vs_idx + 1.U
    }
  }

  when(flush) {
    wb_idx := 0.U
  }.elsewhen(wb_vld) {
    when(wb_idx === rd_vlmul) {
      wb_idx := 0.U
    }.otherwise {
      wb_idx := wb_idx + 1.U
    }
  }

  when(flush) {
    rdata_vs_idx := 0.U
  }.elsewhen(rdata_update_vs_idx) {
    when(rdata_vs_idx === vrgather16_sew8_rd_vlmul) {
      rdata_vs_idx := 0.U
    }.otherwise {
      rdata_vs_idx := rdata_vs_idx + 1.U
    }
  }

  when(flush) {
    rdata_wb_idx := 0.U
  }.elsewhen(rdata_wb_vld) {
    when(rdata_wb_idx === rd_vlmul) {
      rdata_wb_idx := 0.U
    }.otherwise {
      rdata_wb_idx := rdata_wb_idx + 1.U
    }
  }

  when(flush) {
    vd_idx := 0.U
  }.elsewhen(update_vd_idx) {
    when(vd_idx === rd_vlmul) {
      vd_idx := 0.U
    }.otherwise {
      vd_idx := vd_idx + 1.U
    }
  }

  val rdata_reg = RegInit(0.U(128.W))
  val rvalid_reg = RegInit(false.B)

  when(flush) {
    rdata_reg := 0.U
  }.elsewhen(rvalid) {
    rdata_reg := rdata
  }

  when(flush) {
    rvalid_reg := false.B
  }.otherwise {
    rvalid_reg := rvalid
  }

  val vperm_fifo = Module(new perm_RegFifo(UInt(8.W), 8))
  val vslide_fifo_wdata = Wire(UInt(8.W))
  val cmprs_fifo_wdata = Wire(UInt(8.W))
  val vrgather_fifo_wdata = Wire(UInt(8.W))

  vslide_fifo_wdata := Cat(0.U(2.W), vslide_rd_cnt, vslide_hi_valid, vslide_lo_valid, wb_vld, rd_mask_en)
  cmprs_fifo_wdata := Cat(0.U(3.W), cmprs_rd_old_vd, cmprs_rd_wb, update_vs_idx, wb_vld, rd_mask_en)
  vrgather_fifo_wdata := Cat(0.U(1.W), vrgather_rd_cnt, update_vs_idx, wb_vld, rd_mask_en)

  vperm_fifo.io.rst := flush
  vperm_fifo.io.enq.bits := Mux(reg_vcompress, cmprs_fifo_wdata, Mux(reg_vslide, vslide_fifo_wdata, vrgather_fifo_wdata))
  vperm_fifo.io.enq.valid := rd_mask_en || rd_vs_en || cmprs_rd_old_vd
  vperm_fifo.io.deq.ready := rvalid_reg && vperm_fifo.io.deq.valid
  rdata_rd_mask_en := rvalid_reg && vperm_fifo.io.deq.bits(0) && vperm_fifo.io.deq.valid
  rdata_wb_vld := rvalid_reg && vperm_fifo.io.deq.bits(1) && vperm_fifo.io.deq.valid
  rdata_vslide_lo_valid := rvalid_reg && vperm_fifo.io.deq.bits(2) && vperm_fifo.io.deq.valid && reg_vslide
  rdata_vslide_hi_valid := rvalid_reg && vperm_fifo.io.deq.bits(3) && vperm_fifo.io.deq.valid && reg_vslide
  rdata_vslide_rd_cnt := Mux(rvalid_reg && reg_vslide, vperm_fifo.io.deq.bits(5, 4), 0.U) & Fill(2, vperm_fifo.io.deq.valid)
  rdata_update_vs_idx := rvalid_reg && vperm_fifo.io.deq.bits(2) && vperm_fifo.io.deq.valid && (reg_vcompress || reg_vrgather)
  rdata_cmprs_rd_wb := rvalid_reg && vperm_fifo.io.deq.bits(3) && vperm_fifo.io.deq.valid && reg_vcompress
  rdata_cmprs_rd_old_vd := rvalid_reg && vperm_fifo.io.deq.bits(4) && vperm_fifo.io.deq.valid && reg_vcompress
  rdata_vrgather_rd_cnt := Mux(rvalid_reg && reg_vrgather, vperm_fifo.io.deq.bits(6, 3), 0.U) & Fill(4, vperm_fifo.io.deq.valid)

  when(flush) {
    mask := 0.U
    mask_valid := false.B
  }.elsewhen(rdata_rd_mask_en && rvalid_reg) {
    mask := rdata_reg
    mask_valid := true.B
  }.otherwise {
    mask_valid := false.B
  }

  when(flush) {
    old_vd := 0.U
  }.elsewhen(reg_vslide && !rdata_rd_mask_en && rvalid_reg && (rdata_vslide_rd_cnt === 0.U)) {
    old_vd := rdata_reg
  }.elsewhen(reg_vrgather && !rdata_rd_mask_en && rvalid_reg && (rdata_vrgather_rd_cnt === 0.U)) {
    old_vd := rdata_reg
  }

  when(flush) {
    vs_reg := 0.U
  }.elsewhen(reg_vslide && rvalid_reg && (rdata_vslide_rd_cnt === 1.U)) {
    vs_reg := rdata_reg
  }.elsewhen(reg_vrgather && rvalid_reg && (rdata_vrgather_rd_cnt === 1.U)) {
    vs_reg := rdata_reg
  }

  val rs1_bytes = VecInit(Seq.tabulate(8)(i => rs1_reg((i + 1) * 8 - 1, i * 8)))
  val rdata_bytes = VecInit(Seq.tabulate(VLENB)(i => rdata_reg((i + 1) * 8 - 1, i * 8)))
  val old_vd_bytes = VecInit(Seq.tabulate(VLENB)(i => old_vd((i + 1) * 8 - 1, i * 8)))
  val vs_reg_bytes = VecInit(Seq.tabulate(VLENB)(i => vs_reg((i + 1) * 8 - 1, i * 8)))

  calc_done := false.B
  when(flush) {
    calc_done := false.B
  }.elsewhen((vd_idx === rd_vlmul) && update_vd_idx) {
    calc_done := true.B
  }

  when(flush) {
    perm_busy := false.B
  }.elsewhen(uop_valid) {
    perm_busy := true.B
  }.elsewhen(calc_done) {
    perm_busy := false.B
  }

  // vslide
  val vs2_lo = Wire(UInt(128.W))
  val vs2_hi = Wire(UInt(128.W))
  val vslide_old_vd = Wire(UInt(128.W))

  vs2_lo := 0.U
  vs2_hi := 0.U
  vslide_old_vd := 0.U

  when(reg_vslideup || reg_vslide1up) {
    when(!rdata_vslide_hi_valid && !rdata_vslide_lo_valid) {
      vslide_old_vd := rdata_reg
    }.elsewhen(rdata_vslide_hi_valid && !rdata_vslide_lo_valid) { // first old_vd & vs2
      vslide_old_vd := old_vd
      vs2_hi := rdata_reg
    }.elsewhen(rdata_vslide_hi_valid && rdata_vslide_lo_valid) { // vs2(i) & vs2(i-1)
      vslide_old_vd := old_vd
      vs2_lo := vs_reg
      vs2_hi := rdata_reg
    }
  }.elsewhen(reg_vslidedn || reg_vslide1dn) {
    when(rdata_vslide_hi_valid || rdata_vslide_lo_valid) {
      vslide_old_vd := old_vd
    }.otherwise {
      vslide_old_vd := rdata_reg
    }
    when(rdata_vslide_hi_valid && rdata_vslide_lo_valid) {
      vs2_lo := vs_reg
    }.otherwise {
      vs2_lo := rdata_reg
    }
    vs2_hi := rdata_reg
  }

  val vslideEngine = Module(new VslideEngine)
  vslideEngine.io.funct6 := funct6_reg
  vslideEngine.io.funct3 := funct3_reg
  vslideEngine.io.vm := vm_reg
  vslideEngine.io.ma := ma_reg
  vslideEngine.io.vsew := vsew_reg
  vslideEngine.io.vlmul := vlmul_reg
  vslideEngine.io.vl := vl_reg
  vslideEngine.io.vd_idx := rdata_wb_idx
  vslideEngine.io.rs1 := rs1_reg
  vslideEngine.io.vs2_hi := vs2_hi
  vslideEngine.io.vs2_lo := vs2_lo
  vslideEngine.io.old_vd := vslide_old_vd
  vslideEngine.io.vmask := mask
  vslideup_vd := vslideEngine.io.vslideup_vd
  vslidedn_vd := vslideEngine.io.vslidedn_vd
  vslide1up_vd := vslideEngine.io.vslide1up_vd
  vslide1dn_vd := vslideEngine.io.vslide1dn_vd

  val vcmprsEngine = Module(new VcmprsEngine)
  vcmprsEngine.io.funct6 := funct6_reg
  vcmprsEngine.io.funct3 := funct3_reg
  vcmprsEngine.io.vm := vm_reg
  vcmprsEngine.io.ma := ma_reg
  vcmprsEngine.io.ta := ta_reg
  vcmprsEngine.io.vsew := vsew_reg
  vcmprsEngine.io.vlmul := vlmul_reg
  vcmprsEngine.io.vl := vl_reg
  vcmprsEngine.io.vs_idx := rdata_vs_idx
  vcmprsEngine.io.vd_idx := rdata_wb_idx
  vcmprsEngine.io.vs2 := rdata_reg
  vcmprsEngine.io.old_vd := rdata_reg
  vcmprsEngine.io.vmask := vmask_vl
  vcmprsEngine.io.vd_reg := vd_reg
  vcmprsEngine.io.cmprs_rd_wb := rdata_cmprs_rd_wb
  vcmprsEngine.io.update_vs_idx := rdata_update_vs_idx
  vcmprsEngine.io.cmprs_rd_old_vd := rdata_cmprs_rd_old_vd
  vcmprsEngine.io.calc_done := calc_done
  vcmprsEngine.io.flush := flush
  cmprs_vd := vcmprsEngine.io.cmprs_vd


  val first = reg_vrgather && rvalid_reg && !rdata_rd_mask_en && (rdata_vrgather_rd_cnt === 2.U)
  val vrgather_update_vs2 = reg_vrgather && rvalid_reg && !rdata_rd_mask_en && (rdata_vrgather_rd_cnt >= 2.U) && (rdata_vrgather_rd_cnt < 10.U)
  val vrgather_vs2_cnt = Wire(UInt(3.W))
  vrgather_vs2_cnt := rdata_vrgather_rd_cnt - 2.U

  val vrgatherEngine = Module(new VrgatherEngine)
  vrgatherEngine.io.funct6 := funct6_reg
  vrgatherEngine.io.funct3 := funct3_reg
  vrgatherEngine.io.vm := vm_reg
  vrgatherEngine.io.ma := ma_reg
  vrgatherEngine.io.ta := ta_reg
  vrgatherEngine.io.vsew := vsew_reg
  vrgatherEngine.io.vlmul := vlmul_reg
  vrgatherEngine.io.vl := vl_reg
  vrgatherEngine.io.first := first
  vrgatherEngine.io.update_vs2 := vrgather_update_vs2
  vrgatherEngine.io.vs2_cnt := vrgather_vs2_cnt
  vrgatherEngine.io.vd_idx := rdata_vs_idx
  vrgatherEngine.io.rs1 := rs1_reg
  vrgatherEngine.io.vs1 := vs_reg
  vrgatherEngine.io.vs2 := rdata_reg
  vrgatherEngine.io.old_vd := old_vd
  vrgatherEngine.io.vmask := mask
  vrgatherEngine.io.vd_reg := vd_reg
  vrgather_vd := vrgatherEngine.io.vrgather_vd

  vsew_bytes := 1.U << vsew_reg
  vsew_shift := Cat(0.U(1.W), ~vsew_reg(1, 0)) + 1.U
  when(flush) {
    vlRemain := 0.U
  }.elsewhen(uop_valid) {
    vlRemain := Mux(vslideup && (rs1_imm > vl), Mux(rs1_imm > VLEN.U, VLEN.U, rs1_imm), vl)
  }.elsewhen(reg_vcompress && rdata_update_vs_idx) {
    vlRemain := Mux(vlRemain >= (1.U << vsew_shift), vlRemain - (1.U << vsew_shift), 0.U)
  }.elsewhen(!reg_vcompress && rdata_wb_vld) {
    vlRemain := Mux(vlRemain >= (1.U << vsew_shift), vlRemain - (1.U << vsew_shift), 0.U)
  }

  val rdata_update_vs_idx_reg = RegInit(false.B)
  val rdata_wb_vld_reg = RegInit(false.B)
  val rdata_wb_vld_reg2 = RegInit(false.B)

  when(flush) {
    rdata_update_vs_idx_reg := false.B
    rdata_wb_vld_reg := false.B
    rdata_wb_vld_reg2 := false.B
  }.otherwise {
    rdata_update_vs_idx_reg := rdata_update_vs_idx
    rdata_wb_vld_reg := rdata_wb_vld
    rdata_wb_vld_reg2 := rdata_wb_vld_reg
  }

  when(flush) {
    vd_reg := 0.U
  }.elsewhen(reg_vcompress && (rdata_update_vs_idx_reg || rdata_wb_vld_reg)) {
    vd_reg := cmprs_vd
  }.elsewhen(reg_vslideup && rdata_wb_vld) {
    vd_reg := vslideup_vd
  }.elsewhen(reg_vslidedn && rdata_wb_vld) {
    vd_reg := vslidedn_vd
  }.elsewhen(reg_vslide1up && rdata_wb_vld) {
    vd_reg := vslide1up_vd
  }.elsewhen(reg_vslide1dn && rdata_wb_vld) {
    vd_reg := vslide1dn_vd
  }.elsewhen(reg_vrgather && vrgather_update_vs2) {
    vd_reg := vrgather_vd
  }

  val vstartRemain = RegInit(0.U(7.W))
  val vstartRemainBytes = Wire(UInt(7.W))
  val vstart_bytes = Wire(UInt(5.W))
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = Wire(UInt(VLEN.W))
  vstartRemainBytes := vstartRemain << vsew_reg
  vstart_bytes := Mux(vstartRemainBytes >= VLENB.U, VLENB.U, vstartRemainBytes)
  vmask_vstart_bits := vd_mask << vstart_bits
  val vstart_old_vd = old_vd & (~vmask_vstart_bits)

  when(flush) {
    vstartRemain := 0.U
  }.elsewhen(uop_valid) {
    vstartRemain := Mux(vslideup && (rs1_imm > vstart), Mux(rs1_imm > VLEN.U, VLEN.U, rs1_imm), vstart)
  }.elsewhen(reg_vcompress && rdata_update_vs_idx) {
    vstartRemain := Mux(vstartRemain >= (1.U << vsew_shift), vstartRemain - (1.U << vsew_shift), 0.U)
  }.elsewhen(!reg_vcompress && update_vd_idx) {
    vstartRemain := Mux(vstartRemain >= (1.U << vsew_shift), vstartRemain - (1.U << vsew_shift), 0.U)
  }

  perm_tail_mask_vd := vd_reg
  when(vstart_reg >= vl_reg) {
    perm_tail_mask_vd := old_vd
  }.elsewhen(update_vd_idx) {
    perm_tail_mask_vd := (vd_reg & vmask_tail_bits & vmask_vstart_bits) | tail_vd | vstart_old_vd
  }

  perm_vd := Mux(reg_vcompress, vd_reg, perm_tail_mask_vd)

  val rd_en = rd_mask_en || rd_vs_en || cmprs_rd_old_vd
  val reg_rd_en = RegInit(false.B)
  val reg_rd_preg_idx = RegInit(0.U(8.W))

  when(flush) {
    reg_rd_en := false.B
  }.otherwise {
    reg_rd_en := rd_en
  }

  when(flush) {
    reg_rd_preg_idx := 0.U
  }.elsewhen(rd_en) {
    reg_rd_preg_idx := rd_preg_idx
  }


  val uop_reg = Reg(new VUop)

  when(uop_valid) {
    uop_reg := io.in.uop
  }

  io.out.uop := uop_reg
  io.out.rd_en := reg_rd_en & !flush
  io.out.rd_preg_idx := reg_rd_preg_idx
  io.out.wb_vld := Mux(reg_vcompress, rdata_wb_vld_reg2 & !flush, rdata_wb_vld_reg & !flush)
  io.out.wb_data := perm_vd
  io.out.perm_busy := perm_busy | flush
}

import xiangshan._

object Main extends App {
  println("Generating hardware")
  val p = Parameters.empty.alterPartial({ case XSCoreParamsKey => XSCoreParameters() })
  emitVerilog(new Permutation()(p.alterPartial({ case VFuParamsKey => VFuParameters() })), Array("--target-dir", "generated",
    "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}


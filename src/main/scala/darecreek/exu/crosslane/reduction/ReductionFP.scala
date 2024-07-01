package darecreek.exu.crosslane.reduction

import chisel3.{util, _}
import chisel3.util._
import chisel3.util.experimental.decode._
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.fp._
import darecreek.exu.vfucore.reduction._
import darecreek.exu.vfucore.{VFuModule, VFuParamsKey, VFuParameters}
import darecreek._
import darecreek.exu.vfucore.fp.VFPU
import darecreek.exu.vfucore.fp.fudian._
import darecreek.exu.vfucoreconfig.{VUop, Redirect}

class ReductionFP(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VExuInput))
    val redirect = Input(new Redirect)
    val out = Decoupled(new VCrossExuOut)
  })


  val uop = io.in.bits.uop
  val ctrl = io.in.bits.uop.ctrl
  val info = io.in.bits.uop.info
  val funct6 = io.in.bits.uop.ctrl.funct6
  val funct3 = io.in.bits.uop.ctrl.funct3
  val vm = io.in.bits.uop.ctrl.vm
  val vs1_imm = io.in.bits.uop.ctrl.vs1_imm
  val widen = io.in.bits.uop.ctrl.widen
  val narrow = io.in.bits.uop.ctrl.narrow
  val ma = io.in.bits.uop.info.ma
  val ta = io.in.bits.uop.info.ta
  val vsew = io.in.bits.uop.info.vsew
  val vlmul = io.in.bits.uop.info.vlmul
  val vl = io.in.bits.uop.info.vl
  val vstart = io.in.bits.uop.info.vstart
  val in_vstart_gte_vl = vstart >= vl
  val narrow_to_1 = io.in.bits.uop.ctrl.narrow_to_1
  val narrow_to_1_vstart_svl = io.in.bits.uop.ctrl.narrow_to_1 & !in_vstart_gte_vl
  val vxrm = io.in.bits.uop.info.vxrm
  val frm = io.in.bits.uop.info.frm
  val uopIdx = io.in.bits.uop.expdIdx
  val uopEnd = io.in.bits.uop.expdEnd
  //  val sysUop = io.in.bits.uop.sysUop
  val vs1 = Cat(io.in.bits.vSrc(0).reverse)
  val vs2 = Cat(io.in.bits.vSrc(1).reverse)
  val old_vd = Cat(io.in.bits.vSrc(2).reverse)
  val vmask = Cat(io.in.bits.vSrc(3).reverse)
  val ftype = VFPU.getTypeTagFromVSEW(vsew)
  val rs1 = VFPU.unbox(io.in.bits.rs1, ftype)

  val fire = io.in.fire

  val fpu = Seq.fill(NLanes / 2)(Module(new VRedFPU()(p)))
  // val fpu = Seq.fill(NLanes / 2)(Module(new VFPUTop()(p)))

  val vfredosum_vs = ((funct6 === "b000011".U) && (funct3 === "b001".U)) || ((funct6 === "b000001".U) && (funct3 === "b001".U))
  // val vfredusum_vs = (funct6 === "b000001".U) && (funct3 === "b001".U)
  val vfredusum_vs = false.B
  val vfredmax_vs = (funct6 === "b000111".U) && (funct3 === "b001".U)
  val vfredmin_vs = (funct6 === "b000101".U) && (funct3 === "b001".U)
  val vfwredosum_vs = ((funct6 === "b110011".U) && (funct3 === "b001".U)) || ((funct6 === "b110001".U) && (funct3 === "b001".U))
  // val vfwredusum_vs = (funct6 === "b110001".U) && (funct3 === "b001".U)
  val vfwredusum_vs = false.B

  val reg_vfredosum_vs = RegEnable(vfredosum_vs, false.B, fire)
  val reg_vfredusum_vs = RegEnable(vfredusum_vs, false.B, fire)
  val reg_vfredmax_vs = RegEnable(vfredmax_vs, false.B, fire)
  val reg_vfredmin_vs = RegEnable(vfredmin_vs, false.B, fire)
  val reg_vfwredosum_vs = RegEnable(vfwredosum_vs, false.B, fire)
  val reg_vfwredusum_vs = RegEnable(vfwredusum_vs, false.B, fire)

  val fpu_red = vfredosum_vs ||
    vfredusum_vs ||
    vfredmax_vs ||
    vfredmin_vs ||
    vfwredosum_vs ||
    vfwredusum_vs

  val fpu_red_cmp = vfredmax_vs || vfredmin_vs

  val widen2 = Mux(vfwredosum_vs || vfwredusum_vs, true.B, io.in.bits.uop.ctrl.widen2)

  val red_busy = RegInit(false.B)
  val red_uop_busy = RegInit(false.B)

  val outUopReg = Reg(new VUop)
  when(fire && fpu_red) {
    outUopReg := io.in.bits.uop
  }

  // fp reduction redirect handling
  val flush = RegInit(false.B)
  //  val in_robIdx = sysUop.robIdx
  //  val currentRobIdx = RegEnable(in_robIdx, fpu_red && fire)
  // val output_red = red_uop_busy && (fpu(0).io.out.bits.uop.sysUop.robIdx === currentRobIdx)
  val output_red = red_uop_busy
  val flush_in = io.redirect.needFlush(uop.asTypeOf(new darecreek.exu.vfucoreconfig.VUop).robIdx)
  val flush_in_reg = io.redirect.needFlush(outUopReg.asTypeOf(new darecreek.exu.vfucoreconfig.VUop).robIdx)

  when(fpu_red && fire) {
    flush := flush_in
  }.otherwise {
    flush := (flush || flush_in_reg) && red_busy
  }

  val idle :: calc_vs2 :: calc_vs1 :: Nil = Enum(3)

  val vd_vsew = Mux(widen | widen2, vsew + 1.U, vsew)
  val vsew_reg = RegEnable(vsew, 0.U, fire)
  val vd_vsew_reg = RegEnable(vd_vsew, 0.U, fire)
  val eew = SewOH(vsew)
  val eew_reg = SewOH(vsew_reg)
  val eewVd = SewOH(vd_vsew)
  val eewVd_reg = SewOH(vd_vsew_reg)
  val vsew_bits = RegEnable(Mux1H(eew.oneHot, Seq(8.U(7.W), 16.U(7.W), 32.U(7.W), 64.U(7.W))), 0.U, fire)
  val ta_reg = RegEnable(ta, false.B, fire)
  val vl_reg = RegEnable(vl, 0.U, fire)
  val red_state = RegInit(idle)

  val vs2_cnt = RegInit(0.U(vlenbWidth.W))
  val vs2_in_cnt = RegInit(0.U(vlenbWidth.W))
  val vs2_rnd = RegInit(0.U(vlenbWidth.W))

  when(fire) {
    when(vfredosum_vs || vfwredosum_vs || vfwredusum_vs) {
      vs2_rnd := Mux1H(eew.oneHot, VecInit(Seq((VLEN / 8 - 1).U, (VLEN / 16 - 1).U, (VLEN / 32 - 1).U, (VLEN / 64 - 1).U)))
    }.otherwise {
      vs2_rnd := vlenbWidth.U - vsew
    }
  }

  val red_out_valid = Wire(Bool())
  val red_out_ready = true.B
  val fpu_valid = RegInit(false.B)
  val red_in_valid = Wire(Bool())
  val red_in_ready = Wire(Bool())

  val red_out = Wire(Vec(NLanes / 2, new LaneFUOutput))
  val red_in = Wire(Vec(NLanes / 2, new LaneFUInput))
  val red_out_vd = Wire(Vec(NLanes / 2, UInt(LaneWidth.W)))
  val vd_mask = (~0.U(VLEN.W))
  val vd_mask_half = (~0.U((VLEN / 2).W))
  val red_vd_bits = Cat(0.U((VLEN / 2).W), Cat(red_out_vd.reverse))

  val vs1_zero = RegInit(0.U(64.W))
  val vs1_zero_bypass = RegInit(false.B)
  val output_valid = RegInit(false.B)
  val output_data = RegInit(0.U(VLEN.W))


  when(fpu_red && fire) {
    vs1_zero := Mux1H(eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 0.U), vs1(n - 1, 0))) :+ vs1(63, 0))
  }

  val old_vd_bits = RegInit(0.U(VLEN.W))
  val red_vd_tail_one = Wire(UInt(VLEN.W))
  val red_vd_tail_vd = Wire(UInt(VLEN.W))
  val vd_mask_vsew_vd = Wire(UInt(VLEN.W))
  vd_mask_vsew_vd := Mux1H(eewVd_reg.oneHot, Seq(8, 16, 32, 64).map(sew => Cat(~0.U((VLEN - sew).W), 0.U(sew.W))))
  red_vd_tail_one := vd_mask_vsew_vd | Mux(vs1_zero_bypass, vs1_zero, (red_vd_bits & (~vd_mask_vsew_vd)))
  red_vd_tail_vd := (old_vd_bits & vd_mask_vsew_vd) | Mux(vs1_zero_bypass, vs1_zero, (red_vd_bits & (~vd_mask_vsew_vd)))

  val red_vd = Mux(vl_reg === 0.U, old_vd_bits, Mux(ta_reg, red_vd_tail_one, red_vd_tail_vd))

  for (i <- 0 until NLanes / 2) {
    red_out_vd(i) := red_out(i).vd
  }

  when(flush) {
    red_state := idle
  }.otherwise {
    switch(red_state) {
      is(idle) {
        when(fpu_red && fire) {
          red_state := calc_vs2
        }
      }

      is(calc_vs2) {
        when((vs2_cnt === (vs2_rnd - 1.U)) && (red_out_valid && red_out_ready)) {
          red_state := calc_vs1
        }
      }

      is(calc_vs1) {
        when(red_out_valid && red_out_ready) {
          red_state := idle
        }
      }
    }
  }

  val expdIdxZero = RegInit(false.B)
  val output_en = RegInit(false.B)
  when(flush) {
    output_en := false.B
  }.elsewhen(fire) {
    when(fpu_red && uopEnd) {
      output_en := true.B
    }.otherwise {
      output_en := false.B
    }
  }

  when(fire && fpu_red) {
    when(uopIdx === 0.U) {
      expdIdxZero := true.B
    }.otherwise {
      expdIdxZero := false.B
    }
  }

  when(fire) {
    old_vd_bits := old_vd
  }

  when(flush) {
    red_uop_busy := false.B
  }.elsewhen(fire && fpu_red) {
    red_uop_busy := true.B
    // }.elsewhen(output_en && io.out.valid && io.out.ready && (io.out.bits.uop.sysUop.robIdx === currentRobIdx)) {
  }.elsewhen(output_en && io.out.valid && io.out.ready) {
    red_uop_busy := false.B
  }.elsewhen(!output_en && (red_state === calc_vs1) && red_out_valid && red_out_ready) {
    red_uop_busy := false.B
  }

  when(flush) {
    red_busy := false.B
  }.elsewhen(fire && fpu_red && (uopIdx === 0.U)) {
    red_busy := true.B
  }.elsewhen(output_en && io.out.valid && io.out.ready) {
    red_busy := false.B
  }

  fpu_valid := false.B
  when(fpu_red && fire) {
    fpu_valid := true.B
  }

  when(flush) {
    vs2_cnt := 0.U
  }.elsewhen((red_state === calc_vs2) && red_out_valid && red_out_ready) {
    when(vs2_cnt === (vs2_rnd - 1.U)) {
      vs2_cnt := 0.U
    }.otherwise {
      vs2_cnt := vs2_cnt + 1.U
    }
  }

  when(flush) {
    vs2_in_cnt := 0.U
  }.elsewhen((red_state === calc_vs2) && red_in_valid && red_in_ready) {
    when(vs2_in_cnt === vs2_rnd) {
      vs2_in_cnt := 0.U
    }.otherwise {
      vs2_in_cnt := vs2_in_cnt + 1.U
    }
  }

  def zero(w: Int) = 0.U(w.W)

  def umax(w: Int) = ~(0.U(w.W))

  def smax(w: Int) = Cat(0.U(1.W), ~(0.U((w - 1).W)))

  def smin(w: Int) = Cat(1.U(1.W), 0.U((w - 1).W))

  val ele64 = Wire(UInt(64.W))
  ele64 := 0.U
  when(fire) {
    when((vfredosum_vs || vfredusum_vs || vfwredosum_vs || vfwredusum_vs) && (frm =/= RDN)) {
      when(eew.is32) {
        ele64 := Cat(~0.U(33.W), 0.U(31.W))
      }.otherwise {
        ele64 := Cat(1.U(1.W), 0.U(63.W))
      }
    }.elsewhen(vfredmax_vs || vfredmin_vs) {
      when(eew.is32) {
        ele64 := Cat(0.U(32.W), FloatPoint.defaultNaNUInt(VFPU.f32.expWidth, VFPU.f32.precision))
      }.otherwise {
        ele64 := FloatPoint.defaultNaNUInt(VFPU.f64.expWidth, VFPU.f64.precision)
      }
    }
  }

  //---- Tail gen ----
  val tail = TailGen(io.in.bits.uop.info.vl, uopIdx, eewVd, narrow)
  //---- Prestart gen ----
  val prestart = PrestartGen(io.in.bits.uop.info.vstart, uopIdx, eewVd, narrow)
  //---- Mask gen ----
  val maskIdx = Mux(narrow, uopIdx >> 1, uopIdx)
  val mask16b_red = MaskExtract(vmask, maskIdx, eew, VLEN)

  val mask16bReorg_red = MaskReorg.splash(mask16b_red, eew, vlenb)

  val vs2_bytes = VecInit(Seq.tabulate(vlenb)(i => vs2((i + 1) * 8 - 1, i * 8)))
  val vs2m_bytes = Wire(Vec(vlenb, UInt(8.W)))
  val vs2m_bits = RegEnable(Cat(vs2m_bytes.reverse), 0.U, fire)
  val vlRemainBytes = Wire(UInt(8.W))
  vlRemainBytes := Mux((vl << vsew) >= Cat(uopIdx, 0.U((log2Up(VLEN) - 3).W)), (vl << vsew) - Cat(uopIdx, 0.U((log2Up(VLEN) - 3).W)), 0.U)

  val vl_vmask = Wire(UInt(VLEN.W))
  val vmask_vl = Wire(UInt(VLEN.W))
  vl_vmask := ~((~0.U(VLEN.W)) << vl)
  vmask_vl := vmask & vl_vmask

  when(flush) {
    vs1_zero_bypass := false.B
  }.elsewhen(fpu_red && fire && !vm && !(vmask_vl.orR)) {
    vs1_zero_bypass := true.B
  }.elsewhen(output_valid) {
    vs1_zero_bypass := false.B
  }

  for (i <- 0 until vlenb) {
    vs2m_bytes(i) := vs2_bytes(i)
    when((!vm && !mask16bReorg_red(i)) || (i.U >= vlRemainBytes)) {
      when(vsew === 0.U) {
        vs2m_bytes(i) := ele64(7, 0)
      }.elsewhen(vsew === 1.U) {
        vs2m_bytes(i) := ele64((i % 2 + 1) * 8 - 1, (i % 2) * 8)
      }.elsewhen(vsew === 2.U) {
        vs2m_bytes(i) := ele64((i % 4 + 1) * 8 - 1, (i % 4) * 8)
      }.elsewhen(vsew === 3.U) {
        vs2m_bytes(i) := ele64((i % 8 + 1) * 8 - 1, (i % 8) * 8)
      }
    }
  }

  val vs2_rnd0 = fpu_valid
  val vs2_rndx = (red_state === calc_vs2) && (vs2_cnt === (vs2_rnd - 1.U)) && red_out_valid && red_out_ready
  val red_out_bits = Cat(red_out_vd.reverse)
  val vs2m_bits_hi = vs2m_bits(VLEN - 1, VLEN / 2)
  val red_out_hi = (red_out_bits & (vd_mask_half >> ((VLEN / 2).U - ((VLEN / 2).U >> vs2_cnt)))) >> ((VLEN / 4).U >> vs2_cnt)
  val vs2m_bits_lo = vs2m_bits(VLEN / 2 - 1, 0)
  val red_out_lo = red_out_bits & (vd_mask_half >> ((VLEN / 2).U - ((VLEN / 4).U >> vs2_cnt)))

  val red_zero = RegEnable(red_out_bits(63, 0), (red_state === calc_vs1) && red_out_valid && red_out_ready)
  val red_vs1_zero = Mux(expdIdxZero, vs1_zero, red_zero)
  val vs2_shift = Mux1H(eew_reg.oneHot, Seq(3, 4, 5, 6).map(k => Cat(vs2_in_cnt, 0.U(k.W))))
  val vs2_order = (vs2m_bits >> vs2_shift) & (vd_mask >> ((VLEN).U - vsew_bits))
  val red_vs1_bits = Wire(UInt((VLEN / 2).W))
  val red_vs2_bits = Wire(UInt((VLEN / 2).W))
  val red_vs1 = VecInit(Seq.tabulate(NLanes / 2)(i => red_vs1_bits((i + 1) * LaneWidth - 1, i * LaneWidth)))
  val red_vs2 = VecInit(Seq.tabulate(NLanes / 2)(i => red_vs2_bits((i + 1) * LaneWidth - 1, i * LaneWidth)))
  val red_uop = Reg(new VExpdUOp)
  // red_in_valid := (fpu_valid || ((red_state === calc_vs2) && red_out_valid)) & !flush & !red_uop.sysUop.robIdx.needFlush(io.redirect)
  red_in_valid := (fpu_valid || ((red_state === calc_vs2) && red_out_valid)) & !flush

  when(reg_vfredosum_vs || reg_vfwredosum_vs || reg_vfwredusum_vs) {
    when(vs2_rnd0) {
      red_vs2_bits := red_vs1_zero
    }.otherwise {
      red_vs2_bits := red_out_bits
    }
    red_vs1_bits := vs2_order(VLEN / 2 - 1, 0)
  }.otherwise {
    when(vs2_rnd0) {
      red_vs1_bits := vs2m_bits_lo
      red_vs2_bits := vs2m_bits_hi
    }.elsewhen(vs2_rndx) {
      red_vs1_bits := red_out_bits
      red_vs2_bits := red_vs1_zero
    }.otherwise {
      red_vs1_bits := red_out_lo
      red_vs2_bits := red_out_hi
    }
  }

  when(fire) {
    red_uop := uop
    red_uop.ctrl.funct6 := red_uop.ctrl.funct6
    red_uop.ctrl.funct3 := red_uop.ctrl.funct3
    red_uop.info.vsew := red_uop.info.vsew
    red_uop.info.destEew := red_uop.info.destEew
    red_uop.ctrl.lsrc(0) := vs1_imm
    red_uop.ctrl.lsrc(1) := 0.U
    red_uop.ctrl.ldest := 0.U
    red_uop.ctrl.vm := true.B
    red_uop.ctrl.widen := false.B
    red_uop.ctrl.widen2 := widen2
    red_uop.ctrl.narrow := narrow
    red_uop.ctrl.narrow_to_1 := narrow_to_1
    red_uop.info.vstart := vstart
    red_uop.info.vl := Mux(vsew === 2.U, 4.U, 2.U)
    red_uop.info.vxrm := vxrm
    red_uop.info.frm := frm
    red_uop.info.vlmul := 0.U
    red_uop.info.vsew := vsew
    red_uop.info.ma := ma
    red_uop.info.ta := ta
    red_uop.info.destEew := Mux(widen || widen2,
      info.vsew + 1.U, info.vsew)
    red_uop.pdestVal := false.B
    // red_uop.sysUop := sysUop

    red_uop.expdIdx := 0.U
    red_uop.expdEnd := true.B
    when(vfredosum_vs) {
      red_uop.ctrl.funct6 := "b000000".U
      red_uop.ctrl.funct3 := "b001".U
    }.elsewhen(vfredusum_vs) {
      red_uop.ctrl.funct6 := "b000000".U
      red_uop.ctrl.funct3 := "b001".U
    }.elsewhen(vfredmax_vs) {
      red_uop.ctrl.funct6 := "b000110".U
      red_uop.ctrl.funct3 := "b001".U
    }.elsewhen(vfredmin_vs) {
      red_uop.ctrl.funct6 := "b000100".U
      red_uop.ctrl.funct3 := "b001".U
    }.elsewhen(vfwredosum_vs || vfwredusum_vs) {
      red_uop.ctrl.funct6 := "b110100".U
      red_uop.ctrl.funct3 := "b001".U
    }
  }

  for (i <- 0 until NLanes / 2) {
    red_in(i).uop := red_uop
    red_in(i).vs1 := red_vs1(i)
    red_in(i).vs2 := red_vs2(i)
    red_in(i).old_vd := "hffffffffffffffff".U
    red_in(i).rs1 := rs1
    red_in(i).prestart := 0.U
    red_in(i).mask := "hff".U
    red_in(i).tail := 0.U
  }

  when(flush) {
    output_valid := false.B
    // }.elsewhen(output_en && io.out.valid && io.out.ready && (io.out.bits.uop.sysUop.robIdx === currentRobIdx)) {
  }.elsewhen(output_en && io.out.valid && io.out.ready) {
    output_valid := false.B
  }.elsewhen(output_en && (red_state === calc_vs1) && red_out_valid && red_out_ready) {
    output_valid := true.B
  }

  when(output_en && (red_state === calc_vs1) && red_out_valid && red_out_ready) {
    output_data := red_vd
  }

  for (i <- 0 until NLanes / 2) {
    fpu(i).io.in.valid := red_in_valid
    fpu(i).io.in.bits.uop := red_in(i).uop
    fpu(i).io.in.bits.vs1 := red_in(i).vs1
    fpu(i).io.in.bits.vs2 := red_in(i).vs2
    fpu(i).io.in.bits.old_vd := red_in(i).old_vd
    fpu(i).io.in.bits.rs1 := rs1
    fpu(i).io.in.bits.prestart := red_in(i).prestart
    fpu(i).io.in.bits.mask := red_in(i).mask
    fpu(i).io.in.bits.tail := red_in(i).tail
    fpu(i).io.out.ready := red_out_ready
    fpu(i).io.redirect := io.redirect
    red_out(i) := fpu(i).io.out.bits
  }

  io.out.bits.uop := outUopReg

  val vstart_gte_vl = io.out.bits.uop.info.vstart >= io.out.bits.uop.info.vl

  val red_fflag = RegInit(0.U(5.W))

  when(flush || vs1_zero_bypass) {
    red_fflag := 0.U
    // }.elsewhen(output_en && io.out.valid && io.out.ready && (io.out.bits.uop.sysUop.robIdx === currentRobIdx)) {
  }.elsewhen(output_en && io.out.valid && io.out.ready) {
    red_fflag := 0.U
  }.elsewhen(red_out_valid && red_out_ready) {
    red_fflag := red_fflag | fpu(0).io.out.bits.fflags | fpu(1).io.out.bits.fflags
  }

  io.out.bits.vd := VecInit(Seq.tabulate(NLanes)(i => (output_data)((i + 1) * LaneWidth - 1, i * LaneWidth)))
  io.in.ready := fpu(0).io.in.ready && fpu(1).io.in.ready && !red_uop_busy
  io.out.bits.fflags := Mux(vstart_gte_vl, 0.U, Mux(output_en, red_fflag, 0.U))
  io.out.valid := output_valid
  red_in_ready := fpu(0).io.in.ready && fpu(1).io.in.ready
  red_out_valid := fpu(0).io.out.valid && fpu(1).io.out.valid && output_red
}

import xiangshan._

object VerilogRedFP extends App {
  println("Generating hardware")
  val p = Parameters.empty
  emitVerilog(new ReductionFP()(p.alterPartial({ case VFuParamsKey =>
    VFuParameters(VLEN = 256)
  })), Array("--target-dir", "generated",
    "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}

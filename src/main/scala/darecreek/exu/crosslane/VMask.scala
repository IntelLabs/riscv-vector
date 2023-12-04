package darecreek

import chisel3._
import chisel3.util._
import scala.language.postfixOps

class VMask extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VExuInput))
    val out = Decoupled(new VExuOutput)
  })

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
  val expdLen = uop.expdLen
  val expdIdx = uop.expdIdx
  val expdEnd = uop.expdEnd
  val vm = uop.ctrl.vm
  val vlmul = uop.info.vlmul
  val vsew = uop.info.vsew
  val ma = uop.info.ma
  val ta = uop.info.ta
  val vstartRemain = io.in.bits.vstartRemain
  // val vstartRemainBytes = vstartRemain << vsew;
  val vlRemain = io.in.bits.vlRemain
  // val vlRemainBytes = io.in.bits.vlRemain << vsew
  // val vlRemainBits = Cat(vlRemainBytes, 0.U(3.W))
  val fire = io.in.fire

  val vmand_mm = (funct6 === "b011001".U) && (funct3 === "b010".U)
  val vmnand_mm = (funct6 === "b011101".U) && (funct3 === "b010".U)
  val vmandn_mm = (funct6 === "b011000".U) && (funct3 === "b010".U)
  val vmxor_mm = (funct6 === "b011011".U) && (funct3 === "b010".U)
  val vmor_mm = (funct6 === "b011010".U) && (funct3 === "b010".U)
  val vmnor_mm = (funct6 === "b011110".U) && (funct3 === "b010".U)
  val vmorn_mm = (funct6 === "b011100".U) && (funct3 === "b010".U)
  val vmxnor_mm = (funct6 === "b011111".U) && (funct3 === "b010".U)

  val vcpop_m = (funct6 === "b010000".U) && (funct3 === "b010".U) && (vs1_index === "b10000".U)
  val vfirst_m = (funct6 === "b010000".U) && (funct3 === "b010".U) && (vs1_index === "b10001".U)
  val vmsbf_m = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b00001".U)
  val vmsif_m = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b00011".U)
  val vmsof_m = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b00010".U)
  val viota_m = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b10000".U)
  val vid_v = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b10001".U)

  val vm_logical = vmand_mm ||
    vmnand_mm ||
    vmandn_mm ||
    vmxor_mm ||
    vmor_mm ||
    vmnor_mm ||
    vmorn_mm ||
    vmxnor_mm
  /////////////////////////////////

  val vmand = Wire(UInt(VLEN.W))
  val vmandn = Wire(UInt(VLEN.W))
  val vmxor = Wire(UInt(VLEN.W))
  val vmor = Wire(UInt(VLEN.W))
  val vmorn = Wire(UInt(VLEN.W))
  val vmnand = Wire(UInt(VLEN.W))
  val vmnor = Wire(UInt(VLEN.W))
  val vmxnor = Wire(UInt(VLEN.W))

  vmand := vs2 & vs1
  vmandn := vs2 & ~vs1
  vmxor := vs2 ^ vs1
  vmor := vs2 | vs1
  vmorn := vs2 | ~vs1
  vmnand := ~vmand
  vmnor := ~vmor
  vmxnor := ~vmxor

  val eew = SewOH(vsew)
  val vsew_plus1 = Wire(UInt(3.W))
  vsew_plus1 := Cat(0.U(1.W), ~vsew(1, 0)) + 1.U
  val vsew_bytes = Mux1H(eew.oneHot, Seq(1.U(4.W), 2.U(4.W), 4.U(4.W), 8.U(4.W)))
  val vsew_bits = Mux1H(eew.oneHot, Seq(8.U(7.W), 16.U(7.W), 32.U(7.W), 64.U(7.W)))
  val ele_cnt = Mux1H(eew.oneHot, Seq(16.U(5.W), 8.U(5.W), 4.U(5.W), 2.U(5.W)))
  // val vlRemain = Wire(UInt(8.W))
  val vlRemainBytes = vlRemain << vsew
  val all_one = (~0.U(VLEN.W))

  val vmfirst = Wire(UInt(XLEN.W))
  val vmsbf = Wire(UInt(VLEN.W))
  val vmsif = Cat(vmsbf(VLEN - 2, 0), 1.U)
  val vmsof = Wire(UInt(VLEN.W))
  val nmask = Wire(UInt(VLEN.W))
  val vd_nmask = Wire(UInt(VLEN.W))
  val vd_mask = Wire(UInt(VLEN.W))
  val vs2m = Wire(Vec(VLEN, UInt(1.W)))

  def sbf(data: UInt): UInt = {
    val w = data.getWidth
    val result = Wire(UInt(w.W))
    if (w == 1) {
      result := Mux(data(0), 0.U(1.W), 1.U(1.W))
    } else {
      val hi = sbf(data(w - 1, w / 2))
      val lo = sbf(data(w / 2 - 1, 0))
      result := Mux(lo(w / 2 - 1), Cat(hi, lo), Cat(0.U((w / 2).W), lo))
    }
    result
  }

  def vfirst(data: UInt): UInt = {
    val w = data.getWidth
    val logW = log2Ceil(w) // 1 -> 0, 2 -> 1, 4 -> 2
    val result = Wire(UInt((logW + 1).W))
    if (w == 1) {
      result := Mux(data(0), 0.U(1.W), 1.U(1.W))
    } else {
      val hi = vfirst(data(w - 1, w / 2))
      val lo = vfirst(data(w / 2 - 1, 0))
      result := Mux(!lo(logW - 1), Cat(0.U(1.W), lo),
        if (w == 2) Cat(hi(logW - 1), 1.U(1.W)) else Cat(hi(logW - 1), 1.U(1.W), hi(logW - 2, 0)))
    }
    result
  }

  // vlRemain := Mux(vl >= (uopIdx << vsew_plus1), vl - (uopIdx << vsew_plus1), 0.U)

  for (i <- 0 until VLEN) {
    vs2m(i) := 0.U
    when(fire) {
      // vs2m(i) := vs2(i) & (vmask(i) | vm) & (i.U < vl)
      vs2m(i) := vs2(i) & (vmask(i) | vm) & (i.U < vlRemain)
    }
  }

  vmsof := ~vmsbf & vmsif
  vmsbf := sbf(Cat(vs2m.reverse))
  vmfirst := BitsExtend(vfirst(Cat(vs2m.reverse)), XLEN, true.B)


  // viota/vid/vcpop
  val vs2m_uop = MaskExtract(Cat(vs2m.reverse), expdIdx, eew)
  val vs2m_uop_vid = Mux(vid_v, Fill(vlenb, vid_v), vs2m_uop)
  val one_sum = RegInit(0.U(8.W))
  val one_cnt = Wire(Vec(vlenb + 1, UInt(8.W)))
  val one_cnt_uop = Wire(Vec(vlenb + 1, UInt(5.W)))
  val vid_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew8 = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew16 = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew32 = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew64 = Wire(Vec(vlenb, UInt(8.W)))

  for (i <- 0 until vlenb + 1) {
    one_cnt_uop(i) := 0.U
  }

  for (i <- 0 until vlenb) {
    one_cnt_uop(i + 1) := PopCount(vs2m_uop_vid(i, 0))
  }

  when(fire) {
    one_sum := one_cnt(ele_cnt)
  }

  for (i <- 0 until vlenb + 1) {
    one_cnt(i) := Mux(expdIdx === 0.U, one_cnt_uop(i), one_sum + one_cnt_uop(i))
  }

  val tail_vd = Wire(UInt(VLEN.W))
  val vd_reg = RegInit(0.U(VLEN.W))
  val vd_out = Wire(UInt(VLEN.W))

  when(vmand_mm && fire) {
    vd_reg := vmand
  }.elsewhen(vmnand_mm && fire) {
    vd_reg := vmnand
  }.elsewhen(vmandn_mm && fire) {
    vd_reg := vmandn
  }.elsewhen(vmxor_mm && fire) {
    vd_reg := vmxor
  }.elsewhen(vmor_mm && fire) {
    vd_reg := vmor
  }.elsewhen(vmnor_mm && fire) {
    vd_reg := vmnor
  }.elsewhen(vmorn_mm && fire) {
    vd_reg := vmorn
  }.elsewhen(vmxnor_mm && fire) {
    vd_reg := vmxnor
  }.elsewhen(vmsbf_m && fire) {
    vd_reg := vmsbf
  }.elsewhen(vmsif_m && fire) {
    vd_reg := vmsif
  }.elsewhen(vmsof_m && fire) {
    vd_reg := vmsof
  }.elsewhen(vfirst_m && fire) {
    vd_reg := Cat(0.U((VLEN - XLEN).W), vmfirst)
  }.elsewhen((vid_v || viota_m) && fire) {
    vd_reg := Cat(one_cnt.reverse)(VLEN - 1, 0)
  }.elsewhen(vcpop_m && fire) {
    vd_reg := one_cnt(ele_cnt)
  }

  // val vstartRemain = Wire(UInt(7.W))
  // vstartRemain := Mux(vid_v, Mux(vstart >= (expdIdx << vsew_plus1), (vstart - (expdIdx << vsew_plus1)), 0.U), 0.U)

  val vl_reg = RegEnable(vlRemain, 0.U, fire)
  // val vstart_reg = RegEnable(vstart, 0.U, fire)
  val vstart_reg = RegEnable(vstartRemain, 0.U, fire)
  val vm_reg = RegEnable(vm, false.B, fire)
  val ma_reg = RegEnable(ma, false.B, fire)
  val ta_reg = RegEnable(ta, false.B, fire)
  val vsew_reg = RegEnable(vsew, 0.U, fire)
  val uopIdx_reg = RegEnable(expdIdx, 0.U, fire)
  val vlRemainBytes_reg = RegEnable(vlRemainBytes, 0.U, fire)
  val vstartRemain_reg = RegEnable(vstartRemain, 0.U, fire)
  val old_vd_reg = RegEnable(old_vd, 0.U, fire)
  val vmask_reg = RegEnable(vmask, 0.U, fire)
  val reg_vm_logical = RegEnable(vm_logical, false.B, fire)
  val reg_vcpop_m = RegEnable(vcpop_m, false.B, fire)
  val reg_vfirst_m = RegEnable(vfirst_m, false.B, fire)
  val reg_vmsbf_m = RegEnable(vmsbf_m, false.B, fire)
  val reg_vmsif_m = RegEnable(vmsif_m, false.B, fire)
  val reg_vmsof_m = RegEnable(vmsof_m, false.B, fire)
  val reg_viota_m = RegEnable(viota_m, false.B, fire)
  val reg_vid_v = RegEnable(vid_v, false.B, fire)
  val vsew_plus1_reg = Wire(UInt(3.W))
  vsew_plus1_reg := Cat(0.U(1.W), ~vsew_reg(1, 0)) + 1.U

  val vmask_bits = Wire(UInt(VLEN.W))
  vmask_bits := vmask_reg >> (uopIdx_reg << vsew_plus1_reg)
  val vmask_vd_bytes = Wire(Vec(vlenb, UInt(8.W)))
  val vmask_vd_bits = Cat(vmask_vd_bytes.reverse)
  val vmask_old_vd = old_vd_reg & (~vmask_vd_bits)
  val vmask_ones_vd = all_one & (~vmask_vd_bits)
  val vmask_vd = Mux(ma_reg, vmask_ones_vd, vmask_old_vd)
  val vid_mask_vd = (Cat(vid_vd.reverse) & vmask_vd_bits) | vmask_vd

  val vstartRemainBytes = Wire(UInt(7.W))
  val vstart_bytes = Wire(UInt(5.W))
  vstartRemainBytes := vstartRemain_reg << vsew_reg
  vstart_bytes := Mux(vstartRemainBytes >= vlenb.U, vlenb.U, vstartRemainBytes)
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = Wire(UInt(VLEN.W))
  vmask_vstart_bits := all_one << vstart_bits
  val vstart_old_vd = old_vd_reg & (~vmask_vstart_bits)

  val tail_bytes = Mux((vlRemainBytes_reg >= vlenb.U), 0.U, vlenb.U - vlRemainBytes_reg)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = Wire(UInt(VLEN.W))
  vmask_tail_bits := all_one >> tail_bits
  val tail_old_vd = old_vd_reg & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val vid_tail_vd = Mux(ta_reg, tail_ones_vd, tail_old_vd)
  val vid_tail_mask_vd = Wire(UInt(VLEN.W))
  val vd_reg_byte = VecInit(Seq.tabulate(vlenb)(i => vd_reg((i + 1) * 8 - 1, i * 8)))

  for (i <- 0 until vlenb) {
    vid_vd(i) := 0.U
    vid_vd_sew8(i) := 0.U
    vid_vd_sew16(i) := 0.U
    vid_vd_sew32(i) := 0.U
    vid_vd_sew64(i) := 0.U
  }

  for (i <- 0 until vlenb) {
    vid_vd_sew8(i) := vd_reg_byte(i)
  }

  for (i <- 0 until vlenb) {
    when(i.U % 2.U === 0.U) {
      vid_vd_sew16(i) := vd_reg_byte(i / 2)
    }.otherwise {
      vid_vd_sew16(i) := 0.U
    }
  }

  for (i <- 0 until vlenb) {
    when(i.U % 4.U === 0.U) {
      vid_vd_sew32(i) := vd_reg_byte(i / 4)
    }.otherwise {
      vid_vd_sew32(i) := 0.U
    }
  }
  for (i <- 0 until vlenb) {
    when(i.U % 8.U === 0.U) {
      vid_vd_sew64(i) := vd_reg_byte(i / 8)
    }.otherwise {
      vid_vd_sew64(i) := 0.U
    }
  }

  for (i <- 0 until vlenb) {
    when(vsew_reg === 0.U) {
      vid_vd(i) := vid_vd_sew8(i)
    }.elsewhen(vsew_reg === 1.U) {
      vid_vd(i) := vid_vd_sew16(i)
    }.elsewhen(vsew_reg === 2.U) {
      vid_vd(i) := vid_vd_sew32(i)
    }.elsewhen(vsew_reg === 3.U) {
      vid_vd(i) := vid_vd_sew64(i)
    }
  }

  for (i <- 0 until vlenb) {
    vmask_vd_bytes(i) := "hff".U
    when((!vm_reg && !vmask_bits(i.U >> vsew_reg)) || (i.U >= vlRemainBytes_reg)) {
      vmask_vd_bytes(i) := 0.U
    }
  }

  vid_tail_mask_vd := 0.U
  when(reg_vid_v || reg_viota_m) {
    vid_tail_mask_vd := (vid_mask_vd & vmask_tail_bits & vmask_vstart_bits) | vid_tail_vd | vstart_old_vd
  }

  nmask := ~(vmask_reg | Fill(VLEN, vm_reg))
  vd_nmask := Mux(ma_reg, nmask, old_vd_reg & nmask)
  vd_mask := vd_reg & (vmask_reg | Fill(VLEN, vm_reg))
  val mask_vd = vd_nmask | vd_mask

  val old_vd_vl_mask = Wire(UInt(VLEN.W))
  val vd_vl_mask = Wire(UInt(VLEN.W))
  val vstart_mask = Wire(UInt(VLEN.W))
  val vstart_vd = Wire(UInt(VLEN.W))

  old_vd_vl_mask := (~0.U(VLEN.W)) << vl_reg
  vd_vl_mask := (~0.U(VLEN.W)) >> (VLEN.U - vl_reg)

  vstart_mask := (~0.U(VLEN.W)) << vstart_reg
  vstart_vd := old_vd_reg & ~vstart_mask

  tail_vd := vstart_vd | old_vd_vl_mask | (mask_vd & vd_vl_mask & vstart_mask)

  vd_out := vd_reg
  when(vstart_reg >= vl_reg) {
    vd_out := old_vd_reg
  }.elsewhen(reg_vm_logical || reg_vmsbf_m || reg_vmsif_m || reg_vmsof_m) {
    vd_out := tail_vd
  }.elsewhen(reg_vid_v || reg_viota_m) {
    vd_out := vid_tail_mask_vd
  }

  val out_valid = RegInit(false.B)

  when(fire) {
    out_valid := true.B
  }.elsewhen(io.out.valid && io.out.ready) {
    out_valid := false.B
  }

  //--------- Ready & valid ---------
  io.in.ready := (!io.in.valid || io.out.ready)
  io.out.valid := out_valid
  io.out.bits.vd := VecInit(Seq.tabulate(NLanes)(i => (vd_out) ((i + 1) * LaneWidth - 1, i * LaneWidth)))

  io.out.bits.uop := RegEnable(uop, fire)

  io.out.bits.fflags := 0.U
  io.out.bits.vxsat := false.B


  //   /////////////////////////////
  //
  //   val vid_vd = gen_index(expdIdx)
  //
  //   val first = Wire(Vec(NLanes, SInt(xLen.W)))
  //   val vmfirst = Wire(SInt(xLen.W))
  //   val vmsbf = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vmsif = VecInit(Seq.tabulate(NLanes)(i => Cat(Cat(vmsbf.reverse)(VLEN - 2, 0), 1.U)((i + 1) * LaneWidth - 1, i * LaneWidth)))
  //   val vmsof = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vmand = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vmnand = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vmandn = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vmxor = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vmor = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vmnor = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vmorn = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vmxnor = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //
  //   val vd_vmsbf = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_vmsif = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_vmsof = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_vmand = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_vmnand = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_vmandn = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_vmxor = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_vmor = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_vmnor = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_vmorn = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_vmxnor = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //
  //   val ohasone = Wire(Vec(NLanes, Bool()))
  //   val ihasone = Wire(Vec(NLanes, Bool()))
  //
  //   val old_vd_vl_mask = VecInit(Seq.tabulate(NLanes)(i => Mux(vlRemainBytes > vlenb.U, 0.U, ((~0.U(VLEN.W)) << vlRemain))((i + 1) * LaneWidth - 1, i * LaneWidth)))
  //   val vd_vl_mask = VecInit(Seq.tabulate(NLanes)(i => Mux(vlRemainBytes > vlenb.U, (~0.U(VLEN.W)), ((~0.U(VLEN.W)) >> (VLEN.U - vlRemain)))((i + 1) * LaneWidth - 1, i * LaneWidth)))
  //
  //
  //   val nmask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val sbf_mask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val sif_mask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val sof_mask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val vd_nmask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //
  //   val vs2m = Wire(Vec(VLEN, UInt(1.W)))
  //
  //   for (i <- 0 until NLanes) {
  //     vmsof(i) := (~vmsbf(i)) & vmsif(i)
  //     nmask(i) := ~(vmask(i) | Fill(LaneWidth, vm))
  //     vd_nmask(i) := old_vd(i) & nmask(i)
  //     sbf_mask(i) := vmsbf(i) & (vmask(i) | Fill(LaneWidth, vm))
  //     sif_mask(i) := vmsif(i) & (vmask(i) | Fill(LaneWidth, vm))
  //     sof_mask(i) := vmsof(i) & (vmask(i) | Fill(LaneWidth, vm))
  //     vd_vmsbf(i) := vd_nmask(i) | sbf_mask(i)
  //     vd_vmsif(i) := vd_nmask(i) | sif_mask(i)
  //     vd_vmsof(i) := vd_nmask(i) | sof_mask(i)
  //     vd_vmand(i) := vd_nmask(i) | vmand(i)
  //     vd_vmnand(i) := vd_nmask(i) | vmnand(i)
  //     vd_vmandn(i) := vd_nmask(i) | vmandn(i)
  //     vd_vmxor(i) := vd_nmask(i) | vmxor(i)
  //     vd_vmor(i) := vd_nmask(i) | vmor(i)
  //     vd_vmnor(i) := vd_nmask(i) | vmnor(i)
  //     vd_vmorn(i) := vd_nmask(i) | vmorn(i)
  //     vd_vmxnor(i) := vd_nmask(i) | vmxnor(i)
  //
  //   }
  //
  //   for (i <- 0 until VLEN) {
  //     vs2m(i) := 0.U
  //     when(fire) {
  //       vs2m(i) := Cat(vs2.reverse)(i) & (Cat(vmask.reverse)(i) | vm)
  //     }
  //   }
  //
  //   ihasone(0) := false.B
  //   for (i <- 1 until NLanes) {
  //     ihasone(i) := ohasone(i - 1)
  //   }
  //
  //   val slices = Seq.fill(NLanes)(Module(new VMaskSlice))
  //   for (i <- 0 until NLanes) {
  //     slices(i).io.ihasone := ihasone(i)
  //     slices(i).io.vs1 := vs1(i)
  //     slices(i).io.vs2 := vs2m.slice(i * LaneWidth, (i + 1) * LaneWidth)
  //     first(i) := slices(i).io.first + (i * LaneWidth).S
  //     vmsbf(i) := slices(i).io.sbf
  //     ohasone(i) := slices(i).io.ohasone
  //     vmand(i) := slices(i).io.vmand
  //     vmandn(i) := slices(i).io.vmandn
  //     vmxor(i) := slices(i).io.vmxor
  //     vmor(i) := slices(i).io.vmor
  //     vmorn(i) := slices(i).io.vmorn
  //   }
  //
  //   for (i <- 0 until NLanes) {
  //     vmnand(i) := ~vmand(i)
  //     vmnor(i) := ~vmor(i)
  //     vmxnor(i) := ~vmxor(i)
  //   }
  //
  //   vmfirst := -1.S
  //   for (i <- 0 until NLanes reverse) {
  //     when(first(i) =/= -1.S) {
  //       vmfirst := first(i)
  //     }
  //   }
  //
  //   def gen_index(cnt: UInt) = {
  //     val ele64 = Wire(Vec(vlenb, UInt(64.W)))
  //     val vmask_index = Wire(Vec(vlenb, UInt(8.W)))
  //
  //     for (i <- 0 until vlenb) {
  //       ele64(i) := 0.U
  //       ele64(i) := ele_cnt * cnt + i.U / vsew_bytes
  //       vmask_index(i) := 0.U
  //       when(vsew === 0.U) {
  //         vmask_index(i) := ele64(i)(7, 0)
  //       }.elsewhen(vsew === 1.U) {
  //         vmask_index(i) := ele64(i)((i % 2 + 1) * 8 - 1, (i % 2) * 8)
  //       }.elsewhen(vsew === 2.U) {
  //         vmask_index(i) := ele64(i)((i % 4 + 1) * 8 - 1, (i % 4) * 8)
  //       }.elsewhen(vsew === 3.U) {
  //         vmask_index(i) := ele64(i)((i % 8 + 1) * 8 - 1, (i % 8) * 8)
  //       }
  //     }
  //     vmask_index
  //   }
  //
  //   val viota_vd = gen_viota(Cat(vs2m.reverse) >> (ele_cnt * expdIdx), viota_m && fire, viota_m && fire && expdEnd)
  //
  //   def gen_viota(vs: UInt, incr: Bool, end: Bool) = {
  //
  //     val viota_cnt = RegInit(0.U(bVL.W))
  //     //val ele64 = Wire(UInt(64.W))
  //     val viota_vd = Wire(Vec(vlenb, UInt(8.W)))
  //     val slice_cnt = Wire(Vec(vlenb, UInt(64.W)))
  //
  //     // ele64 := 0.U
  //     for (i <- 0 until vlenb) {
  //       viota_vd(i) := 0.U
  //       slice_cnt(i) := viota_cnt
  //     }
  //
  //     when(incr) {
  //       for (i <- 0 until (vlenb - 1)) {
  //         when(i.U < ele_cnt) {
  //           when(vs(i)) {
  //             slice_cnt(i + 1) := slice_cnt(i) + 1.U
  //           }.otherwise {
  //             slice_cnt(i + 1) := slice_cnt(i)
  //           }
  //         }
  //       }
  //
  //       for (i <- 0 until vlenb) {
  //         when(vsew === 0.U) {
  //           viota_vd(i) := slice_cnt(i.U / vsew_bytes)(7, 0)
  //         }.elsewhen(vsew === 1.U) {
  //           viota_vd(i) := slice_cnt(i.U / vsew_bytes)((i % 2 + 1) * 8 - 1, (i % 2) * 8)
  //         }.elsewhen(vsew === 2.U) {
  //           viota_vd(i) := slice_cnt(i.U / vsew_bytes)((i % 4 + 1) * 8 - 1, (i % 4) * 8)
  //         }.elsewhen(vsew === 3.U) {
  //           viota_vd(i) := slice_cnt(i.U / vsew_bytes)((i % 8 + 1) * 8 - 1, (i % 8) * 8)
  //         }
  //       }
  //     }
  //     when(end) {
  //       viota_cnt := 0.U
  //     }.elsewhen(incr) {
  //       viota_cnt := viota_cnt + slice_cnt(ele_cnt)
  //     }
  //     viota_vd
  //   }
  //
  //   val vd_mask = (~0.U(VLEN.W))
  //   val vmask_bits = Cat(vmask.reverse) >> (ele_cnt * expdIdx)
  //   val vmask_vd_bytes = Wire(Vec(vlenb, UInt(8.W)))
  //   val vmask_vd_bits = Cat(vmask_vd_bytes.reverse)
  //
  //   val vmask_old_vd = Cat(old_vd.reverse) & (~vmask_vd_bits)
  //   val vmask_ones_vd = vd_mask & (~vmask_vd_bits)
  //   val vmask_vd = Mux(ma, vmask_ones_vd, vmask_old_vd)
  //
  //   for (i <- 0 until vlenb) {
  //     vmask_vd_bytes(i) := "hff".U
  //     when((!vm && !vmask_bits(i.U / vsew_bytes)) || (i.U >= vlRemainBytes)) {
  //       vmask_vd_bytes(i) := 0.U
  //     }
  //   }
  //
  //   val cross_lane_vd = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val cross_lane_tail_vd = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //   val cross_lane_vd_reg = Reg(Vec(NLanes, UInt(LaneWidth.W)))
  //   val cross_lane_valid = RegInit(false.B)
  //
  //   when((vfirst_m || vpopc_m || vmsbf_m || vmsif_m || vmsof_m || vm_logical || viota_m || vid_v) && fire) {
  //     cross_lane_valid := true.B
  //   }.elsewhen(io.out.valid && io.out.ready) {
  //     cross_lane_valid := false.B
  //   }
  //
  //   val vid_mask_vd = (Cat(vid_vd.reverse) & vmask_vd_bits) | vmask_vd
  //   val viota_mask_vd = (Cat(viota_vd.reverse) & vmask_vd_bits) | vmask_vd
  //
  //   when(vmsbf_m && fire) {
  //     cross_lane_vd := vd_vmsbf
  //   }.elsewhen(vmsif_m && fire) {
  //     cross_lane_vd := vd_vmsif
  //   }.elsewhen(vmsof_m && fire) {
  //     cross_lane_vd := vd_vmsof
  //   }.elsewhen(vmand_mm && fire) {
  //     cross_lane_vd := vd_vmand
  //   }.elsewhen(vmnand_mm && fire) {
  //     cross_lane_vd := vd_vmnand
  //   }.elsewhen(vmandn_mm && fire) {
  //     cross_lane_vd := vd_vmandn
  //   }.elsewhen(vmxor_mm && fire) {
  //     cross_lane_vd := vd_vmxor
  //   }.elsewhen(vmor_mm && fire) {
  //     cross_lane_vd := vd_vmor
  //   }.elsewhen(vmnor_mm && fire) {
  //     cross_lane_vd := vd_vmnor
  //   }.elsewhen(vmorn_mm && fire) {
  //     cross_lane_vd := vd_vmorn
  //   }.elsewhen(vmxnor_mm && fire) {
  //     cross_lane_vd := vd_vmxnor
  //   }.otherwise {
  //     cross_lane_vd := cross_lane_vd_reg
  //   }
  //
  //   val tail_bytes = Mux((vlRemainBytes >= vlenb.U), 0.U, vlenb.U - vlRemainBytes)
  //   val tail_bits = Cat(tail_bytes, 0.U(3.W))
  //   val vmask_tail_bits = vd_mask >> tail_bits
  //   val tail_old_vd = Cat(old_vd.reverse) & (~vmask_tail_bits)
  //   val tail_ones_vd = ~vmask_tail_bits
  //   val tail_vd = Mux(ta, tail_ones_vd, tail_old_vd)
  //   val vid_cross_lane_vd = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  //
  //   for (i <- 0 until NLanes) {
  //     vid_cross_lane_vd(i) := 0.U
  //   }
  //
  //   when(vid_v && fire) {
  //     vid_cross_lane_vd := VecInit(Seq.tabulate(NLanes)(i => ((vid_mask_vd & vmask_tail_bits) | tail_vd) ((i + 1) * LaneWidth - 1, i * LaneWidth)))
  //   }.elsewhen(viota_m && fire) {
  //     vid_cross_lane_vd := VecInit(Seq.tabulate(NLanes)(i => ((viota_mask_vd & vmask_tail_bits) | tail_vd) ((i + 1) * LaneWidth - 1, i * LaneWidth)))
  //   }
  //
  //   for (i <- 0 until NLanes) {
  //     // cross_lane_tail_vd(i) := Mux(ta, (old_vd_vl_mask(i) | (cross_lane_vd(i) & vd_vl_mask(i))), ((old_vd(i) & old_vd_vl_mask(i)) | (cross_lane_vd(i) & vd_vl_mask(i))))
  //     cross_lane_tail_vd(i) := old_vd_vl_mask(i) | (cross_lane_vd(i) & vd_vl_mask(i))
  //   }
  //
  //   for (i <- 0 until NLanes) {
  //     when(fire) {
  //       when(vid_v || viota_m) {
  //         cross_lane_vd_reg(i) := vid_cross_lane_vd(i)
  //       }.otherwise {
  //         cross_lane_vd_reg(i) := cross_lane_tail_vd(i)
  //       }
  //     }
  //   }
  //
  //   val rd = RegInit(0.U(xLen.W))
  //
  //   when(vpopc_m && fire) {
  //     rd := PopCount(Cat(vs2m.reverse))
  //   }.elsewhen(vfirst_m && fire) {
  //     rd := vmfirst.asUInt
  //   }
  //
  //   //--------- Ready & valid ---------
  //   io.in.ready := (!io.in.valid || io.out.ready)
  //   io.out.valid := cross_lane_valid
  //   io.out.bits.vd := cross_lane_vd_reg
  //   when(io.out.bits.uop.ctrl.rdVal) {
  //     io.out.bits.vd(0) := rd
  //   }
  //
  //   io.out.bits.uop := RegEnable(uop, fire)
  //   // io.out.bits.rd := rd
  //
  //   // temp!!
  //   io.out.bits.fflags := 0.U
  //   io.out.bits.vxsat := false.B
}

object VerilogMask extends App {
  println("Generating the VPU Mask hardware")
  emitVerilog(new VMask(), Array("--target-dir", "generated"))
}


object MaskExtract {
  def apply(vmask128b: UInt, uopIdx: UInt, sew: SewOH) = {
    val extracted = Wire(UInt(16.W))
    extracted := Mux1H(Seq.tabulate(8)(uopIdx === _.U),
      Seq.tabulate(8)(idx => Mux1H(sew.oneHot, Seq(16, 8, 4, 2).map(stride =>
        vmask128b((idx + 1) * stride - 1, idx * stride)))))
    extracted
  }

  def mask16_to_2x8(maskIn: UInt, sew: SewOH): Seq[UInt] = {
    require(maskIn.getWidth == 16)
    val result16 = Mux1H(Seq(
      sew.is8 -> maskIn,
      sew.is16 -> Cat(0.U(4.W), maskIn(7, 4), 0.U(4.W), maskIn(3, 0)),
      sew.is32 -> Cat(0.U(6.W), maskIn(3, 2), 0.U(6.W), maskIn(1, 0)),
      sew.is64 -> Cat(0.U(7.W), maskIn(1), 0.U(7.W), maskIn(0)),
    ))
    Seq(result16(7, 0), result16(15, 8))
  }
}


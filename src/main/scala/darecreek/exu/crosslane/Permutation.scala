package darecreek

import chisel3._
import chisel3.util._

class Permutation extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VExuInput))   
    val out = Decoupled(new VCrossExuOut)
  })

val vlenbWidth = log2Up(vlenb)+1
val uop = io.in.bits.uop
val vs1 = io.in.bits.vSrc(0)
val vs2 = io.in.bits.vSrc(1)
val old_vd = io.in.bits.vSrc(2)
val vmask = io.in.bits.vSrc(3)
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
val vlmul =uop.info.vlmul
val vsew =uop.info.vsew
val ma =uop.info.ma
val ta =uop.info.ta
val vstartRemain = io.in.bits.vstartRemain
val vstartRemainBytes = vstartRemain << vsew 
val vlRemain = io.in.bits.vlRemain
val vlRemainBytes = io.in.bits.vlRemain << vsew
val fire = io.in.fire
val vsew_bytes = 1.U << vsew 
val vsew_bytes_reg = RegEnable(vsew_bytes, fire)
val vsew_bytes_mux = Mux(fire, vsew_bytes, vsew_bytes_reg)
val vsew_bits = 8.U << vsew 
val ele_cnt = vlenb.U/vsew_bytes
val old_vs2 = RegEnable(io.in.bits.vSrc(1), fire)

def VMV_X_S         = BitPat("b0100001?????00000010?????1010111")
def VMV_S_X         = BitPat("b010000100000?????110?????1010111")
def VFMV_F_S        = BitPat("b0100001?????00000001?????1010111")
def VFMV_S_F        = BitPat("b010000100000?????101?????1010111")
def VSLIDEUP_VX     = BitPat("b001110???????????100?????1010111")
def VSLIDEUP_VI     = BitPat("b001110???????????011?????1010111")
def VSLIDEDOWN_VX   = BitPat("b001111???????????100?????1010111")
def VSLIDEDOWN_VI   = BitPat("b001111???????????011?????1010111")
def VSLIDE1UP_VX    = BitPat("b001110???????????110?????1010111")
def VSLIDE1UP_VF    = BitPat("b001110???????????101?????1010111")
def VSLIDE1DOWN_VX  = BitPat("b001111???????????110?????1010111")
def VSLIDE1DOWN_VF  = BitPat("b001111???????????101?????1010111")
def VRGATHER_VV     = BitPat("b001100???????????000?????1010111")
def VRGATHEREI16_VV = BitPat("b001110???????????000?????1010111")
def VRGATHER_VX     = BitPat("b001100???????????100?????1010111")
def VRGATHER_VI     = BitPat("b001100???????????011?????1010111")
def VCOMPRESS_VM    = BitPat("b0101111??????????010?????1010111")
def VMV1R_V         = BitPat("b1001111?????00000011?????1010111")
def VMV2R_V         = BitPat("b1001111?????00001011?????1010111")
def VMV4R_V         = BitPat("b1001111?????00011011?????1010111")
def VMV8R_V         = BitPat("b1001111?????00111011?????1010111")


val vmv_x_s          = (funct6==="b010000".U) && (funct3 === "b010".U) && (vs1_index === "b00000".U)
val vmv_s_x          = (funct6==="b010000".U) && (funct3 === "b110".U) && (vs2_index === "b00000".U)
val vfmv_f_s         = (funct6==="b010000".U) && (funct3 === "b001".U) && (vs1_index === "b00000".U)
val vfmv_s_f         = (funct6==="b010000".U) && (funct3 === "b101".U) && (vs2_index === "b00000".U)
val vslideup_vx      = (funct6==="b001110".U) && (funct3 === "b100".U)
val vslideup_vi      = (funct6==="b001110".U) && (funct3 === "b011".U)
val vslidedn_vx      = (funct6==="b001111".U) && (funct3 === "b100".U)
val vslidedn_vi      = (funct6==="b001111".U) && (funct3 === "b011".U)
val vslide1up_vx     = (funct6==="b001110".U) && (funct3 === "b110".U)
val vfslide1up_vf    = (funct6==="b001110".U) && (funct3 === "b101".U)
val vslide1dn_vx     = (funct6==="b001111".U) && (funct3 === "b110".U)
val vfslide1dn_vf    = (funct6==="b001111".U) && (funct3 === "b101".U)
val vrgather_vv      = (funct6==="b001100".U) && (funct3 === "b000".U)
val vrgatherei16_vv  = (funct6==="b001110".U) && (funct3 === "b000".U)
val vrgather_vx      = (funct6==="b001100".U) && (funct3 === "b100".U)
val vrgather_vi      = (funct6==="b001100".U) && (funct3 === "b011".U)
val vcompress_vm     = (funct6==="b010111".U) && (funct3 === "b010".U)
val vmvr_v           = (funct6==="b100111".U) && (funct3 === "b011".U) 

val vmv = vmv_s_x  ||
          vfmv_s_f  

val vm_instr = vslideup_vx     ||
               vslideup_vi     ||
               vslidedn_vx     ||
               vslidedn_vi     ||
               vslide1up_vx    || 
               vfslide1up_vf   || 
               vslide1dn_vx    || 
               vfslide1dn_vf   || 
               vrgather_vv     || 
               vrgatherei16_vv || 
               vrgather_vx     || 
               vrgather_vi        

val vslideup  = vslideup_vx  || vslideup_vi 
val vslidedn  = vslidedn_vx  || vslidedn_vi
val vslide    = vslideup     || vslidedn
val vslide1up = vslide1up_vx || vfslide1up_vf
val vslide1dn = vslide1dn_vx || vfslide1dn_vf
val vslide1   = vslide1up    || vslide1dn
val vrgather  = vrgather_vv || vrgatherei16_vv || vrgather_vx || vrgather_vi
val vrgather_vxi = vrgather_vx || vrgather_vi
val vs1_bits = Cat(vs1.reverse) >> (ele_cnt * expdIdx)
val vs1_byte_strb = Wire(Vec(vlenb, UInt(1.W))) 
val slice_byte_cnt = Wire(Vec(NLanes, UInt(4.W)))
val slice_data = Wire(Vec(NLanes, UInt(LaneWidth.W)))
val cmprs_slice_data = Wire(Vec(NLanes, UInt(VLEN.W)))
val slice_byte_shift  = Wire(Vec(NLanes+1, UInt(vlenbWidth.W)))
val cmprs_data = Wire(Vec((NLanes+1), UInt(VLEN.W)))
val cmprs_vd = Wire(UInt(VLEN.W))
val compress_busy = RegInit(false.B)
val vslidedn_busy = RegInit(false.B)
val compress_busy_valid = RegNext(compress_busy)
val vslidedn_busy_valid = RegNext(vslidedn_busy)

val old_cmprs_byte_cnt = RegInit(0.U(vlenbWidth.W))
val old_cmprs_data = RegInit(0.U(VLEN.W))

when (vcompress_vm && fire) {
  when ((old_cmprs_byte_cnt + slice_byte_shift(NLanes)) < vlenb.U(vlenbWidth.W)) {
    when (vlRemainBytes > vlenb.U) {
      old_cmprs_byte_cnt := old_cmprs_byte_cnt + slice_byte_shift(NLanes)
    } .otherwise {
      old_cmprs_byte_cnt := 0.U 
    }
  } .otherwise {
    old_cmprs_byte_cnt := old_cmprs_byte_cnt + slice_byte_shift(NLanes) - vlenb.U(vlenbWidth.W)
  }
}

when (vcompress_vm && fire) {
  when ((old_cmprs_byte_cnt + slice_byte_shift(NLanes)) < vlenb.U(vlenbWidth.W)) {
    when (vlRemainBytes > vlenb.U) {
      old_cmprs_data := (cmprs_data(NLanes) << Cat(old_cmprs_byte_cnt, 0.U(3.W))) | old_cmprs_data 
    } .otherwise {
      old_cmprs_data := 0.U 
    }
  } .otherwise {
    old_cmprs_data := cmprs_data(NLanes) >> Cat((vlenb.U(vlenbWidth.W) - old_cmprs_byte_cnt), 0.U(3.W))
  }
}

for (i <-0 until vlenb) { 
  when (i.U < vlRemainBytes) {
    vs1_byte_strb(i) := vs1_bits(i)
    when (vsew === 1.U(3.W)) {
      vs1_byte_strb(i) := vs1_bits(i/2)
    } .elsewhen (vsew === 2.U(3.W)) { 
      vs1_byte_strb(i) := vs1_bits(i/4)
    } .elsewhen (vsew === 3.U(3.W)){ 
      vs1_byte_strb(i) := vs1_bits(i/8)
    }
  } .otherwise {
    vs1_byte_strb(i) := 0.U 
  }
}
 
val slices = Seq.fill(NLanes)(Module(new VCompressSlice))
for (i <- 0 until NLanes) {
  slices(i).io.fire := vcompress_vm && fire
  slices(i).io.vs1  := vs1_byte_strb.slice(i*NByteLane, (i+1)*NByteLane) 
  slices(i).io.vs2  := vs2(i) 
  slice_data(i)     := Cat(slices(i).io.slice_data.reverse) 
  slice_byte_cnt(i) := slices(i).io.slice_byte_cnt
}

slice_byte_shift(0) := 0.U(vlenbWidth.W)
for (i <- 0 until NLanes) {
  slice_byte_shift(i+1) := slice_byte_cnt(i) + slice_byte_shift(i) 
}

for (i <- 0 until NLanes) {
  cmprs_slice_data(i) := Cat(0.U((VLEN-LaneWidth).W), slice_data(i)) << 
                         Cat(slice_byte_shift(i), 0.U(3.W))
}

cmprs_data(0) := 0.U(VLEN.W)
for (i <- 0 until NLanes) {
  cmprs_data(i+1) := cmprs_data(i) | cmprs_slice_data(i) 
}

val output_data_cnt_w = Wire(UInt(4.W))
val output_data_cnt_r = RegNext(output_data_cnt_w, 0.U)
val lmul = RegEnable(Mux(vmv, 1.U, expdLen), fire && (expdIdx===0.U))
val vslide_offset     = Mux(uop.ctrl.vx, rs1, Cat(0.U((xLen-5).W),imme))
val vslide_ele_offset = Mux(vslide, vslide_offset, 1.U(xLen.W)) 
val vslide_byte_offset = vslide_ele_offset << vsew
val vslide_bits_offset = Cat(vslide_byte_offset, 0.U(3.W)) 
val vslide_vd = Wire(UInt(VLEN.W))
val vd_mask = (~0.U(VLEN.W))
val vs1_lmul = Reg(Vec(8, UInt(VLEN.W)))
val vs2_lmul = Reg(Vec(8, UInt(VLEN.W)))
val vd_lmul = Reg(Vec(8, UInt(VLEN.W)))
val uop_lmul = Reg(Vec(8, new(VExpdUOp)))
val vslideup_vs2_index = RegInit(0.U(3.W))
val vslideup_vs2_index_en = (expdIdx >= (vslide_byte_offset/vlenb.U(vlenbWidth.W))) && 
                            ((vslide_byte_offset/vlenb.U(vlenbWidth.W)) =/= 0.U(3.W))
val vslideup_vs2_left = Mux(((vslide_byte_offset/vlenb.U(vlenbWidth.W)) === 0.U(3.W)), Cat(vs2.reverse), 
                        vs2_lmul(vslideup_vs2_index))
val vslideup_right = Mux(((vslide_byte_offset/vlenb.U(vlenbWidth.W)) === 0.U(3.W)), vs2_lmul(expdIdx-1.U(3.W)),
                         vs2_lmul(vslideup_vs2_index-1.U(3.W)))
val vslideup_vs2_right = Mux((expdIdx === (vslide_byte_offset/vlenb.U(vlenbWidth.W))), 
    Mux((vslide1up || vmv_s_x || vfmv_s_f), Cat(0.U((VLEN-xLen).W), rs1), Cat(old_vd.reverse)), vslideup_right)
val vslideup_vs2_left_shift = vslideup_vs2_left << vslide_bits_offset%VLEN.U(bVL.W)
val vmv_left = Cat(old_vd.reverse) & (vd_mask << vslide_bits_offset%VLEN.U)
val vslideup_vs2_right_shift = Mux((expdIdx === (vslide_byte_offset/vlenb.U(vlenbWidth.W))), vslideup_vs2_right & (vd_mask >> (VLEN.U(bVL.W) - vslide_bits_offset%VLEN.U(bVL.W))), 
                                  vslideup_vs2_right >> (VLEN.U(bVL.W) - vslide_bits_offset%VLEN.U(bVL.W)))
val vlmul_bytes = (1.U<<vlmul)*vlenb.U
val vslidedn_flush = RegNext((vslidedn || vslide1dn) && expdEnd && (expdIdx =/= 0.U) && fire && (vslide_byte_offset <= vlmul_bytes))
val vslidedn_flush_expdIdx = RegNext(expdIdx)

val vlRemainBytes_reg = RegEnable(vlRemainBytes, 0.U, fire && (expdIdx === 0.U))
val vlRemainBytes_output = Mux(fire && (expdIdx === 0.U), vlRemainBytes, 
  Mux((vlRemainBytes_reg > vlenb.U*output_data_cnt_w), vlRemainBytes_reg-vlenb.U*output_data_cnt_w, 0.U))
val vslidedn_rs1_shift = Wire(UInt(vlenbWidth.W))
val vslide1dn_busy = RegInit(false.B)

output_data_cnt_w := output_data_cnt_r
when (io.out.valid && io.out.ready) {
  when ((output_data_cnt_r +1.U)  === lmul) {
    output_data_cnt_w := 0.U
  } .otherwise {
    output_data_cnt_w := output_data_cnt_r + 1.U
  }
}

when (vslide1dn && fire && expdEnd && (expdLen =/= 1.U)) {
  vslide1dn_busy := true.B
} .otherwise {
  vslide1dn_busy := false.B
}

vslidedn_rs1_shift := 0.U 
when (vslide1dn && fire && (expdIdx === 0.U) && expdEnd) {
  vslidedn_rs1_shift := vlRemainBytes - vsew_bytes
} .elsewhen (((vslide1dn && fire) || vslide1dn_busy) && ((output_data_cnt_w +1.U) === (vlRemainBytes_reg/(vlenb.U)) + (vlRemainBytes_reg%(vlenb.U)).orR)) {
  vslidedn_rs1_shift := vlRemainBytes_output - vsew_bytes
}  

val vslide1dn_rs1 = Wire(UInt(VLEN.W))

vslide1dn_rs1 := 0.U
when (vslide1dn && fire && (expdIdx === 0.U) && expdEnd) {
  vslide1dn_rs1 := Cat(rs1, 0.U((VLEN-xLen).W)) << (xLen.U - vsew_bits) >> Cat(((vlenb.U) - vlRemainBytes), 0.U(3.W))
} .elsewhen (((vslide1dn && fire) || vslide1dn_busy) && ((output_data_cnt_w +1.U) === (vlRemainBytes_reg/(vlenb.U)) + (vlRemainBytes_reg%(vlenb.U)).orR)) {
  vslide1dn_rs1 := Cat(rs1, 0.U((VLEN-xLen).W)) << (xLen.U - vsew_bits) >> Cat(((vlenb.U) - vlRemainBytes_output), 0.U(3.W))
}

val vslide1dn_mask_bits = Wire(UInt(VLEN.W)) 

vslide1dn_mask_bits := vd_mask
when (vslide1dn && fire && (expdIdx === 0.U) && expdEnd) {
  vslide1dn_mask_bits := vd_mask >> (VLEN.U - Cat(vslidedn_rs1_shift, 0.U(3.W)))
} .elsewhen (((vslide1dn && fire) || vslide1dn_busy) && ((output_data_cnt_w +1.U) === (vlRemainBytes_reg/(vlenb.U)) + (vlRemainBytes_reg%(vlenb.U)).orR)) {
  vslide1dn_mask_bits := vd_mask >> (VLEN.U - Cat(vslidedn_rs1_shift, 0.U(3.W)))
}

when (fire) {
  vs1_lmul(expdIdx) := Cat(vs1.reverse)
  vs2_lmul(expdIdx) := Cat(vs2.reverse)
  vd_lmul(expdIdx) := Cat(old_vd.reverse)
  uop_lmul(expdIdx) := io.in.bits.uop
}

when (fire) {
  when (expdEnd) {
    vslideup_vs2_index := 0.U(3.W)
  } .elsewhen (vslideup_vs2_index_en) {
    vslideup_vs2_index := vslideup_vs2_index +1.U(3.W)
  }
}

vslide_vd := 0.U(VLEN.W)
when (fire) {
  when (vslideup || vslide1up) {
    when (expdIdx < (vslide_byte_offset/vlenb.U(vlenbWidth.W))) {
      vslide_vd := Cat(old_vd.reverse)
    }.otherwise {
      vslide_vd := vslideup_vs2_left_shift | vslideup_vs2_right_shift
    }
  } .elsewhen (vslidedn || vslide1dn) {
      when ((expdIdx === 0.U) && expdEnd) {
        vslide_vd := vslide1dn_rs1 |  
                   (Cat(vs2.reverse) >> vslide_bits_offset) 
     
      } .elsewhen (expdIdx > (vslide_byte_offset/vlenb.U(vlenbWidth.W))) {
        vslide_vd := (Cat(vs2.reverse) << (VLEN.U(bVL.W) - vslide_bits_offset%VLEN.U(bVL.W))) |
                     (vs2_lmul(expdIdx-1.U(3.W)) >> vslide_bits_offset%VLEN.U(bVL.W))
      }
  } .elsewhen (vmv_s_x || vfmv_s_f) {
     vslide_vd := vmv_left | vslideup_vs2_right_shift
  } 
} .elsewhen (vslidedn_flush) {
      vslide_vd := vslide1dn_rs1 |  
                   vs2_lmul(vslidedn_flush_expdIdx) >> vslide_bits_offset%VLEN.U(bVL.W) 
}

def gen_vrgather_byte_sel(vs1: UInt) = {
  val vrgather_byte_sel = Wire(Vec(vlenb, UInt(16.W)))
  for (i<-0 until vlenb) {
    vrgather_byte_sel(i) := 0.U(16.W) 
    when (vsew === 0.U(3.W)) {
      vrgather_byte_sel(i) := Cat(0.U(8.W), vs1((i+1)*8 -1,i*8)) 
    } .elsewhen (vsew === 1.U(3.W)) {
      vrgather_byte_sel(i) := Cat(vs1((i/2+1)*16-1,i/2*16), 0.U(1.W)) +i.U(vlenbWidth.W)%2.U 
    } .elsewhen (vsew === 2.U(3.W)) {
      vrgather_byte_sel(i) := Cat(vs1((i/4+1)*32-1,i/4*32), 0.U(2.W)) +i.U(vlenbWidth.W)%4.U 
    } .elsewhen (vsew === 3.U(3.W)) {
      vrgather_byte_sel(i) := Cat(vs1((i/8+1)*64-1,i/8*64), 0.U(3.W)) +i.U(vlenbWidth.W)%8.U 
    } 
  }
  vrgather_byte_sel
}

def gen_vrgather16_byte_sel(vs1: UInt) = {
  val vrgather_byte_sel = Wire(Vec(vlenb, UInt(16.W)))
  for (i<-0 until vlenb) {
    vrgather_byte_sel(i) := 0.U(16.W) 
    when (vsew === 0.U(3.W)) {
      vrgather_byte_sel(i) := vs1((i+1)*16 -1,i*16) 
    } .elsewhen (vsew === 1.U(3.W)) {
      vrgather_byte_sel(i) := Cat(vs1((i/2+1)*16-1,i/2*16), 0.U(1.W)) +i.U(vlenbWidth.W)%2.U 
    } .elsewhen (vsew === 2.U(3.W)) {
      vrgather_byte_sel(i) := Cat(vs1((i/2+1)*16-1,i/2*16), 0.U(2.W)) +i.U(vlenbWidth.W)%4.U 
    } .elsewhen (vsew === 3.U(3.W)) {
      vrgather_byte_sel(i) := Cat(vs1((i/2+1)*16-1,i/2*16), 0.U(3.W)) +i.U(vlenbWidth.W)%8.U 
    } 
  }
  vrgather_byte_sel
}

val vrgather_vs1_cnt = RegInit(0.U(3.W))
val vrgather_vs2_cnt = RegInit(0.U(3.W))
val vrgather_vv_busy = RegInit(false.B)
val vs2_bytes_min = vrgather_vs2_cnt*vlenb.U
val vs2_bytes_max = (vrgather_vs2_cnt+1.U)*vlenb.U
val vs1_mux = Mux(((vrgather_vv || vrgatherei16_vv) && fire && (expdIdx === 0.U)), Cat(vs1.reverse), vs1_lmul(vrgather_vs1_cnt)) 
val vrgathervv_byte_sel = gen_vrgather_byte_sel(vs1_mux)
val vs1_vrgather16 = WireDefault(0.U((2*VLEN).W))
val vrgather16_byte_sel = gen_vrgather16_byte_sel(vs1_vrgather16)
val vrgather_byte_sel = Mux(vrgather_vv, vrgathervv_byte_sel, vrgather16_byte_sel) 
val vrgather_vd = Wire(Vec(vlenb, UInt(8.W)))
val vrgather_vd_reg = RegNext(Cat(vrgather_vd.reverse), 0.U(VLEN.W))

val sew = SewOH(vsew)  // 0:8, 1:16, 2:32, 3:64
// sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U)) 
val rs1_imm = Mux(uop.ctrl.vi, Cat(Fill(59, 0.U(1.W)), imme), rs1)
val rs1_imm_bytes = rs1_imm << vsew
val vrgather_rs1_imm = RegInit(0.U(LaneWidth.W))
val rs1_imm_repeat = Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => Fill(VLEN/n, vrgather_rs1_imm(n-1, 0))))


val vrgather_vxi_vd = VecInit(Seq.tabulate(NLanes)(i => rs1_imm_repeat((i+1)*LaneWidth-1, i*LaneWidth)))
val vrgather_vxi_valid = RegInit(false.B)
val expdLen_reg = RegInit(0.U(4.W))
val expdLen_reg_minus1 = Mux(fire & (expdIdx === 0.U), expdLen -1.U, lmul - 1.U)
val vrgather_xi_busy = RegInit(false.B)

when (vsew === 0.U) {
  vs1_vrgather16 := Cat(vs1_lmul(2.U*vrgather_vs1_cnt+1.U), vs1_lmul(2.U*vrgather_vs1_cnt))
} .elsewhen (vsew === 1.U) {
  vs1_vrgather16 := Cat(0.U(VLEN.W), vs1_lmul(vrgather_vs1_cnt))
} .elsewhen (vsew === 2.U) {
  vs1_vrgather16 := Cat(0.U(VLEN.W), vs1_lmul(vrgather_vs1_cnt/2.U) >> (VLEN/2).U *vrgather_vs1_cnt(0))
} .elsewhen (vsew === 3.U) {
  vs1_vrgather16 := Cat(0.U(VLEN.W), vs1_lmul(vrgather_vs1_cnt/4.U) >> (VLEN/4).U *vrgather_vs1_cnt(1,0))
} 

when ((vrgather || vcompress_vm) && fire) {
  expdLen_reg := expdLen 
}

when (vrgather_vxi && fire) {
  when (rs1_imm_bytes >= vlmul_bytes) {
    vrgather_rs1_imm := 0.U 
  } .elsewhen ((rs1_imm_bytes < vlmul_bytes) && (expdIdx === rs1_imm_bytes/vlenb.U)) {
    vrgather_rs1_imm := (Cat(vs2.reverse) >> Cat(rs1_imm_bytes%vlenb.U, 0.U(3.W)))(LaneWidth-1,0)
  }
} 

when (vrgather_vxi && fire && ((rs1_imm_bytes >= vlmul_bytes) || ((rs1_imm_bytes < vlmul_bytes) && (expdIdx === rs1_imm_bytes/vlenb.U)))) {
  vrgather_vxi_valid := true.B
} .elsewhen ((output_data_cnt_w === (lmul -1.U)) && (io.out.valid && io.out.ready)) {
  vrgather_vxi_valid := false.B
}

when ((output_data_cnt_w === (lmul -1.U)) && (io.out.valid && io.out.ready)) {
  vrgather_xi_busy := false.B
} .elsewhen (vrgather_vxi && fire && expdEnd) {
  vrgather_xi_busy := true.B
}

when ((vrgather_vs1_cnt === expdLen_reg_minus1) && (vrgather_vs2_cnt === expdLen_reg_minus1)) {
  vrgather_vv_busy := false.B
} .elsewhen ((vrgather_vv || vrgatherei16_vv) && fire && expdEnd) {
  vrgather_vv_busy := true.B
}

when ((vrgather_vv && fire) || vrgather_vv_busy) {
  when(vrgather_vs2_cnt === expdLen_reg_minus1) {
    vrgather_vs2_cnt := 0.U(3.W)
  } .otherwise {
    vrgather_vs2_cnt := vrgather_vs2_cnt + 1.U
  }
}

when (((vrgather_vv && fire) || vrgather_vv_busy) && (vrgather_vs2_cnt === expdLen_reg_minus1)) {
  when (vrgather_vs1_cnt === expdLen_reg_minus1) {
    vrgather_vs1_cnt := 0.U(3.W)
  } .otherwise {
    vrgather_vs1_cnt := vrgather_vs1_cnt + 1.U
  }
}

for (i<-0 until vlenb) {
  vrgather_vd(i) := 0.U
}

val vs2_mux = Mux(vrgather_vv_busy, vs2_lmul(vrgather_vs2_cnt), Cat(vs2.reverse))
val vs2_lmul_byte = VecInit(Seq.tabulate(vlenb)(i => vs2_mux(8*i+7, 8*i)))

when ((vrgather_vv && fire) || (vrgather_vv_busy)) {
  for (i<-0 until vlenb) {
    vrgather_vd(i) := vrgather_vd_reg((i+1)*8-1,i*8)
    when ((vrgather_byte_sel(i) >= vs2_bytes_min) && (vrgather_byte_sel(i) < vs2_bytes_max)) {
      vrgather_vd(i) := vs2_lmul_byte(vrgather_byte_sel(i.U-vs2_bytes_min))
    } 
  }
}

val cmprs_valid = (((old_cmprs_byte_cnt + slice_byte_shift(NLanes)) >= vlenb.U) || (vlRemainBytes <= vlenb.U)) && vcompress_vm && fire
val vslidedn_valid = ((expdIdx > (vslide_byte_offset/vlenb.U(vlenbWidth.W))) || ((expdIdx === 0.U) && expdEnd) || (vslide_byte_offset > vlmul_bytes)) && (vslidedn || vslide1dn) && fire
val cross_lane_vd  = Wire(UInt(VLEN.W)) 
val old_vd_reg     = Reg(UInt(VLEN.W)) 
val cross_lane_valid  = Wire(Bool()) 
val cross_lane_valid_reg  = RegNext(cross_lane_valid, false.B) 
val vmask_reg = RegEnable(Cat(vmask.reverse), fire)
val vm_reg = RegEnable(vm, fire)
val vmask_bits = Mux(fire, Cat(vmask.reverse), vmask_reg) >> (ele_cnt * output_data_cnt_w)
val vmask_vd_bytes = Wire(Vec(vlenb, UInt(8.W)))
val vmask_vd_bits = Cat(vmask_vd_bytes.reverse)

val vmask_old_vd = Mux((output_data_cnt_w === expdIdx) && fire, Cat(old_vd.reverse), vd_lmul(output_data_cnt_w)) & (~vmask_vd_bits)
val vmask_ones_vd = vd_mask & (~vmask_vd_bits)
val vmask_vd = Mux(ma, vmask_ones_vd, vmask_old_vd)

val compress_tail = Wire(UInt(vlenbWidth.W)) 
val non_compress_tail = Mux(vmv & fire, vlenb.U-vsew_bytes, Mux((vlRemainBytes_output >= vlenb.U), 0.U, vlenb.U-vlRemainBytes_output))
val tail_bytes = Wire(UInt(vlenbWidth.W)) 

compress_tail := 0.U 
when ((vcompress_vm && fire)  || compress_busy) {
  when ((old_cmprs_byte_cnt + slice_byte_shift(NLanes)) <= vlenb.U(vlenbWidth.W)) {
    when (vlRemainBytes <= vlenb.U) {
      compress_tail := vlenb.U - (old_cmprs_byte_cnt + slice_byte_shift(NLanes)) 
    } 
  }
}

when ((vcompress_vm && fire) || compress_busy) {
  tail_bytes := compress_tail
} .otherwise {
  tail_bytes := non_compress_tail
} 

when (((output_data_cnt_r +2.U)  === lmul) && io.out.valid && io.out.ready) {
  compress_busy := false.B
} .elsewhen (vcompress_vm && fire && expdEnd && (expdIdx =/= 0.U) && ((output_data_cnt_r +2.U) <= lmul)) {
  compress_busy := true.B
}

when (((output_data_cnt_r +2.U)  === lmul) && io.out.valid && io.out.ready) {
  vslidedn_busy := false.B
} .elsewhen (vslidedn_flush && ((output_data_cnt_r +2.U) <= lmul)) {
  vslidedn_busy := true.B
}

val tail_bits = Cat(tail_bytes, 0.U(3.W))
val vmask_tail_bits = vd_mask >> tail_bits 
val old_vd_tail = Mux((output_data_cnt_w === expdIdx) && fire, Cat(old_vd.reverse), vd_lmul(output_data_cnt_w))  
val tail_old_vd = Mux((output_data_cnt_w === expdIdx) && fire, Cat(old_vd.reverse), vd_lmul(output_data_cnt_w)) & (~vmask_tail_bits)  
val tail_ones_vd = ~vmask_tail_bits
val tail_vd = Mux(ta, tail_ones_vd, tail_old_vd)

val cross_lane_vd_mask_tail = Mux(vmvr_v && fire, cross_lane_vd, 
                             (Mux((vslide1dn_busy || (vslide1dn && fire)), ((cross_lane_vd & vslide1dn_mask_bits) | vslide1dn_rs1), cross_lane_vd) & vmask_vd_bits) 
                              | vmask_vd | tail_vd) 
val cross_lane_vd_reg = RegEnable(VecInit(Seq.tabulate(NLanes)(i => cross_lane_vd_mask_tail((i+1)*LaneWidth-1, i*LaneWidth))), ((cross_lane_valid && !cross_lane_valid_reg) || (io.out.valid && io.out.ready))) 

when (compress_busy_valid) {
  cmprs_vd := 0.U
} .otherwise {
  cmprs_vd := (old_vd_tail & (vd_mask << Cat(((old_cmprs_byte_cnt + slice_byte_shift(NLanes))), 0.U(3.W)))) | (cmprs_data(NLanes) << Cat(old_cmprs_byte_cnt, 0.U(3.W))) | old_cmprs_data 
}

cross_lane_valid := cross_lane_valid_reg
when ((vcompress_vm && cmprs_valid) || compress_busy) { 
  cross_lane_valid := true.B
} .elsewhen ((vslideup || vslide1up || vmv_s_x || vfmv_s_f) && fire) { 
  cross_lane_valid := true.B
} .elsewhen (((vslidedn || vslide1dn) && vslidedn_valid) || vslidedn_flush || vslidedn_busy) {
  cross_lane_valid := true.B
} .elsewhen (((vrgather_vv && fire) || vrgather_vv_busy) && (vrgather_vs2_cnt === expdLen_reg_minus1)) { 
  cross_lane_valid := true.B
} .elsewhen (vrgather_vxi_valid) {
  cross_lane_valid := true.B
} .elsewhen (vmvr_v && fire) {
  cross_lane_valid := true.B
} .elsewhen ((vmv_x_s || vfmv_f_s) && fire) {
  cross_lane_valid := true.B
} .elsewhen (io.out.valid && io.out.ready) {
  cross_lane_valid := false.B
}

cross_lane_vd := Cat(cross_lane_vd_reg.reverse)

when ((vcompress_vm && cmprs_valid) || compress_busy) { 
  cross_lane_vd := cmprs_vd
} .elsewhen ((vslideup || vslide1up || vmv_s_x || vfmv_s_f) && fire) { 
   cross_lane_vd := vslide_vd
} .elsewhen (((vslidedn || vslide1dn) && vslidedn_valid) || vslidedn_flush || vslidedn_busy) {
  cross_lane_vd := vslide_vd
} .elsewhen (((vrgather_vv && fire) || vrgather_vv_busy) && (vrgather_vs2_cnt === expdLen_reg_minus1)) { 
  cross_lane_vd := Cat(vrgather_vd.reverse)
} .elsewhen (vrgather_vxi_valid) {
  cross_lane_vd := Cat(vrgather_vxi_vd.reverse)
} .elsewhen (vmvr_v && fire) {
  cross_lane_vd := Cat(vs2.reverse)
}

val vm_mask = Mux(fire, vm, vm_reg)
for (i<-0 until vlenb) {
  vmask_vd_bytes(i) := "hff".U
  when ((!vm_mask && !vmask_bits(i.U/vsew_bytes_mux)) || (i.U >= vlRemainBytes_output)) {
    vmask_vd_bytes(i) := 0.U 
  }
}

when (fire) {
  old_vd_reg := Cat(old_vd.reverse)
}

val rd = RegInit(0.U(xLen.W))

when ((vmv_x_s || vfmv_f_s) && fire) {
  rd := Mux1H(sew.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(LaneWidth-n, vs2(0)(n-1)), vs2(0)(n-1,0))) :+ vs2(0))
}


//--------- Ready & valid ---------
io.in.ready := (!io.in.valid || io.out.ready) && !compress_busy && !vslidedn_flush && !vslidedn_busy && !vrgather_xi_busy && !vrgather_vxi_valid && !vrgather_vv_busy
io.out.valid := cross_lane_valid_reg
io.out.bits.vd := cross_lane_vd_reg
when (io.out.bits.uop.ctrl.rdVal) { io.out.bits.vd(0) := rd }

io.out.bits.uop := uop_lmul(output_data_cnt_r)
// io.out.bits.rd := rd

// temp!!
io.out.bits.fflags := 0.U


}

object VerilogPer extends App {
  println("Generating the VPU CrossLane hardware")
  emitVerilog(new Permutation(), Array("--target-dir", "generated"))
}



package darecreek

import chisel3._
import chisel3.util._
import darecreek.exu.fu.alu._
import darecreek.exu.fp._
import darecreek.ctrl.decode.VInstructions._
import VAluFunct6._

class Reduction extends Module {
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
val vstartRemainBytes = vstartRemain << vsew;
val vlRemain = io.in.bits.vlRemain
val vlRemainBytes = io.in.bits.vlRemain << vsew
val vsew_bytes = 1.U << vsew;
val fire = io.in.fire

def VREDSUM_VS     = BitPat("b000000???????????010?????1010111")
def VREDMAX_VS     = BitPat("b000111???????????010?????1010111")
def VREDMAXU_VS    = BitPat("b000110???????????010?????1010111")
def VREDMIN_VS     = BitPat("b000101???????????010?????1010111")
def VREDMINU_VS    = BitPat("b000100???????????010?????1010111")
def VREDAND_VS     = BitPat("b000001???????????010?????1010111")
def VREDOR_VS      = BitPat("b000010???????????010?????1010111")
def VREDXOR_VS     = BitPat("b000011???????????010?????1010111")
def VWREDSUM_VS    = BitPat("b110001???????????000?????1010111")
def VWREDSUMU_VS   = BitPat("b110000???????????000?????1010111")
def VFREDOSUM_VS   = BitPat("b000011???????????001?????1010111")
def VFREDSUM_VS    = BitPat("b000001???????????001?????1010111")
def VFREDMAX_VS    = BitPat("b000111???????????001?????1010111")
def VFREDMIN_VS    = BitPat("b000101???????????001?????1010111")
def VFWREDOSUM_VS  = BitPat("b110011???????????001?????1010111")
def VFWREDSUM_VS   = BitPat("b110001???????????001?????1010111")
 

val vredsum_vs     = (funct6==="b000000".U) && (funct3 === "b010".U)
val vredmax_vs     = (funct6==="b000111".U) && (funct3 === "b010".U)
val vredmaxu_vs    = (funct6==="b000110".U) && (funct3 === "b010".U)
val vredmin_vs     = (funct6==="b000101".U) && (funct3 === "b010".U)
val vredminu_vs    = (funct6==="b000100".U) && (funct3 === "b010".U)
val vredand_vs     = (funct6==="b000001".U) && (funct3 === "b010".U)
val vredor_vs      = (funct6==="b000010".U) && (funct3 === "b010".U)
val vredxor_vs     = (funct6==="b000011".U) && (funct3 === "b010".U)
val vwredsum_vs    = (funct6==="b110001".U) && (funct3 === "b000".U)
val vwredsumu_vs   = (funct6==="b110000".U) && (funct3 === "b000".U)
val vfredosum_vs   = (funct6==="b000011".U) && (funct3 === "b001".U)
val vfredusum_vs   = (funct6==="b000001".U) && (funct3 === "b001".U)
val vfredmax_vs    = (funct6==="b000111".U) && (funct3 === "b001".U)
val vfredmin_vs    = (funct6==="b000101".U) && (funct3 === "b001".U)
val vfwredosum_vs  = (funct6==="b110011".U) && (funct3 === "b001".U)
val vfwredusum_vs  = (funct6==="b110001".U) && (funct3 === "b001".U)

val vsew_bits = RegEnable(8.U << Mux(vwredsum_vs || vwredsumu_vs, (vsew+1.U), vsew), 0.U, fire);

val alu_red = vredsum_vs   ||
              vredmax_vs   ||
              vredmaxu_vs  ||
              vredmin_vs   ||
              vredminu_vs  ||
              vredand_vs   ||
              vredor_vs    ||
              vredxor_vs   ||
              vwredsum_vs  ||
              vwredsumu_vs 

val fpu_red = vfredosum_vs  ||
              vfredusum_vs  ||
              vfredmax_vs   ||
              vfredmin_vs   ||
              vfwredosum_vs ||
              vfwredusum_vs 

val idle :: calc_vs2 :: calc_vs1 :: Nil = Enum(3)

val red_state = RegInit(idle)

val vs2_cnt = RegInit(0.U(vlenbWidth.W))
val widen_vs2_cnt = RegInit(0.U(vlenbWidth.W))
val vs2_rnd = RegInit(0.U(vlenbWidth.W))
val vwredsum_rnd_hi = RegInit(0.U(vlenbWidth.W))
val vwredsum_rnd_lo = RegInit(0.U(vlenbWidth.W))

when (fire) {
  when (vfwredosum_vs) {
    vs2_rnd := vlenb.U/vsew_bytes
  } .elsewhen (vfredosum_vs) {
    vs2_rnd := vlenb.U/vsew_bytes-1.U
  } .elsewhen (vwredsum_vs || vwredsumu_vs) {
    vs2_rnd := (vlenbWidth-2).U - vsew 
  } .otherwise {
    vs2_rnd := (vlenbWidth-1).U -vsew
  } 
}
       
when (fire) {
vwredsum_rnd_hi := 3.U 
vwredsum_rnd_lo := 1.U
}

val alu_busy = RegInit(false.B)
val alu_out_valid = Wire(Bool())
val alu_out_ready = Wire(Bool())
alu_out_ready := true.B
val alu_valid = RegInit(false.B)
val alu_in_valid = alu_valid || ((red_state === calc_vs2) && alu_out_valid) 
val alu_in_ready = Wire(Bool())

val fpu_busy = RegInit(false.B)
val fpu_out_valid = Wire(Bool())
val fpu_out_ready = Wire(Bool())
fpu_out_ready := true.B
val fpu_valid = RegInit(false.B)
val fpu_in_valid = fpu_valid || ((red_state === calc_vs2) && fpu_out_valid) 
val fpu_in_ready = Wire(Bool())

val alu_out = Wire(Vec(NLanes/2, new LaneFUOutput)) 
val fpu_out = Wire(Vec(NLanes/2, new LaneFUOutput))
val red_in = Wire(Vec(NLanes/2, new LaneFUInput)) 
val red_out = Mux(alu_busy, alu_out, fpu_out) 
val red_out_vd = Wire(Vec(NLanes/2, UInt(LaneWidth.W))) 
val vd_mask = (~0.U(VLEN.W))
val vd_mask_half = (~0.U((VLEN/2).W))
val vd_mask_lane = (~0.U((LaneWidth).W))
val red_vd_bits = Cat(0.U((VLEN/2).W), Cat(red_out_vd.reverse))  

val red_busy = alu_busy || fpu_busy
val red_in_valid = alu_in_valid || fpu_in_valid
val red_in_ready = alu_in_ready || fpu_in_ready
val red_out_valid = alu_out_valid || fpu_out_valid
val red_out_ready = alu_out_ready || fpu_out_ready

val old_vd_bits = RegInit(0.U(VLEN.W))
val red_vd_tail_one = (vd_mask << vsew_bits) | (red_vd_bits & (vd_mask >> (VLEN.U - vsew_bits)))
val red_vd_tail_vd = (old_vd_bits & (vd_mask << vsew_bits)) | (red_vd_bits & (vd_mask >> (VLEN.U - vsew_bits)))

val red_vd = Mux(ta, red_vd_tail_one, red_vd_tail_vd)

for (i <- 0 until NLanes/2) {
  red_out_vd(i) := red_out(i).vd
}

switch (red_state) {
  is (idle) {
    when ((alu_red || fpu_red) && fire) {
      red_state := calc_vs2
    }
  }

  is (calc_vs2) {
    when ((vs2_cnt === (vs2_rnd -1.U)) && (red_out_valid && red_out_ready)) { 
      red_state := calc_vs1
    }
  }

  is (calc_vs1) {
    when (red_out_valid && red_out_ready) { 
      red_state := idle
    }
  }
}

val expdIdxZero = RegInit(false.B)
val output_en = RegInit(false.B)

when (fire) {
  when (expdEnd) {
    output_en := true.B
  } .otherwise {
    output_en := false.B
  }
}

when (fire) {
  when (expdIdx===0.U) {
    expdIdxZero := true.B
  } .otherwise {
    expdIdxZero := false.B
  }
}

when (fire) {
  old_vd_bits := Cat(old_vd.reverse)
}

when (alu_red && fire) {
  alu_busy := true.B
} .elsewhen ((red_state === calc_vs1) && red_out_valid && red_out_ready) {
  alu_busy := false.B
}

alu_valid := false.B
when (alu_red && fire) {
  alu_valid := true.B
} 

when (fpu_red && fire) {
  fpu_busy := true.B
} .elsewhen ((red_state === calc_vs1) && red_out_valid && red_out_ready) {
  fpu_busy := false.B
}

fpu_valid := false.B
when (fpu_red && fire) {
  fpu_valid := true.B
}

val vwredsumu_busy = RegInit(false.B)
val vwredsum_busy = RegInit(false.B)

when ((red_state === calc_vs2) && (vwredsum_busy || vwredsumu_busy)) {
  widen_vs2_cnt := widen_vs2_cnt +1.U
} .otherwise {
  widen_vs2_cnt := 0.U
}

when ((red_state === calc_vs2) && red_out_valid && red_out_ready) {
  when ((widen_vs2_cnt === 1.U) || (widen_vs2_cnt === 2.U) || (widen_vs2_cnt === 3.U)) {
     vs2_cnt := 0.U
  } .elsewhen (vs2_cnt === (vs2_rnd -1.U)) { 
     vs2_cnt := 0.U
  } .otherwise {
     vs2_cnt := vs2_cnt +1.U
  }
}

def zero(w: Int) = 0.U(w.W)
def umax(w: Int) = ~(0.U(w.W))
def smax(w: Int) = Cat(0.U(1.W), ~(0.U((w-1).W)))
def smin(w: Int) = Cat(1.U(1.W), 0.U((w-1).W))
def fmax(w: Int) = {
  w match { 
    case 32 => Cat(0.U(1.W), ~(0.U(8.W)), 0.U(23.W))
    case 64 => Cat(0.U(1.W), ~(0.U(11.W)), 0.U(52.W))
    case _ => Cat(0.U(1.W), ~(0.U((w-1).W)))
  }
}

def fmin(w: Int) = {
  w match { 
    case 32 => Cat(1.U(1.W), ~(0.U(8.W)), 0.U(23.W))
    case 64 => Cat(1.U(1.W), ~(0.U(11.W)), 0.U(52.W))
    case _ => Cat(1.U(1.W), ~(0.U((w-1).W)))
  }
}

val ele64 = Wire(UInt(64.W))
val sew = SewOH(vsew)  // 0:8, 1:16, 2:32, 3:64
// sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U)) 

ele64 := 0.U
when (fire) {
  when (vredmax_vs) {
    ele64 := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => smin(n))) 
  } .elsewhen (vredmin_vs) {
    ele64 := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => smax(n))) 
  } .elsewhen (vredminu_vs || vredand_vs) {
    ele64 := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => umax(n))) 
  } .elsewhen (vredminu_vs || vredand_vs) {
    ele64 := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => umax(n))) 
  } .elsewhen (vfredmax_vs) {
    ele64 := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => fmin(n))) 
  } .elsewhen (vfredmin_vs) {
    ele64 := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => fmax(n))) 
  } 
}

val ele_cnt = vlenb.U/vsew_bytes
val vmask_bits = Cat(vmask.reverse) >> (ele_cnt * expdIdx)

val vs2_bytes = VecInit(Seq.tabulate(vlenb)(i => Cat(vs2.reverse)((i+1)*8-1, i*8)))
val vs2m_bytes = Wire(Vec(vlenb, UInt(8.W)))
val vs2m_bits = RegEnable(Cat(vs2m_bytes.reverse), 0.U, fire)

for (i<-0 until vlenb) {
  vs2m_bytes(i) := vs2_bytes(i)
  when ((!vm && !vmask_bits(i.U/vsew_bytes)) || (i.U >= vlRemainBytes)) {
    when (vsew === 0.U) {
      vs2m_bytes(i) := ele64(7, 0) 
    } .elsewhen (vsew === 1.U) {
      vs2m_bytes(i) := ele64((i%2+1)*8-1, (i%2)*8) 
    } .elsewhen (vsew === 2.U) {
      vs2m_bytes(i) := ele64((i%4+1)*8-1, (i%4)*8) 
    } .elsewhen (vsew === 3.U) {
      vs2m_bytes(i) := ele64((i%8+1)*8-1, (i%8)*8) 
    } 
   }
}

val div_cnt = 1.U << vs2_cnt
val vs2_rnd0 = alu_valid || fpu_valid 
val vs2_rnd01 = (vs2_cnt === 0.U) && red_out_valid && red_out_ready  
val vs2_rnd1 = vs2_cnt === 1.U 
val vs2_rndx = (red_state === calc_vs2) && (vs2_cnt === (vs2_rnd -1.U)) && red_out_valid && red_out_ready  
val red_out_bits = Cat(red_out_vd.reverse)
val vs2m_bits_hi = vs2m_bits(VLEN-1, VLEN/2)
val red_out_hi = (red_out_bits & (vd_mask_half >> ((VLEN/2).U - (VLEN/2).U/div_cnt))) >> (VLEN/4).U/div_cnt
val vs2m_bits_lo = vs2m_bits(VLEN/2 -1, 0)
val red_out_lo = red_out_bits & (vd_mask_half >> ((VLEN/2).U - (VLEN/4).U/div_cnt))
val widen_sew = Mux((vwredsumu_vs || vwredsum_vs) && fire, Cat(vsew_bits, 0.U(1.W)), vsew_bits)
val vs1_zero = RegEnable(vs1(0) & (vd_mask_lane >> ((LaneWidth).U - widen_sew)), fire) 
val red_zero = RegEnable(red_out_bits(63,0), (red_state === calc_vs1) && red_out_valid && red_out_ready)
val red_vs1_zero = Mux(expdIdxZero, Cat(Fill(VLEN/2-64, 0.U), vs1_zero), Cat(Fill(VLEN/2-64, 0.U), red_zero))
val vs2_order = (Cat(vs2.reverse) >> vs2_cnt*vsew_bits) & (vd_mask >> ((VLEN).U - vsew_bits))
val red_vs1_bits = Wire(UInt((VLEN/2).W)) 
val red_vs2_bits = Wire(UInt((VLEN/2).W))
val red_vs1 = VecInit(Seq.tabulate(NLanes/2)(i => red_vs1_bits((i+1)*LaneWidth-1, i*LaneWidth)))
val red_vs2 = VecInit(Seq.tabulate(NLanes/2)(i => red_vs2_bits((i+1)*LaneWidth-1, i*LaneWidth)))
val red_uop = Reg(new VExpdUOp)
val vwredsumu_rnd0 = (vs2_rnd0 && (red_uop.ctrl.funct6 === "b110000".U) && (red_uop.ctrl.funct3 === "b010".U) && (red_uop.info.destEew === red_uop.info.vsew+1.U))
val vwredsum_rnd0  = (vs2_rnd0 && (red_uop.ctrl.funct6 === "b110001".U) && (red_uop.ctrl.funct3 === "b010".U) && (red_uop.info.destEew === red_uop.info.vsew+1.U))
val vwredsumu_rnd01 = ((widen_vs2_cnt === 1.U) && vwredsumu_busy)
val vwredsum_rnd01  = ((widen_vs2_cnt === 1.U) && vwredsum_busy)
val vwredsumu_rndx  = ((widen_vs2_cnt === 2.U) && vwredsumu_busy)
val vwredsum_rndx   = ((widen_vs2_cnt === 2.U) && vwredsum_busy)
val vwredsumu_rndxx = ((widen_vs2_cnt === 4.U) && vwredsumu_busy)
val vwredsum_rndxx  = ((widen_vs2_cnt === 4.U) && vwredsum_busy)
val vwredsum_lo = RegEnable(red_out_bits, 0.U(LaneWidth.W), (vwredsum_rndx || vwredsumu_rndx))

when (vwredsum_vs && fire) {
  vwredsum_busy := true.B
} .elsewhen ((red_state === calc_vs1) && red_out_valid && red_out_ready) {
  vwredsum_busy := false.B
}

when (vwredsumu_vs && fire) {
  vwredsumu_busy := true.B
} .elsewhen ((red_state === calc_vs1) && red_out_valid && red_out_ready) {
  vwredsumu_busy := false.B
}

when (vfredosum_vs || vfwredosum_vs) {
  when (vs2_rnd0) {
    red_vs1_bits := vs1_zero
  } .otherwise {
    red_vs1_bits := red_out_bits 
  }
  red_vs2_bits := vs2_order(VLEN/2 -1, 0)
} .otherwise {
  when (vs2_rnd0) {
    red_vs1_bits := vs2m_bits_lo 
    red_vs2_bits := vs2m_bits_hi
  } .elsewhen (vwredsumu_rndx || vwredsum_rndx) {
    red_vs1_bits := Cat(vs2m_bits_lo(95,64), vs2m_bits_lo(127,96), vs2m_bits_lo(31,0), vs2m_bits_lo(63,32)) 
    red_vs2_bits := Cat(vs2m_bits_hi(95,64), vs2m_bits_hi(127,96), vs2m_bits_hi(31,0), vs2m_bits_hi(63,32)) 
  } .elsewhen (vwredsumu_rndxx || vwredsum_rndxx) {
    red_vs1_bits := red_out_bits 
    red_vs2_bits := vwredsum_lo
  } .elsewhen (vs2_rndx) {
    red_vs1_bits := red_out_bits 
    red_vs2_bits := red_vs1_zero
  } .otherwise {
    red_vs1_bits := red_out_lo 
    red_vs2_bits := red_out_hi
  }
}

red_uop.ctrl.funct6 := red_uop.ctrl.funct6
red_uop.ctrl.funct3 := red_uop.ctrl.funct3
red_uop.info.vsew   := red_uop.info.vsew  
red_uop.info.destEew := red_uop.info.destEew  

when (fire) {
  red_uop := uop
  red_uop.expdLen := 1.U
  red_uop.expdIdx := 0.U
  red_uop.expdEnd := true.B
  when (vredsum_vs) {
    red_uop.ctrl.funct6 := "b000000".U 
    red_uop.ctrl.funct3 := "b000".U     
  } .elsewhen (vredmax_vs) {
    red_uop.ctrl.funct6 := "b000111".U 
    red_uop.ctrl.funct3 := "b000".U  
  } .elsewhen (vredmaxu_vs) { 
    red_uop.ctrl.funct6 := "b000110".U 
    red_uop.ctrl.funct3 := "b000".U  
  } .elsewhen (vredmin_vs) { 
    red_uop.ctrl.funct6 := "b000101".U 
    red_uop.ctrl.funct3 := "b000".U 
  } .elsewhen (vredminu_vs) { 
    red_uop.ctrl.funct6 := "b000100".U 
    red_uop.ctrl.funct3 := "b000".U  
  } .elsewhen (vredand_vs) { 
    red_uop.ctrl.funct6 := "b001001".U 
    red_uop.ctrl.funct3 := "b000".U  
  } .elsewhen (vredor_vs) { 
    red_uop.ctrl.funct6 := "b001010".U 
    red_uop.ctrl.funct3 := "b000".U  
  } .elsewhen (vredxor_vs) { 
    red_uop.ctrl.funct6 := "b001011".U 
    red_uop.ctrl.funct3 := "b000".U    
  } .elsewhen (vwredsum_vs) {
    red_uop.ctrl.funct6 := "b110001".U 
    red_uop.ctrl.funct3 := "b010".U 
    red_uop.ctrl.widen := true.B 
    red_uop.info.destEew := vsew+1.U     
  } .elsewhen (vwredsumu_vs) {
    red_uop.ctrl.funct6 := "b110000".U 
    red_uop.ctrl.funct3 := "b010".U  
    red_uop.ctrl.widen := true.B 
    red_uop.info.destEew := vsew+1.U     
  } .elsewhen (vfredosum_vs) { 
    red_uop.ctrl.funct6 := "b000000".U 
    red_uop.ctrl.funct3 := "b001".U     
  } .elsewhen (vfredusum_vs) { 
    red_uop.ctrl.funct6 := "b000000".U 
    red_uop.ctrl.funct3 := "b001".U      
  } .elsewhen (vfredmax_vs) { 
    red_uop.ctrl.funct6 := "b000110".U 
    red_uop.ctrl.funct3 := "b001".U         
  } .elsewhen (vfredmin_vs) { 
    red_uop.ctrl.funct6 := "b000100".U 
    red_uop.ctrl.funct3 := "b001".U    
  } .elsewhen (vfwredosum_vs) { 
    when (vs2_rnd0) {
      red_uop.ctrl.funct6 := "b110000".U 
      red_uop.ctrl.funct3 := "b001".U      
    } .otherwise {
      red_uop.ctrl.funct6 := "b110100".U 
      red_uop.ctrl.funct3 := "b001".U     
    }
  } .elsewhen (vfwredusum_vs) { 
    when (vs2_rnd0) {
      red_uop.ctrl.funct6 := "b110000".U 
      red_uop.ctrl.funct3 := "b001".U     
    } .otherwise {
      red_uop.ctrl.funct6 := "b000000".U 
      red_uop.ctrl.funct3 := "b001".U     
      red_uop.info.vsew   := vsew+1.U     
    }
  }
} .elsewhen (vwredsum_rnd0 || vwredsumu_rnd0 || vwredsum_rndx || vwredsumu_rndx) {
      red_uop.ctrl.funct6 := "b000000".U 
      red_uop.ctrl.funct3 := "b000".U     
      red_uop.ctrl.widen  := false.B 
      red_uop.info.vsew   := red_uop.info.destEew     
      red_uop.info.destEew := red_uop.info.destEew     
} .elsewhen (vwredsum_rnd01) {
    red_uop.ctrl.funct6 := "b110001".U 
    red_uop.ctrl.funct3 := "b010".U 
    red_uop.ctrl.widen  := true.B 
    red_uop.info.vsew   := red_uop.info.vsew -1.U      
    red_uop.info.destEew := vsew+1.U     
} .elsewhen (vwredsumu_rnd01) {
    red_uop.ctrl.funct6 := "b110000".U 
    red_uop.ctrl.funct3 := "b010".U  
    red_uop.ctrl.widen := true.B 
    red_uop.info.vsew   := red_uop.info.vsew -1.U      
    red_uop.info.destEew := vsew+1.U     
}

 
val alu = Seq.fill(NLanes/2)(Module(new VAlu))
val fpu = Seq.fill(NLanes/2)(Module(new VFPUTop))

for (i <- 0 until NLanes/2) {
  red_in(i).uop      := red_uop               
  red_in(i).vs1      := red_vs1(i)                  
  red_in(i).vs2      := red_vs2(i)                  
  red_in(i).old_vd   := "hffffffffffffffff".U       
  red_in(i).rs1      := rs1            
  red_in(i).prestart := 0.U                         
  red_in(i).mask     := "hff".U                     
  red_in(i).tail     := 0.U 
}

for (i <- 0 until NLanes/2) {
  alu(i).io.in.bits := red_in(i) 
  alu(i).io.in.valid := alu_in_valid
  alu_in_ready := alu(0).io.in.ready
  fpu(i).io.in.bits := red_in(i) 
  fpu(i).io.in.valid := fpu_in_valid
  fpu_in_ready := fpu(0).io.in.ready
  alu_out(i) := alu(i).io.out.bits 
  fpu_out(i) := fpu(i).io.out.bits 
  alu(i).io.out.ready := alu_out_ready 
  fpu(i).io.out.ready := fpu_out_ready
}

alu_out_valid := alu(0).io.out.valid 
fpu_out_valid := fpu(0).io.out.valid 

val output_valid = RegInit(false.B)
val output_data = RegInit(0.U(VLEN.W))

when (io.out.valid && io.out.ready) {
  output_valid := false.B
} .elsewhen (output_en && (red_state === calc_vs1) && red_out_valid && red_out_ready) {
  output_valid := true.B
}

when (output_en && (red_state === calc_vs1) && red_out_valid && red_out_ready) {
  output_data := red_vd_tail_vd
}

//--------- Ready & valid ---------
io.in.ready := (!io.in.valid || io.out.ready) && !red_busy 
io.out.valid := output_valid 
io.out.bits.vd := VecInit(Seq.tabulate(NLanes)(i => output_data((i+1)*LaneWidth-1, i*LaneWidth)))
// io.out.bits.rd := 0.U 

io.out.bits.uop := RegEnable(uop, fire) 
// temp!!
io.out.bits.fflags := Mux(alu_busy, alu(0).io.out.bits.fflags, fpu(0).io.out.bits.fflags)


}

object VerilogRed extends App {
  println("Generating the VPU Reduction hardware")
  emitVerilog(new Reduction(), Array("--target-dir", "generated"))
}




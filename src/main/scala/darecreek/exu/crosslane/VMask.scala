package darecreek

import chisel3._
import chisel3.util._
import scala.language.postfixOps

class VMask extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VExuInput))   
    val out = Decoupled(new VExuOutput)
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
val vlRemainBits = Cat(vlRemainBytes, 0.U(3.W))
val vsew_bytes = 1.U << vsew;
val vsew_bits = 8.U << vsew;
val fire = io.in.fire
val ele_cnt = vlenb.U/vsew_bytes


def VMAND_MM  = BitPat("b011001???????????010?????1010111")
def VMNAND_MM = BitPat("b011101???????????010?????1010111")
def VMANDN_MM = BitPat("b011000???????????010?????1010111")
def VMXOR_MM  = BitPat("b011011???????????010?????1010111")
def VMOR_MM   = BitPat("b011010???????????010?????1010111")
def VMNOR_MM  = BitPat("b011110???????????010?????1010111")
def VMORN_MM  = BitPat("b011100???????????010?????1010111")
def VMXNOR_MM = BitPat("b011111???????????010?????1010111")

def VCPOP_M   = BitPat("b010000??????10000010?????1010111")
def VFIRST_M  = BitPat("b010000??????10001010?????1010111")
def VMSBF_M   = BitPat("b010100??????00001010?????1010111")
def VMSIF_M   = BitPat("b010100??????00011010?????1010111")
def VMSOF_M   = BitPat("b010100??????00010010?????1010111")
def VIOTA_M   = BitPat("b010100??????10000010?????1010111")
def VID_V     = BitPat("b010100?0000010001010?????1010111")

val vmand_mm  = (funct6 === "b011001".U) && (funct3 === "b010".U)
val vmnand_mm = (funct6 === "b011101".U) && (funct3 === "b010".U)
val vmandn_mm = (funct6 === "b011000".U) && (funct3 === "b010".U)
val vmxor_mm  = (funct6 === "b011011".U) && (funct3 === "b010".U)
val vmor_mm   = (funct6 === "b011010".U) && (funct3 === "b010".U)
val vmnor_mm  = (funct6 === "b011110".U) && (funct3 === "b010".U)
val vmorn_mm  = (funct6 === "b011100".U) && (funct3 === "b010".U)
val vmxnor_mm = (funct6 === "b011111".U) && (funct3 === "b010".U)

val vpopc_m   = (funct6 === "b010000".U) && (funct3 === "b010".U) && (vs1_index === "b10000".U)
val vfirst_m  = (funct6 === "b010000".U) && (funct3 === "b010".U) && (vs1_index === "b10001".U)
val vmsbf_m   = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b00001".U)
val vmsif_m   = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b00011".U)
val vmsof_m   = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b00010".U)
val viota_m   = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b10000".U)
val vid_v     = (funct6 === "b010100".U) && (funct3 === "b010".U) && (vs1_index === "b10001".U)

val vm_logical = vmand_mm  || 
                 vmnand_mm || 
                 vmandn_mm || 
                 vmxor_mm  || 
                 vmor_mm   || 
                 vmnor_mm  || 
                 vmorn_mm  || 
                 vmxnor_mm 

val vid_vd = gen_index(expdIdx) 

val first = Wire(Vec(NLanes, SInt(xLen.W)))
val vmfirst = Wire(SInt(xLen.W))
val vmsbf   = Wire(Vec(NLanes, UInt(LaneWidth.W)))
val vmsif   = VecInit(Seq.tabulate(NLanes)(i => Cat(Cat(vmsbf.reverse) (VLEN-2, 0), 1.U)((i+1)*LaneWidth-1, i*LaneWidth)))
val vmsof   = Wire(Vec(NLanes, UInt(LaneWidth.W)))
val vmand   = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vmnand  = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vmandn  = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vmxor   = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vmor    = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vmnor   = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vmorn   = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vmxnor  = Wire(Vec(NLanes, UInt(LaneWidth.W))) 

val vd_vmsbf   = Wire(Vec(NLanes, UInt(LaneWidth.W)))
val vd_vmsif   = Wire(Vec(NLanes, UInt(LaneWidth.W)))
val vd_vmsof   = Wire(Vec(NLanes, UInt(LaneWidth.W)))
val vd_vmand   = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vd_vmnand  = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vd_vmandn  = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vd_vmxor   = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vd_vmor    = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vd_vmnor   = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vd_vmorn   = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vd_vmxnor  = Wire(Vec(NLanes, UInt(LaneWidth.W))) 

val ohasone    = Wire(Vec(NLanes, Bool())) 
val ihasone    = Wire(Vec(NLanes, Bool())) 

val old_vd_vl_mask = VecInit(Seq.tabulate(NLanes)(i => Mux(vlRemainBytes > vlenb.U, 0.U, ((~0.U(VLEN.W)) << vlRemain))((i+1)*LaneWidth-1, i*LaneWidth)))
val vd_vl_mask = VecInit(Seq.tabulate(NLanes)(i => Mux(vlRemainBytes > vlenb.U, (~0.U(VLEN.W)), ((~0.U(VLEN.W)) >> (VLEN.U- vlRemain)))((i+1)*LaneWidth-1, i*LaneWidth)))


val nmask   = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val sbf_mask = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val sif_mask = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val sof_mask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
val vd_nmask = Wire(Vec(NLanes, UInt(LaneWidth.W))) 

val vs2m = Wire(Vec(VLEN, UInt(1.W)))
val vs2m_reg = RegEnable(Cat(vs2m.reverse), 0.U, fire)

for (i<-0 until NLanes) {
  vmsof(i) := (~vmsbf(i)) & vmsif(i)
  nmask(i) := ~(vmask(i) | Fill(LaneWidth, vm))
  vd_nmask(i)  := old_vd(i) & nmask(i)
  sbf_mask(i)  := vmsbf(i) & (vmask(i) | Fill(LaneWidth, vm))
  sif_mask(i)  := vmsif(i) & (vmask(i) | Fill(LaneWidth, vm))
  sof_mask(i)  := vmsof(i) & (vmask(i) | Fill(LaneWidth, vm))
  vd_vmsbf(i)  := vd_nmask(i) | sbf_mask(i) 
  vd_vmsif(i)  := vd_nmask(i) | sif_mask(i) 
  vd_vmsof(i)  := vd_nmask(i) | sof_mask(i) 
  vd_vmand(i)  := vd_nmask(i) | vmand(i) 
  vd_vmnand(i) := vd_nmask(i) | vmnand(i)
  vd_vmandn(i) := vd_nmask(i) | vmandn(i)
  vd_vmxor(i)  := vd_nmask(i) | vmxor(i) 
  vd_vmor(i)   := vd_nmask(i) | vmor(i)  
  vd_vmnor(i)  := vd_nmask(i) | vmnor(i) 
  vd_vmorn(i)  := vd_nmask(i) | vmorn(i) 
  vd_vmxnor(i) := vd_nmask(i) | vmxnor(i)

}

for (i<-0 until VLEN) {
  vs2m(i) := 0.U 
  when (fire) {
    vs2m(i) := Cat(vs2.reverse)(i) & (Cat(vmask.reverse)(i) | vm)
  }
}

ihasone(0) := false.B
for (i <- 1 until NLanes) {
  ihasone(i) := ohasone(i-1)
}

val slices = Seq.fill(NLanes)(Module(new VMaskSlice))
for (i <- 0 until NLanes) {
   slices(i).io.ihasone := ihasone(i) 
   slices(i).io.vs1 := vs1(i) 
   slices(i).io.vs2 := vs2m.slice(i*LaneWidth, (i+1)*LaneWidth) 
   first(i)   := slices(i).io.first + (i*LaneWidth).S
   vmsbf(i)   := slices(i).io.sbf  
   ohasone(i) := slices(i).io.ohasone 
   vmand(i)   := slices(i).io.vmand 
   vmandn(i)  := slices(i).io.vmandn
   vmxor(i)   := slices(i).io.vmxor 
   vmor(i)    := slices(i).io.vmor  
   vmorn(i)   := slices(i).io.vmorn 
}

for (i <- 0 until NLanes) {
  vmnand(i) := ~vmand(i) 
  vmnor(i) := ~vmor(i) 
  vmxnor(i) := ~vmxor(i) 
}

vmfirst := -1.S
for (i <- 0 until NLanes reverse) {
  when (first(i) =/= -1.S) {
    vmfirst := first(i)
  }
}

def gen_index(cnt: UInt) = {
  val ele64 = Wire(Vec(vlenb, UInt(64.W)))
  val vmask_index = Wire(Vec(vlenb, UInt(8.W)))

  for (i<-0 until vlenb) {
    ele64(i)  := 0.U 
    ele64(i)  := ele_cnt * cnt + i.U/vsew_bytes
    vmask_index(i) := 0.U
    when (vsew === 0.U) {
      vmask_index(i) := ele64(i)(7, 0) 
    } .elsewhen (vsew === 1.U) {
      vmask_index(i) := ele64(i)((i%2+1)*8-1, (i%2)*8) 
    } .elsewhen (vsew === 2.U) {
      vmask_index(i) := ele64(i)((i%4+1)*8-1, (i%4)*8) 
    } .elsewhen (vsew === 3.U) {
      vmask_index(i) := ele64(i)((i%8+1)*8-1, (i%8)*8) 
    } 
  }
  vmask_index
} 

val viota_vd = gen_viota(Cat(vs2m.reverse) >> (ele_cnt * expdIdx), viota_m && fire, viota_m && fire && expdEnd)  

def gen_viota(vs: UInt, incr: Bool, end: Bool) = {

  val viota_cnt = RegInit(0.U(bVL.W))
  //val ele64 = Wire(UInt(64.W))
  val viota_vd = Wire(Vec(vlenb, UInt(8.W)))
  val slice_cnt = Wire(Vec(vlenb, UInt(64.W)))  

  // ele64 := 0.U 
  for (i<-0 until vlenb) {
    viota_vd(i) := 0.U 
    slice_cnt(i) := viota_cnt
  }

  when (incr) {
  for (i<-0 until (vlenb-1)) {
    when (i.U < ele_cnt) {
      when (vs(i)) {
         slice_cnt(i+1) := slice_cnt(i) +1.U
      } .otherwise {
         slice_cnt(i+1) := slice_cnt(i) 
      }
    }
  }

  for (i<-0 until vlenb) {
    when (vsew === 0.U) {
      viota_vd(i) := slice_cnt(i.U/vsew_bytes)(7, 0) 
    } .elsewhen (vsew === 1.U) {
      viota_vd(i) := slice_cnt(i.U/vsew_bytes)((i%2+1)*8-1, (i%2)*8) 
    } .elsewhen (vsew === 2.U) {
      viota_vd(i) := slice_cnt(i.U/vsew_bytes)((i%4+1)*8-1, (i%4)*8) 
    } .elsewhen (vsew === 3.U) {
      viota_vd(i) := slice_cnt(i.U/vsew_bytes)((i%8+1)*8-1, (i%8)*8) 
    } 
  }
  }
  when (end) {
    viota_cnt := 0.U 
  } .elsewhen (incr) {
    viota_cnt := viota_cnt + slice_cnt(ele_cnt)
  }
  viota_vd
}

val vd_mask = (~0.U(VLEN.W))
val vmask_bits = Cat(vmask.reverse) >> (ele_cnt * expdIdx)
val vmask_vd_bytes = Wire(Vec(vlenb, UInt(8.W)))
val vmask_vd_bits = Cat(vmask_vd_bytes.reverse)

val vmask_old_vd = Cat(old_vd.reverse) & (~vmask_vd_bits)
val vmask_ones_vd = vd_mask & (~vmask_vd_bits)
val vmask_vd = Mux(ma, vmask_ones_vd, vmask_old_vd)

for (i<-0 until vlenb) {
  vmask_vd_bytes(i) := "hff".U
  when ((!vm && !vmask_bits(i.U/vsew_bytes)) || (i.U >= vlRemainBytes)) {
    vmask_vd_bytes(i) := 0.U 
  }
}

val cross_lane_vd      = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val cross_lane_tail_vd = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val cross_lane_vd_reg  = Reg(Vec(NLanes, UInt(LaneWidth.W))) 
val cross_lane_valid   = RegInit(false.B) 

when ((vfirst_m || vpopc_m || vmsbf_m || vmsif_m || vmsof_m || vm_logical || viota_m || vid_v) && fire) {  
  cross_lane_valid := true.B                                           
}  .elsewhen (io.out.valid && io.out.ready) {
  cross_lane_valid := false.B
}

val vid_mask_vd = (Cat(vid_vd.reverse) & vmask_vd_bits) | vmask_vd
val viota_mask_vd = (Cat(viota_vd.reverse) & vmask_vd_bits) | vmask_vd

when (vmsbf_m && fire) {
  cross_lane_vd := vd_vmsbf
} .elsewhen (vmsif_m && fire) {
  cross_lane_vd := vd_vmsif
} .elsewhen (vmsof_m && fire) {
  cross_lane_vd := vd_vmsof
} .elsewhen (vmand_mm && fire) {
  cross_lane_vd := vd_vmand
} .elsewhen (vmnand_mm && fire) {
  cross_lane_vd := vd_vmnand
} .elsewhen (vmandn_mm && fire) {
  cross_lane_vd := vd_vmandn
} .elsewhen (vmxor_mm && fire) {
  cross_lane_vd := vd_vmxor
} .elsewhen (vmor_mm && fire) {
  cross_lane_vd := vd_vmor
} .elsewhen (vmnor_mm && fire) {
  cross_lane_vd := vd_vmnor
} .elsewhen (vmorn_mm && fire) {
  cross_lane_vd := vd_vmorn
} .elsewhen (vmxnor_mm && fire) {
  cross_lane_vd := vd_vmxnor
} .otherwise {
  cross_lane_vd := cross_lane_vd_reg 
}

val tail_bytes = Mux((vlRemainBytes >= vlenb.U), 0.U, vlenb.U-vlRemainBytes) 
val tail_bits = Cat(tail_bytes, 0.U(3.W))
val vmask_tail_bits = vd_mask >> tail_bits 
val tail_old_vd = Cat(old_vd.reverse) & (~vmask_tail_bits)  
val tail_ones_vd = ~vmask_tail_bits
val tail_vd = Mux(ta, tail_ones_vd, tail_old_vd)
val vid_cross_lane_vd = Wire(Vec(NLanes, UInt(LaneWidth.W)))

for (i <- 0 until NLanes) {
  vid_cross_lane_vd(i) := 0.U 
}

when (vid_v && fire) {
  vid_cross_lane_vd := VecInit(Seq.tabulate(NLanes)(i => ((vid_mask_vd & vmask_tail_bits) | tail_vd)((i+1)*LaneWidth-1, i*LaneWidth)))
} .elsewhen (viota_m && fire) {
  vid_cross_lane_vd := VecInit(Seq.tabulate(NLanes)(i => ((viota_mask_vd & vmask_tail_bits) | tail_vd)((i+1)*LaneWidth-1, i*LaneWidth)))
}

for (i <- 0 until NLanes) {
  // cross_lane_tail_vd(i) := Mux(ta, (old_vd_vl_mask(i) | (cross_lane_vd(i) & vd_vl_mask(i))), ((old_vd(i) & old_vd_vl_mask(i)) | (cross_lane_vd(i) & vd_vl_mask(i))))
  cross_lane_tail_vd(i) := old_vd_vl_mask(i) | (cross_lane_vd(i) & vd_vl_mask(i))
}

for (i <- 0 until NLanes) {
  when (fire) {
    when (vid_v || viota_m) {
      cross_lane_vd_reg(i) := vid_cross_lane_vd(i)
    } .otherwise {
      cross_lane_vd_reg(i) := cross_lane_tail_vd(i)
    }
  }
}

val rd = RegInit(0.U(xLen.W))

when (vpopc_m && fire) {
  rd := PopCount(Cat(vs2m.reverse)) 
} .elsewhen (vfirst_m && fire) {
  rd := vmfirst.asUInt 
}

//--------- Ready & valid ---------
io.in.ready := (!io.in.valid || io.out.ready) 
io.out.valid := cross_lane_valid
io.out.bits.vd := cross_lane_vd_reg
when (io.out.bits.uop.ctrl.rdVal) { io.out.bits.vd(0) := rd }

io.out.bits.uop := RegEnable(uop, fire)
// io.out.bits.rd := rd

// temp!!
io.out.bits.fflags := 0.U
io.out.bits.vxsat := false.B
}

object VerilogMask extends App {
  println("Generating the VPU Mask hardware")
  emitVerilog(new VMask(), Array("--target-dir", "generated"))
}




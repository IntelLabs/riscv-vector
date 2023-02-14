package vfu.alu

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import yunsuan._
import vfu._

class VMask extends Module {
  val VLEN = 128
  val xLen = 64
  val LaneWidth = 64
  val NLanes = VLEN/64
  val vlenb = VLEN/8
  val io = IO(new Bundle() {
    val vs1           = Input (UInt(VLEN.W))
    val vs2           = Input (UInt(VLEN.W))
    val vmask         = Input (UInt(VLEN.W))
    val old_vd        = Input (UInt(VLEN.W))
    val srcType       = Input (Vec(2, VectorElementFormat()))  
    val vdType        = Input (VectorElementFormat())  
    val op_code       = Input (OpType())        
    val info          = Input(new VIFuInfo)
    val valid         = Input(Bool())
    val vd            = Output(UInt(VLEN.W))
  })


val vs2 = VecInit(Seq.tabulate(NLanes)(i => (io.vs2)((i+1)*LaneWidth-1, i*LaneWidth)))  
val vmask = VecInit(Seq.tabulate(NLanes)(i => (io.vmask)((i+1)*LaneWidth-1, i*LaneWidth)))  
val old_vd = VecInit(Seq.tabulate(NLanes)(i => (io.old_vd)((i+1)*LaneWidth-1, i*LaneWidth))) 
val vm     = io.info.vm     
val ma     = io.info.ma     
val ta     = io.info.ta     
val lmul   = io.info.vlmul   
val vl     = io.info.vl     
val uopIdx = io.info.uopIdx 
val fire   = io.valid

val vpopc_m  = io.op_code === VipuType.vpopc 
val vfirst_m = io.op_code === VipuType.vfirst
val vmsbf_m  = io.op_code === VipuType.vmsbf 
val vmsif_m  = io.op_code === VipuType.vmsif 
val vmsof_m  = io.op_code === VipuType.vmsof 
val viota_m  = io.op_code === VipuType.viota 
val vid_v    = io.op_code === VipuType.vid   

val first = Wire(Vec(NLanes, SInt(xLen.W)))
val vmfirst = Wire(SInt(xLen.W))
val vmsbf   = Wire(Vec(NLanes, UInt(LaneWidth.W)))
val vmsif   = VecInit(Seq.tabulate(NLanes)(i => Cat(Cat(vmsbf.reverse) (VLEN-2, 0), 1.U)((i+1)*LaneWidth-1, i*LaneWidth)))
val vmsof   = Wire(Vec(NLanes, UInt(LaneWidth.W)))

val vd_vmsbf   = Wire(Vec(NLanes, UInt(LaneWidth.W)))
val vd_vmsif   = Wire(Vec(NLanes, UInt(LaneWidth.W)))
val vd_vmsof   = Wire(Vec(NLanes, UInt(LaneWidth.W)))

val ohasone    = Wire(Vec(NLanes, Bool())) 
val ihasone    = Wire(Vec(NLanes, Bool())) 

val nmask    = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val vd_nmask = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val sbf_mask = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val sif_mask = Wire(Vec(NLanes, UInt(LaneWidth.W))) 
val sof_mask = Wire(Vec(NLanes, UInt(LaneWidth.W)))

val vs2m = Wire(Vec(VLEN, UInt(1.W)))

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
   slices(i).io.vs2 := vs2m.slice(i*LaneWidth, (i+1)*LaneWidth) 
   first(i)   := slices(i).io.first + (i*LaneWidth).S
   vmsbf(i)   := slices(i).io.sbf  
   ohasone(i) := slices(i).io.ohasone 
}

vmfirst := -1.S
for (i <- 0 until NLanes reverse) {
  when (first(i) =/= -1.S) {
    vmfirst := first(i)
  }
}


// vid
val vsew = io.vdType(1,0)
val vsew_bytes = 1.U << vsew
val ele_cnt = vlenb.U/vsew_bytes
val vid_vd = gen_index(uopIdx) 

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

val vd_mask = (~0.U(VLEN.W))
val vmask_bits = Cat(vmask.reverse) >> (ele_cnt * uopIdx)
val vmask_vd_bytes = Wire(Vec(vlenb, UInt(8.W)))
val vmask_vd_bits = Cat(vmask_vd_bytes.reverse)

val vmask_old_vd = Cat(old_vd.reverse) & (~vmask_vd_bits)
val vmask_ones_vd = vd_mask & (~vmask_vd_bits)
val vmask_vd = Mux(ma, vmask_ones_vd, vmask_old_vd)

val vlRemain = Mux((vl >= (ele_cnt * uopIdx)), (vl - (ele_cnt * uopIdx)), 0.U) 
val vlRemainBytes = vlRemain << vsew

for (i<-0 until vlenb) {
  vmask_vd_bytes(i) := "hff".U
  when ((!vm && !vmask_bits(i.U/vsew_bytes)) || (i.U >= vlRemainBytes)) {
    vmask_vd_bytes(i) := 0.U 
  }
}

val vid_mask_vd = (Cat(vid_vd.reverse) & vmask_vd_bits) | vmask_vd

val tail_bytes = Mux((vlRemainBytes >= vlenb.U), 0.U, vlenb.U-vlRemainBytes) 
val tail_bits = Cat(tail_bytes, 0.U(3.W))
val vmask_tail_bits = vd_mask >> tail_bits 
val tail_old_vd = Cat(old_vd.reverse) & (~vmask_tail_bits)  
val tail_ones_vd = ~vmask_tail_bits
val vid_tail_vd = Mux(ta, tail_ones_vd, tail_old_vd)
val vid_tail_mask_vd = Wire(UInt(VLEN.W))

vid_tail_mask_vd := 0.U 
when (vid_v && fire) {
  vid_tail_mask_vd := (vid_mask_vd & vmask_tail_bits) | vid_tail_vd
}

val vd      = Wire(UInt(VLEN.W)) 
val rd      = Wire(UInt(xLen.W))
val tail_vd = Wire(UInt(VLEN.W)) 
val vd_reg  = RegInit(0.U(VLEN.W)) 

when (vmsbf_m && fire) {
  vd := Cat(vd_vmsbf.reverse)
} .elsewhen (vmsif_m && fire) {
  vd := Cat(vd_vmsif.reverse)
} .elsewhen (vmsof_m && fire) {
  vd := Cat(vd_vmsof.reverse)
} .otherwise {
  vd := vd_reg 
}

val old_vd_vl_mask = (~0.U(VLEN.W)) << vl
val vd_vl_mask = (~0.U(VLEN.W)) >> (VLEN.U- vl)

tail_vd := old_vd_vl_mask | (vd & vd_vl_mask)

when ((vmsbf_m || vmsif_m || vmsof_m) && fire) {
  vd_reg := tail_vd
} .elsewhen((vpopc_m || vfirst_m) && fire) {
  vd_reg := Cat(0.U((VLEN-xLen).W), rd)
} .elsewhen (vid_v && fire) {
  vd_reg := vid_tail_mask_vd 
}

rd := 0.U
when (vpopc_m && fire) {
  rd := PopCount(Cat(vs2m.reverse)) 
} .elsewhen (vfirst_m && fire) {
  rd := vmfirst.asUInt 
}

io.vd := vd_reg 

}

object VerilogMask extends App {
  println("Generating the VPU Mask hardware")
  emitVerilog(new VMask(), Array("--target-dir", "generated"))
}




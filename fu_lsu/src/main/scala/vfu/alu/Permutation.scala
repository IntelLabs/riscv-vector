package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import yunsuan._
import yunsuan.vector._

class Permutation extends Module {
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


val (vs1, vs2, vmask, old_vd) = (io.vs1, io.vs2, io.vmask, io.old_vd)

val vm     = io.info.vm     
val ma     = io.info.ma     
val ta     = io.info.ta     
val lmul   = io.info.vlmul   
val vl     = io.info.vl     
val uopIdx = io.info.uopIdx 
val fire   = io.valid
val vsew   = io.srcType(1)(1,0)
val signed = io.srcType(1)(2).asBool
val widen  = io.vdType(1,0) === (io.srcType(1)(1,0) + 1.U) 
val vsew_bytes = 1.U << vsew;
val vsew_bits = 8.U << vsew;
val ele_cnt = vlenb.U/vsew_bytes
val vlRemain = Mux((vl >= (ele_cnt * uopIdx)), (vl - (ele_cnt * uopIdx)), 0.U) 
val vlRemainBytes = vlRemain << vsew
val current_res_boundary = Mux((vlRemainBytes > vlenb.U), vlenb.U, vlRemainBytes)

val previous_ones_sum = Wire(UInt(7.W))
val res_idx = Wire(UInt(7.W))
val res_valid = Wire(Bool())
val current_ones_sum = Wire(Vec(vlenb+1, UInt(7.W)))
val base = Cat(uopIdx, 0.U(4.W))
val is_cmprs = io.op_code === VipuType.vcompress

val vs1_bits = vs1 >> (ele_cnt * uopIdx)
val vs1_byte_strb = Wire(Vec(vlenb, UInt(1.W))) 
val cmprs_vd = Wire(Vec(vlenb, UInt(8.W))) 
val cmprs_vd_reg = RegEnable(Cat(cmprs_vd.reverse), 0.U, fire) 

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

previous_ones_sum := vmask(6,0)

for (i <-0 until (vlenb+1)) {
  current_ones_sum(i) := 0.U
}

for (i <-0 until vlenb) {
  cmprs_vd(i) := 0.U
}

res_idx := 0.U
res_valid := false.B
current_ones_sum(0) := previous_ones_sum
for (i <-0 until vlenb) {
  cmprs_vd(i) := old_vd(i*8+7, i*8)
  current_ones_sum(i+1) := current_ones_sum(i) + vs1_byte_strb(i)
  res_idx := current_ones_sum(i+1) -base - 1.U
  res_valid := current_ones_sum(i+1) >= base + 1.U
  when (vs1_byte_strb(i).asBool && res_valid && (res_idx < current_res_boundary)) {
    cmprs_vd(res_idx) := vs2(i*8+7, i*8)
  } 
}


io.vd := cmprs_vd_reg

}

object VerilogPer extends App {
  println("Generating the VPU CrossLane hardware")
  emitVerilog(new Permutation(), Array("--target-dir", "generated"))
}



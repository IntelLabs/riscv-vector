/***************************************************************************************
*Copyright (c) 2023-2024 Intel Corporation
*Vector Acceleration IP core for RISC-V* is licensed under Mulan PSL v2.
*You can use this software according to the terms and conditions of the Mulan PSL v2.
*You may obtain a copy of Mulan PSL v2 at:
*        http://license.coscl.org.cn/MulanPSL2
*THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
*EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
*MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*See the Mulan PSL v2 for more details.
***************************************************************************************/

package vfu.alu

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import vfu._
import vfu.alu.VAluOpcode._

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
    val srcType       = Input (Vec(2, UInt(4.W)))  
    val vdType        = Input (UInt(4.W))  
    val op_code       = Input (UInt(6.W))       
    val info          = Input(new VIFuInfo)
    val valid         = Input(Bool())
    val vd            = Output(UInt(VLEN.W))
  })


val vcompress = io.op_code === VAluOpcode.vcompress

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
val res_idx = Wire(Vec(vlenb, UInt(7.W)))
val res_valid = Wire(Vec(vlenb, Bool()))
val current_ones_sum = Wire(Vec(vlenb+1, UInt(7.W)))
val base = Cat(uopIdx, 0.U(4.W))

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
  res_idx(i) := 0.U
  res_valid(i) := false.B
}

current_ones_sum(0) := previous_ones_sum
for (i <-0 until vlenb) {
  cmprs_vd(i) := Mux(ta, "hff".U, old_vd(i*8+7, i*8))
  current_ones_sum(i+1) := current_ones_sum(i) + vs1_byte_strb(i)
  res_idx(i) := current_ones_sum(i+1) -base - 1.U
  res_valid(i) := current_ones_sum(i+1) >= base + 1.U
  when (vs1_byte_strb(i).asBool && res_valid(i) && (res_idx(i) < current_res_boundary)) {
    cmprs_vd(res_idx(i)) := vs2(i*8+7, i*8)
  } 
}


io.vd := cmprs_vd_reg

}

object VerilogPer extends App {
  println("Generating the VPU CrossLane hardware")
  emitVerilog(new Permutation(), Array("--target-dir", "generated"))
}



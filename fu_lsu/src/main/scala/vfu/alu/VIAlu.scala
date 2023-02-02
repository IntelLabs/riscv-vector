package yunsuan.vector.alu

import chisel3._
import chisel3.util._

class VIAlu extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val opcode = Input(UInt(6.W))
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(128.W))
    val vs2 = Input(UInt(128.W))
    val old_vd = Input(UInt(128.W))
    val mask = Input(UInt(128.W))
    val vxrm = Input(UInt(2.W))
    val is_sub = Input(Bool())  // subtract

    val vd = Output(UInt(128.W))
    val vxsat = Output(Bool())
  })



  
}
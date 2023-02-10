package vfu.alu

import chisel3._
import chisel3.util._
import yunsuan.vector.VFuInfo
import yunsuan.vector.alu.{VIAlu}

class VIAluInput extends Bundle {
  val opcode = UInt(6.W)
  val info = new VFuInfo
  val srcType = Vec(2, UInt(4.W))
  val vdType  = UInt(4.W)
  val vs1 = UInt(128.W)
  val vs2 = UInt(128.W)
  val old_vd = UInt(128.W)
  val mask = UInt(128.W)
  val vxrm = UInt(2.W)
  val is_sub = Bool()
}

class VIAluOutput extends Bundle {
  val vd = UInt(128.W)
  val vxsat = Bool()
}

class VIAluWrapper extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VIAluInput))
    val out = Decoupled(new VIAluOutput)
  })

  val vIAlu = Module(new VIAlu)
  vIAlu.io.opcode := io.in.bits.opcode
  vIAlu.io.info := io.in.bits.info
  vIAlu.io.srcType := io.in.bits.srcType
  vIAlu.io.vdType := io.in.bits.vdType
  vIAlu.io.vs1 := io.in.bits.vs1
  vIAlu.io.vs2 := io.in.bits.vs2
  vIAlu.io.old_vd := io.in.bits.old_vd
  vIAlu.io.mask := io.in.bits.mask
  vIAlu.io.vxrm := io.in.bits.vxrm
  vIAlu.io.is_sub := io.in.bits.is_sub
  io.out.bits.vd := vIAlu.io.vd
  io.out.bits.vxsat := vIAlu.io.vxsat

  vIAlu.io.valid := io.in.valid
  io.out.valid := vIAlu.io.validOut
  io.in.ready := io.out.ready
}
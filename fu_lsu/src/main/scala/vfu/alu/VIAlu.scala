//---- Need discussion ----
// 1) add/sub seperation

package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import yunsuan.vector._

class VIAlu extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val opcode = Input(UInt(6.W))
    val info = Input(new VFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(128.W))
    val vs2 = Input(UInt(128.W))
    val old_vd = Input(UInt(128.W))
    val mask = Input(UInt(128.W))
    val vxrm = Input(UInt(2.W))
    val is_sub = Input(Bool())  // subtract

    val validOut = Output(Bool())
    val vd = Output(UInt(128.W))
    val vxsat = Output(Bool())
  })

  // Latency of ALU is two cycles
  val validS1 = RegInit(false.B)
  validS1 := io.valid
  io.validOut := RegNext(validS1)

  val vIntFixpAlu = Module(new VIntFixpAlu)
  vIntFixpAlu.io.opcode := io.opcode 
  vIntFixpAlu.io.info := io.info
  vIntFixpAlu.io.srcType := io.srcType
  vIntFixpAlu.io.vdType := io.vdType
  vIntFixpAlu.io.vs1 := io.vs1
  vIntFixpAlu.io.vs2 := io.vs2
  vIntFixpAlu.io.vmask := io.mask
  vIntFixpAlu.io.is_sub := io.is_sub

  val vReduAlu = Module(new Reduction)
  vReduAlu.io.op_code := io.opcode 
  vReduAlu.io.info := io.info
  vReduAlu.io.srcType := io.srcType
  vReduAlu.io.vdType := io.vdType
  vReduAlu.io.vs1 := io.vs1
  vReduAlu.io.vs2 := io.vs2
  vReduAlu.io.vmask := io.mask
  vReduAlu.io.old_vd := io.old_vd
  vReduAlu.io.valid := io.valid

  val vMaskAlu = Module(new VMask)
  vMaskAlu.io.op_code := io.opcode 
  vMaskAlu.io.info := io.info
  vMaskAlu.io.srcType := io.srcType
  vMaskAlu.io.vdType := io.vdType
  vMaskAlu.io.vs1 := io.vs1
  vMaskAlu.io.vs2 := io.vs2
  vMaskAlu.io.vmask := io.mask
  vMaskAlu.io.old_vd := io.old_vd
  vMaskAlu.io.valid := io.valid

  val vPermAlu = Module(new Permutation)
  vPermAlu.io.op_code := io.opcode 
  vPermAlu.io.info := io.info
  vPermAlu.io.srcType := io.srcType
  vPermAlu.io.vdType := io.vdType
  vPermAlu.io.vs1 := io.vs1
  vPermAlu.io.vs2 := io.vs2
  vPermAlu.io.vmask := io.mask
  vPermAlu.io.old_vd := io.old_vd
  vPermAlu.io.valid := io.valid

  val opcodeS1 = RegNext(io.opcode)
  val vdFinal = Mux(opcodeS1 < 28.U, vIntFixpAlu.io.vd,
                Mux(opcodeS1 < 34.U, vReduAlu.io.vd, 
                Mux(opcodeS1 < 41.U, vMaskAlu.io.vd, vPermAlu.io.vd)))
  io.vd := RegNext(vdFinal)
  io.vxsat := false.B         //!!!! Todo
}

object VerilogAlu extends App {
  println("Generating the VALU hardware")
  emitVerilog(new VIAlu(), Array("--target-dir", "generated"))
}
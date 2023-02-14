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
import vfu._

class VIAlu extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VIFuInput))
    val out = ValidIO(new VIFuOutput)
  })

  // Latency of ALU is two cycles
  val validS1 = RegInit(false.B)
  validS1 := io.in.valid
  io.out.valid := RegNext(validS1)

  val vIntFixpAlu = Module(new VIntFixpAlu)
  vIntFixpAlu.io.in := io.in.bits

  val vReduAlu = Module(new Reduction)
  vReduAlu.io.op_code := io.in.bits.opcode 
  vReduAlu.io.info := io.in.bits.info
  vReduAlu.io.srcType := io.in.bits.srcType
  vReduAlu.io.vdType := io.in.bits.vdType
  vReduAlu.io.vs1 := io.in.bits.vs1
  vReduAlu.io.vs2 := io.in.bits.vs2
  vReduAlu.io.vmask := io.in.bits.mask
  vReduAlu.io.old_vd := io.in.bits.old_vd
  vReduAlu.io.valid := io.in.valid

  val vMaskAlu = Module(new VMask)
  vMaskAlu.io.op_code := io.in.bits.opcode 
  vMaskAlu.io.info := io.in.bits.info
  vMaskAlu.io.srcType := io.in.bits.srcType
  vMaskAlu.io.vdType := io.in.bits.vdType
  vMaskAlu.io.vs1 := io.in.bits.vs1
  vMaskAlu.io.vs2 := io.in.bits.vs2
  vMaskAlu.io.vmask := io.in.bits.mask
  vMaskAlu.io.old_vd := io.in.bits.old_vd
  vMaskAlu.io.valid := io.in.valid

  val vPermAlu = Module(new Permutation)
  vPermAlu.io.op_code := io.in.bits.opcode 
  vPermAlu.io.info := io.in.bits.info
  vPermAlu.io.srcType := io.in.bits.srcType
  vPermAlu.io.vdType := io.in.bits.vdType
  vPermAlu.io.vs1 := io.in.bits.vs1
  vPermAlu.io.vs2 := io.in.bits.vs2
  vPermAlu.io.vmask := io.in.bits.mask
  vPermAlu.io.old_vd := io.in.bits.old_vd
  vPermAlu.io.valid := io.in.valid

  val opcodeS1 = RegNext(io.in.bits.opcode)
  val vdFinal = Mux(opcodeS1 < 33.U, vIntFixpAlu.io.out.vd,
                Mux(opcodeS1 < 39.U, vReduAlu.io.vd, 
                Mux(opcodeS1 < 46.U, vMaskAlu.io.vd, vPermAlu.io.vd)))
  io.out.bits.vd := RegNext(vdFinal)
  io.out.bits.vxsat := false.B         //!!!! Todo
}

object VerilogAlu extends App {
  println("Generating the VALU hardware")
  emitVerilog(new VIAlu(), Array("--target-dir", "generated"))
}
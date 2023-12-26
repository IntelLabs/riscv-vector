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

/** Vector Lane: 64-bit data-path of FUs
  */
package darecreek

import chisel3._
import chisel3.util._
import darecreek.exu.fp._
import darecreek.exu.lanevfu.alu._
import darecreek.exu.lanevfu.mac._
import darecreek.exu.fu.div._
import chipsalliance.rocketchip.config._
import darecreek.exu.vfu.{VFuParamsKey, VFuParameters}

class DummyLaneFU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LaneFUInput))
    val out = Decoupled(new LaneFUOutput)
  })

  io.out.bits.uop := io.in.bits.uop
  io.out.bits.vd := 0.U
  io.out.valid := false.B
  io.out.bits.fflags := 0.U 
  io.out.bits.vxsat := false.B
  io.in.ready := io.out.ready
}

class VLane extends Module{
  val io = IO(new Bundle {
    val idx = Input(UInt(LaneIdxWidth.W))
    val in = new Bundle {
      val data = Input(new LaneFUInput)
      val valids = Input(Vec(NLaneExuFUs, Bool()))
      val readys = Output(Vec(NLaneExuFUs, Bool()))
    }
    val out = Vec(2, Decoupled(new LaneFUOutput))
  })

  val p = Parameters.empty.alterPartial({
                     case VFuParamsKey => VFuParameters(VLEN = 256)})
  // ALU
  val valu = Module(new LaneVAlu()(p))
  // val valu = Module(new DummyLaneFU)
  // MUL
  val vmac = Module(new LaneVMac)
  // val vmac = Module(new DummyLaneFU)
  // FP
  // val vfp = Module(new VFPUTop)
  val vfp = Module(new DummyLaneFU)
  // fake div
  // val vdiv = Module(new DivTop)
  val vdiv = Module(new DummyLaneFU)

  // Input of ALU
  valu.io.in.bits := io.in.data
  valu.io.in.valid := io.in.valids(0)
  io.in.readys(0) := valu.io.in.ready
  // Input of MUL
  vmac.io.in.bits := io.in.data
  vmac.io.in.valid := io.in.valids(1)
  io.in.readys(1) := vmac.io.in.ready
  // Input of FP
  vfp.io.in.bits := io.in.data
  vfp.io.in.valid := io.in.valids(2)
  io.in.readys(2) := vfp.io.in.ready
  // Input of div
  vdiv.io.in.bits := io.in.data
  vdiv.io.in.valid := io.in.valids(3)
  io.in.readys(3) := vdiv.io.in.ready

  /**
    * Outputs (two write-back ports)
    */
  io.out(0) <> valu.io.out
  val arb = Module(new Arbiter(new LaneFUOutput, 3))
  arb.io.in(0) <> vdiv.io.out
  arb.io.in(1) <> vfp.io.out
  arb.io.in(2) <> vmac.io.out
  io.out(1) <> arb.io.out  
}
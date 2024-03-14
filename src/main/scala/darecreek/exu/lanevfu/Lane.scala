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
package darecreek.exu.lanevfu

import chisel3._
import chisel3.util._
import darecreek.exu.lanevfu.alu._
import darecreek.exu.lanevfu.mac._
import darecreek.exu.lanevfu.fp._
// import darecreek.exu.vfucore.fp._
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.{VFuParamsKey, VFuParameters}
import darecreek._

object RedirectConvert {
  def apply(in: Redirect) = {
    val out = in.valid.asTypeOf(new darecreek.exu.vfucoreconfig.Redirect_darecreek)
    out
  }
}

class DummyLaneFU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LaneFUInput))
    val redirect = Input(new Redirect)
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
    val redirect = Input(new Redirect)
    val out = Vec(2, Decoupled(new LaneFUOutput))
  })

  implicit val p = Parameters.empty.alterPartial({
                     case VFuParamsKey => VFuParameters(VLEN = VLEN)})
  // ALU
  val valu = Module(new LaneVAlu)
  // val valu = Module(new DummyLaneFU)
  // MUL
  val vmac = Module(new LaneVMac)
  // val vmac = Module(new DummyLaneFU)
  // FP
  val vfp = Module(new LaneFP)

  // Input of ALU
  valu.io.in.bits := io.in.data
  valu.io.in.valid := io.in.valids(0)
  valu.io.redirect := RedirectConvert(io.redirect)
  io.in.readys(0) := valu.io.in.ready
  // Input of MUL
  vmac.io.in.bits := io.in.data
  vmac.io.in.valid := io.in.valids(1)
  vmac.io.redirect := RedirectConvert(io.redirect)
  io.in.readys(1) := vmac.io.in.ready
  // Input of FP
  vfp.io.in.bits := io.in.data
  vfp.io.in.valid := io.in.valids(2)
  vfp.io.redirect := RedirectConvert(io.redirect)
  io.in.readys(2) := vfp.io.in.ready

  /**
    * Outputs (two write-back ports)
    */
  io.out(0) <> valu.io.out

  val arb = Module(new Arbiter(new LaneFUOutput, NLaneExuFUs - 1))
  arb.io.in(0) <> vfp.io.out
  arb.io.in(1) <> vmac.io.out
  io.out(1) <> arb.io.out  
}
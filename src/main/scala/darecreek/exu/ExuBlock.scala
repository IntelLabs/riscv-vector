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

/**
  * ExuBlock: arithmetic functional units
  */

package darecreek

import chisel3._
import chisel3.util._

class VExuBlock extends Module {
  val io = IO(new Bundle {
    val flush = Input(Bool())
    val in = new Bundle {
      val bits = Input(new VExuInput)
      val valid = Input(Bool())
      val readys = Output(Vec(NArithFUs, Bool()))
    }
  val out = ValidIO(new VExuOutput)
  })

  val laneExu = Module(new VLaneExu)
  val crossLExu = Module(new VCrossLaneExu) 
  
  // LaneExu input
  laneExu.io.in.bits := io.in.bits
  laneExu.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.laneExu
  for (i <- 0 until NLaneExuFUs) {
    io.in.readys(i) := laneExu.io.in.readys(i)
  }
  // Cross-lane Exu input
  crossLExu.io.in.bits := io.in.bits
  crossLExu.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.crossLane
  for (i <- 0 until (NArithFUs - NLaneExuFUs)) {
    io.in.readys(i + NLaneExuFUs) := crossLExu.io.in.readys(i)
  }

  // Output arbiter
  val arb = Module(new Arbiter(new VExuOutput, 2))
  arb.io.in(0) <> laneExu.io.out
  arb.io.in(1) <> crossLExu.io.out
  io.out.valid := arb.io.out.valid && !io.flush
  io.out.bits := arb.io.out.bits
  arb.io.out.ready := true.B
}
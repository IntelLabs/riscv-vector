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

package vfutest.alu

import chisel3._
import chisel3.util._
import vfu.{VIFuInfo, VIFuInput, VIFuOutput}
import vfu.alu.{VIAlu}

class VIAluWrapper extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VIFuInput))
    val out = Decoupled(new VIFuOutput)
  })

  val vIAlu = Module(new VIAlu)
  vIAlu.io.in.bits := io.in.bits
  io.out.bits := vIAlu.io.out.bits

  vIAlu.io.in.valid := io.in.valid
  io.out.valid := vIAlu.io.out.valid
  io.in.ready := io.out.ready
}
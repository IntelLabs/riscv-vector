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

package darecreek.exu.vfucore

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import darecreek.exu.vfucoreconfig.VUop
// import darecreek.Redirect

class SewOH extends Bundle {  // 0   1   2   3
  val oneHot = UInt(4.W) // b0-b3: 8, 16, 32, 64
  def is8 = oneHot(0)
  def is16 = oneHot(1)
  def is32 = oneHot(2)
  def is64 = oneHot(3)
}
object SewOH {
  def apply(vsew: UInt): SewOH = {
    val sew = Wire(new SewOH)
    sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U)).asUInt
    sew
  }
}

// Input of FU
class VFuInput(implicit p: Parameters) extends VFuBundle {
  val uop = new VUop
  val vs1 = UInt(VLEN.W)
  val vs2 = UInt(VLEN.W)
  val rs1 = UInt(XLEN.W)
  val oldVd = UInt(VLEN.W)
  val mask = UInt(VLEN.W)
}
// Output of ALU
class VAluOutput(implicit p: Parameters) extends VFuBundle {
  val vd = UInt(VLEN.W)
  val vxsat = Bool()
}
// Output of VMASK
class VMaskOutput(implicit p: Parameters) extends VFuBundle {
  val uop = new VUop
  val vd = UInt(VLEN.W)
}

// Output of FPU
class VFpuOutput(implicit p: Parameters) extends VFuBundle {
  val uop = new VUop
  val vd = UInt(VLEN.W)
  val fflags = UInt(5.W)
}

class VPermInput(implicit p: Parameters) extends VFuBundle {
  val uop = new VUop
  val rs1 = UInt(64.W)
  val vs1_preg_idx = Vec(8, UInt(8.W))
  val vs2_preg_idx = Vec(8, UInt(8.W))
  val old_vd_preg_idx = Vec(8, UInt(8.W))
  val mask_preg_idx = UInt(8.W)
  val uop_valid = Bool()
  val rdata = UInt(VLEN.W)
  val rvalid = Bool()
}

class VPermOutput(implicit p: Parameters) extends VFuBundle {
  val uop = new VUop
  val rd_en = Bool()
  val rd_preg_idx = UInt(8.W)
  val wb_vld = Bool()
  val wb_data = UInt(VLEN.W)
  val perm_busy = Bool()
}

// Input of the lane FU
class LaneFUInput(implicit p: Parameters) extends VFuBundle {
  val uop = new VUop
  val vs1 = UInt(64.W)
  val vs2 = UInt(64.W)
  val old_vd = UInt(64.W)
  val rs1 = UInt(XLEN.W)
  val prestart = UInt(8.W)
  val mask = UInt(8.W)
  val tail = UInt(8.W)
}
// Output of the lane FU
class LaneFUOutput(implicit p: Parameters) extends Bundle {
  val uop = new VUop
  val vd = UInt(64.W)
  val fflags = UInt(5.W) // Floating-point accrued exception flag
  val vxsat = Bool() // Fixed-point accrued saturation flag
}

class LaneUnit(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val out = DecoupledIO(new LaneFUOutput)
  })
}
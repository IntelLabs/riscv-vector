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

package yunsuan.vector

import chisel3._
import chisel3.util._

class VIFuInfo extends Bundle {
  val vm = Bool()
  val ma = Bool()
  val ta = Bool()
  val vlmul = UInt(3.W)
  val vl = UInt(8.W)
  val uopIdx = UInt(6.W)
  val vxrm = UInt(2.W)
}

class VIFuInput extends Bundle {
  val opcode = UInt(6.W)
  val info = new VIFuInfo
  val srcType = Vec(2, UInt(4.W))
  val vdType  = UInt(4.W)
  val vs1 = UInt(128.W)
  val vs2 = UInt(128.W)
  val old_vd = UInt(128.W)
  val mask = UInt(128.W)
}

class VIFuOutput extends Bundle {
  val vd = UInt(128.W)
  val vxsat = Bool()
}

class SewOH extends Bundle {  // 0   1   2   3
  val oneHot = Vec(4, Bool()) // 8, 16, 32, 64
  def is8 = oneHot(0)
  def is16 = oneHot(1)
  def is32 = oneHot(2)
  def is64 = oneHot(3)
}
object SewOH {
  def apply(vsew: UInt): SewOH = {
    val sew = Wire(new SewOH)
    sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U))
    sew
  }
}
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

package vfu

import chisel3._
import chisel3.util._

object UIntSplit {
  //Split into elements, e.g., if sew=8, UInt(64.W) => Seq(UInt(8.W) * 8)
  def apply(data: UInt, sew: Int): Seq[UInt] = {
    val w = data.getWidth
    // require(w == 64 || w == 32 || w == 16 || w == 8)
    require(w >= sew && w % sew == 0)
    Seq.tabulate(w/sew)(i => data(sew*i+sew-1, sew*i))
  }
  // def apply(dataVLEN: UInt): Seq[UInt] = {
  //   Seq.tabulate(NLanes)(i => dataVLEN(LaneWidth*i+LaneWidth-1, LaneWidth*i))
  // }
}

object BitsExtend {
  def apply(data: UInt, extLen: Int, signed: Bool): UInt = {
    val width = data.getWidth
    require(width < extLen)
    Cat(Fill(extLen - width, data(width-1) && signed), data)
  }
  def vector(data: UInt, extLen: Int, signed: Bool, sew: Int): UInt = { // For extension instrn
    require(data.getWidth % sew == 0)
    val nVec = data.getWidth / sew
    require(extLen % nVec == 0)
    Cat(UIntSplit(data, sew).map(dataSplit => apply(dataSplit, extLen/nVec, signed)).reverse)
  }
}
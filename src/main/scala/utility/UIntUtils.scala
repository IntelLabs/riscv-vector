/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utility

import chisel3._
import chisel3.util._

class UIntCompressor (inWidth: Int, filter: Seq[Int]) extends Module {
  val outWidth = filter.length
  val filterSorted = filter.sorted
  var filterMask = new String
  for (i <- 0 until inWidth) {
    var flag = 0
    for (sel <- filter) {
      if (sel == i)
        flag = 1
    }
    filterMask ++= flag.toString()
  }
  filterMask = filterMask.reverse

  require(filter.length != 0)
  require(filter.length == filter.distinct.length)
  require(filter.max < inWidth)

  override def desiredName = s"UIntCompressor_${inWidth}_${filterMask}"

  val io = IO(new Bundle() {
    val in = Input(UInt(inWidth.W))
    val out = Output(UInt(outWidth.W))
  })

  io.out := VecInit(filterSorted.map(io.in(_))).asUInt
}

class UIntExtractor (outWidth: Int, filter: Seq[Int]) extends Module {
  val inWidth = filter.length
  val filterSorted = filter.sorted
  var filterMask = new String
  for (i <- 0 until outWidth) {
    var flag = 0
    for (sel <- filter) {
      if (sel == i)
        flag = 1
    }
    filterMask ++= flag.toString()
  }
  filterMask = filterMask.reverse

  require(filter.length != 0)
  require(filter.length == filter.distinct.length)
  require(filter.max < outWidth)

  override def desiredName = s"UIntExtractor_${outWidth}_${filterMask}"

  val io = IO(new Bundle() {
    val in = Input(UInt(inWidth.W))
    val out = Output(UInt(outWidth.W))
  })

  val outVec = WireInit(VecInit(Seq.fill(outWidth)(false.B)))
  for (i <- 0 until inWidth) {
    outVec(filterSorted(i)) := io.in(i)
  }
  io.out := outVec.asUInt
}

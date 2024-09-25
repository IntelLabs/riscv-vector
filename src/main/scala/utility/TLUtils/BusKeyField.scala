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
import freechips.rocketchip.util._

// indicates where the memory access request comes from
object MemReqSource extends Enumeration {
  val NoWhere = Value("NoWhere")

  val CPUInst = Value("CPUInst")
  val CPULoadData = Value("CPULoadData")
  val CPUStoreData = Value("CPUStoreData")
  val CPUAtomicData = Value("CPUAtomicData")
  val L1InstPrefetch = Value("L1InstPrefetch")
  val L1DataPrefetch = Value("L1DataPrefetch")
  val PTW = Value("PTW")
  val Prefetch2L2BOP = Value("Prefetch2L2BOP")
  val Prefetch2L2PBOP = Value("Prefetch2L2PBOP")
  val Prefetch2L2SMS = Value("Prefetch2L2SMS")
  val Prefetch2L2Stream = Value("Prefetch2L2Stream")
  val Prefetch2L2Stride = Value("Prefetch2L2Stride")
  val Prefetch2L2TP = Value("Prefetch2L2TP")
  val Prefetch2L2Unknown = Value("Prefetch2L2Unknown")
  val Prefetch2L3Unknown = Value("Prefetch2L3Unknown")
  val ReqSourceCount = Value("ReqSourceCount")

  val reqSourceBits = log2Ceil(ReqSourceCount.id)
}

// Used to indicate the source of the req (L1I/L1D/PTW)
case object ReqSourceKey extends ControlKey[UInt]("reqSource")

case class ReqSourceField() extends BundleField[UInt](ReqSourceKey, Output(UInt(MemReqSource.reqSourceBits.W)), x =>
  x := MemReqSource.NoWhere.id.U(MemReqSource.reqSourceBits.W)
)

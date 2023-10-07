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

package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
// import difftest._
// import xiangshan.backend.execute.exu.{ExuConfig, ExuType}
// import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
// import utils._
import xiangshan._
// import xiangshan.backend.writeback._
// import xiangshan.frontend.FtqPtr
import xs.utils._

class RobPtr(implicit p: Parameters) extends CircularQueuePtr[RobPtr](
  p => p(XSCoreParamsKey).RobSize
) with HasCircularQueuePtrHelper {

  def needFlush(redirect: Valid[Redirect]): Bool = {
    // val flushItself = redirect.bits.flushItself() && this === redirect.bits.robIdx
    // redirect.valid && (flushItself || isAfter(this, redirect.bits.robIdx))
    redirect.valid && isAfter(this, redirect.bits.robIdx)
  }

  def needFlush(redirect: Seq[Valid[Redirect]]): Bool = VecInit(redirect.map(needFlush)).asUInt.orR
}
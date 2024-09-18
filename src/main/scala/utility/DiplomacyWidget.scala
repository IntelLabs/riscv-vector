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
import freechips.rocketchip.diplomacy.{BundleBridgeNexusNode, LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters

class ValidIOBroadcast[T <: Data]()(implicit p: Parameters) extends LazyModule {
  val node = BundleBridgeNexusNode[ValidIO[T]]()
  lazy val module = new LazyModuleImp(this) {
    require(node.in.size == 1)
    require(node.out.nonEmpty)
    val input = node.in.head._1
    val outputs = node.out.map(_._1)
    for (o <- outputs) {
      o.valid := input.valid
      o.bits := input.bits
    }
  }
  // TODO: adapt to tp-meta requirements
}

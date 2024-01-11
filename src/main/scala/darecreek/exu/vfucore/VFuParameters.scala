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
import chipsalliance.rocketchip.config._

case object VFuParamsKey extends Field[VFuParameters]

case class VFuParameters
(
  XLEN: Int = 64,
  FLEN: Int = 64,
  VLEN: Int = 128
)

trait HasVFuParameters {
  implicit val p: Parameters
  
  val vfuParams = p(VFuParamsKey)

  val XLEN = vfuParams.XLEN
  val FLEN = vfuParams.FLEN
  require(XLEN == FLEN)
  val VLEN = vfuParams.VLEN
  val bVL = log2Up(VLEN) + 1
  val bVSTART = bVL - 1
  val LaneWidth = 64  // constant
  def laneWidth = LaneWidth
  val NLanes = VLEN / LaneWidth  // must be power of 2
  val vlenb = VLEN / 8  //CSR
  def VLENB = vlenb
  val vlenbWidth = log2Up(vlenb)
}

abstract class VFuModule(implicit val p: Parameters) extends Module with HasVFuParameters
abstract class VFuBundle(implicit val p: Parameters) extends Bundle with HasVFuParameters

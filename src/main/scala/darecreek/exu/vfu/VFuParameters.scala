package darecreek.exu.vfu

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

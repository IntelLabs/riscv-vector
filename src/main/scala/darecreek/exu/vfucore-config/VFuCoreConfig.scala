package darecreek.exu.vfucoreconfig

import chisel3._
import chisel3.util._
import darecreek.VExpdUOp

/**
  * Choose which VUop/Redirect as the final ones
  */
class VUop extends VUop_darecreek
class Redirect extends Redirect_darecreek


/**
 *  Dare Creek
 */
class VUop_darecreek extends VExpdUOp {
  def uopIdx = expdIdx
  def uopEnd = expdEnd
  def robIdx = 0.U
}
class Redirect_darecreek extends darecreek.Redirect {
  def needFlush(robIdx: UInt) = valid // Darecreek flush all instrns
}
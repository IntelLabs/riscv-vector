package darecreek.exu.vfucoreconfig

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
}
class Redirect_darecreek extends darecreek.Redirect
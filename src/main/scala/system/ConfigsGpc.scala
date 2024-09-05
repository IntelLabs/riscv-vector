// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package gpc.system

import org.chipsalliance.cde.config.Config
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import gpc.subsystem._

class DefaultConfig extends Config(new WithNBigCoresGpc(1) ++ new WithCoherentBusTopology ++ new BaseConfig)

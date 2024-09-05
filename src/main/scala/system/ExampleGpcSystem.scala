// See LICENSE.SiFive for license details.

package gpc.system

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util.DontTouch

import gpc.subsystem._
import freechips.rocketchip.system._

/** Example Top with periphery devices and ports, and a Gpc subsystem */
class ExampleGpcSystem(implicit p: Parameters) extends GpcSubsystem
    with HasAsyncExtInterrupts
    with CanHaveMasterAXI4MemPort
    with CanHaveMasterAXI4MMIOPort
    with CanHaveSlaveAXI4Port
{
  // optionally add ROM devices
  // Note that setting BootROMLocated will override the reset_vector for all tiles
  val bootROM  = p(BootROMLocated(location)).map { BootROM.attach(_, this, CBUS) }
  val maskROMs = p(MaskROMLocated(location)).map { MaskROM.attach(_, this, CBUS) }

  override lazy val module = new ExampleGpcSystemModuleImp(this)
}

class ExampleGpcSystemModuleImp[+L <: ExampleGpcSystem](_outer: L) extends GpcSubsystemModuleImp(_outer)
    with HasRTCModuleImp
    with HasExtInterruptsModuleImp
    with DontTouch

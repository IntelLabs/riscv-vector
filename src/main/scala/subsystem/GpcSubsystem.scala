// See LICENSE.SiFive for license details.

package gpc.subsystem

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing}
import freechips.rocketchip.tile._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug}
import freechips.rocketchip.util.{HasCoreMonitorBundles}
import freechips.rocketchip.devices.tilelink.{CanHavePeripheryCLINT, CanHavePeripheryPLIC}

import gpc.tile._
import gpc.core._
import freechips.rocketchip.subsystem._

case class GpcCrossingParams(
  crossingType: ClockCrossingType = SynchronousCrossing(),
  master: HierarchicalElementPortParamsLike = HierarchicalElementMasterPortParams(),
  slave: HierarchicalElementSlavePortParams = HierarchicalElementSlavePortParams(),
  mmioBaseAddressPrefixWhere: TLBusWrapperLocation = CBUS,
  resetCrossingType: ResetCrossingType = NoResetCrossing(),
  forceSeparateClockReset: Boolean = false
) extends HierarchicalElementCrossingParamsLike

case class GpcTileAttachParams(
  tileParams: GpcTileParams,
  crossingParams: GpcCrossingParams
) extends CanAttachTile { type TileType = GpcTile }

trait HasGpcTiles {
  this: BaseSubsystem with InstantiatesHierarchicalElements =>
  val gpcTiles = totalTiles.values.collect { case r: GpcTile => r }

  def coreMonitorBundles = (gpcTiles map { t =>
    t.module.core.gpcImpl.coreMonitorBundle
  }).toList
}

class GpcSubsystem(implicit p: Parameters) extends BaseSubsystem
    with InstantiatesHierarchicalElements
    with HasTileNotificationSinks
    with HasTileInputConstants
    with CanHavePeripheryCLINT
    with CanHavePeripheryPLIC
    with HasPeripheryDebug
    with HasHierarchicalElementsRootContext
    with HasHierarchicalElements
    with HasCoreMonitorBundles
    with HasGpcTiles
{
  override lazy val module = new GpcSubsystemModuleImp(this)
}

class GpcSubsystemModuleImp[+L <: GpcSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
    with HasHierarchicalElementsRootContextModuleImp


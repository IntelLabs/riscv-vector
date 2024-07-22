package grapecoveDCache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class BaseDCache()(
    implicit p: Parameters
) extends LazyModule {

  protected def cacheClientParameters = Seq(TLMasterParameters.v1(
    name = "Dcache",
    sourceId = IdRange(0, 1 max nMSHRs + nWBQEntries),
    supportsProbe = TransferSizes(blockBytes, blockBytes),
  ))

  protected def mmioClientParameters = Seq(TLMasterParameters.v1(
    name = "Dcache MMIO",
    sourceId = IdRange(nMSHRs + nWBQEntries, nMSHRs + nWBQEntries + nMMIOs),
    requestFifo = true,
  ))

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = cacheClientParameters,
    minLatency = 1,
  )))

  lazy val module = new BaseDCacheImp(this)
}

class BaseDCacheImp(outer: BaseDCache) extends LazyModuleImp(outer) {
  implicit val edge        = outer.node.edges.out(0)
  val (tl_out, _)          = outer.node.out(0)
  private val fifoManagers = edge.manager.managers.filter(TLFIFOFixer.allVolatile)
  fifoManagers.foreach { m =>
    require(
      m.fifoId == fifoManagers.head.fifoId,
      s"IOMSHRs must be FIFO for all regions with effects, but Cache sees\n" +
        s"${m.nodePath.map(_.name)}\nversus\n${fifoManagers.head.nodePath.map(_.name)}",
    )
  }
  val io = IO(new DataExchangeIO())
}

package gpc.core

import gpc.core.GPCDCacheWrapper
import grapecoveDCache._
import chisel3._
import chisel3.util.{isPow2, log2Ceil, log2Up, Decoupled, Valid}
import chisel3.dontTouch
import freechips.rocketchip.amba._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import scala.collection.mutable.ListBuffer

class MemBlockReq(
    implicit p: Parameters
) extends CoreBundle()(p) {
  val source  = UInt(MasterSource.width.W)        // INT/FP/VECTOR?
  val addr    = UInt(vaddrWidth.W)
  val cmd     = UInt(grapecoveDCache.M_SZ.W)
  val size    = UInt(log2Up(log2Up(dataBytes)).W) // 1B->size = 0; 2B->1; ...; 64B->6
  val phys    = Bool()                            // is physical address?
  val signed  = Bool()                            // load data sign extendion
  val dprv    = UInt(PRV.SZ.W)
  val dv      = Bool()
  val noAlloc = Bool()                            // cache no allocate
  val dest    = UInt(destWidth.W)                 // reg addr or lsq idx
}

class MemBlockResp(
    implicit p: Parameters
) extends CoreBundle()(p) {
  val source  = UInt(MasterSource.width.W)
  val dest    = UInt(destWidth.W)
  val status  = CacheRespStatus()
  val hasData = Bool()
  val data    = UInt(dataWidth.W)
}

class MemBlockIO(
    implicit p: Parameters
) extends CoreBundle()(p) {
  val req  = Decoupled(new MemBlockReq)
  val resp = Flipped(Valid(new MemBlockResp))

  val asid     = Output(UInt((asIdBits max 1).W)) // for sfence
  val s1_wdata = Output(UInt(dataWidth.W))
  val s1_wmask = Output(UInt(dataBytes.W))

  val s1_kill     = Output(Bool())
  val s2_kill     = Output(Bool())
  val s2_xcpt     = Input(new HellaCacheExceptions)
  val fenceRdy    = Input(Bool()) // all cache reqs has done
  val nextCycleWb = Input(Bool())
}

class DCacheTLBPort(
    implicit p: Parameters
) extends CoreBundle()(p) {
  val req     = Flipped(Decoupled(new TLBReq(coreDataBytes.log2)))
  val s1_resp = Output(new TLBResp)
  val s2_kill = Input(Bool())
}

abstract class MemBlock(tileId: Int)(
    implicit p: Parameters
) extends LazyModule
    with HasNonDiplomaticTileParameters {
  protected val cfg = tileParams.dcache.get

  protected def cacheClientParameters = cfg.scratch.map(x => Seq()).getOrElse(Seq(TLMasterParameters.v1(
    name = s"Core ${tileId} DCache",
    sourceId = IdRange(0, 1 max 16), // FIXME
    supportsProbe = TransferSizes(cfg.blockBytes, cfg.blockBytes),
  )))

  protected def mmioClientParameters = Seq(TLMasterParameters.v1(
    name = s"Core ${tileId} DCache MMIO",
    sourceId = IdRange(firstMMIO, firstMMIO + 8), // FIXME
    requestFifo = true,
  ))

  def firstMMIO = (cacheClientParameters.map(_.sourceId.end) :+ 0).max

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = cacheClientParameters ++ mmioClientParameters,
    minLatency = 1,
    requestFields = tileParams.core.useVM.option(Seq()).getOrElse(Seq(AMBAProtField())),
  )))

  val hartIdSinkNodeOpt            = cfg.scratch.map(_ => BundleBridgeSink[UInt]())
  val mmioAddressPrefixSinkNodeOpt = cfg.scratch.map(_ => BundleBridgeSink[UInt]())

  val module: MemBlockModule

  def flushOnFenceI = cfg.scratch.isEmpty && !node.edges.out(0).manager.managers.forall(m =>
    !m.supportsAcquireB || !m.executable || m.regionType >= RegionType.TRACKED || m.regionType <= RegionType.IDEMPOTENT
  )

  def canSupportCFlushLine = !usingVM || cfg.blockBytes * cfg.nSets <= (1 << pgIdxBits)

  require(!tileParams.core.haveCFlush || cfg.scratch.isEmpty, "CFLUSH_D_L1 instruction requires a D$")
}

class MemBlockBundle(val outer: MemBlock)(
    implicit p: Parameters
) extends CoreBundle()(p) {
  val cpu      = Flipped(new MemBlockIO)
  val ptw      = new TLBPTWIO()
  val tlb_port = new DCacheTLBPort
}

class MemBlockModule(outer: MemBlock) extends LazyModuleImp(outer) {
  implicit val edge          = outer.node.edges.out(0)
  val (tl_out, _)            = outer.node.out(0)
  val io                     = IO(new MemBlockBundle(outer))
  val io_hartid              = outer.hartIdSinkNodeOpt.map(_.bundle)
  val io_mmio_address_prefix = outer.mmioAddressPrefixSinkNodeOpt.map(_.bundle)
  dontTouch(io.cpu.resp) // Users like to monitor these fields even if the core ignores some signals

  require(rowBits == edge.bundle.dataBits)

  private val fifoManagers = edge.manager.managers.filter(TLFIFOFixer.allVolatile)
  fifoManagers.foreach { m =>
    require(
      m.fifoId == fifoManagers.head.fifoId,
      s"IOMSHRs must be FIFO for all regions with effects, but MemBlock sees\n" +
        s"${m.nodePath.map(_.name)}\nversus\n${fifoManagers.head.nodePath.map(_.name)}",
    )
  }
}

/** Support overriding which MemBlock is instantiated */

case object BuildMemBlock extends Field[BaseTile => Parameters => MemBlock](MemBlockFactory.apply)

object MemBlockFactory {
  def apply(tile: BaseTile)(p: Parameters): MemBlock =
    new GPCDCacheWrapper(tile.tileId)(p)
}

/** Mix-ins for constructing tiles that have a MemBlock */

trait HasMemBlock { this: BaseTile =>
  val module:     HasMemBlockModule
  implicit val p: Parameters
  var nDCachePorts = 0
  lazy val dcache: MemBlock = LazyModule(p(BuildMemBlock)(this)(p))

  tlMasterXbar.node := TLWidthWidget(tileParams.dcache.get.rowBits / 8) := dcache.node
  dcache.hartIdSinkNodeOpt.map(_ := hartIdNexusNode)
  dcache.mmioAddressPrefixSinkNodeOpt.map(_ := mmioAddressPrefixNexusNode)
  InModuleBody {
    dcache.module.io.tlb_port := DontCare
  }
}

trait HasMemBlockModule {
  val outer:      HasMemBlock with HasTileParameters
  implicit val p: Parameters
  val dcachePorts = ListBuffer[MemBlockIO]()
  val dcacheArb   = Module(new MemBlockArbiter(outer.nDCachePorts)(outer.p))
  outer.dcache.module.io.cpu <> dcacheArb.io.mem
}

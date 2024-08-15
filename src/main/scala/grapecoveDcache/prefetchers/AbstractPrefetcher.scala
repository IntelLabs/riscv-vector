package grapecoveDcache

// ucb-bar bar-fetchers
// https://github.com/ucb-bar/bar-fetchers/blob/master/src/main/scala/AbstractPrefetcher.scala

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.CacheBlockBytes
import grapecoveDCache.DCacheParams
import grapecoveDCache.MemoryOpConstants._

trait CanInstantiatePrefetcher {
  def desc: String
  def instantiate()(
      implicit p: Parameters
  ): AbstractPrefetcher
}

class PrefetchTrain(
    implicit val p: Parameters
) extends Bundle with DCacheParams {
  val pc            = UInt(64.W)
  val vaddr         = UInt(vaddrWidth.W)
  val paddr         = UInt(lineAddrWidth.W)
  val cmd           = UInt(M_SZ.W)
  val isMiss        = Bool()
  val isPrefetchHit = Bool()
}

class PrefetchRequest(
    implicit val p: Parameters
) extends Bundle with DCacheParams {
  val vaddr          = UInt(vaddrWidth.W)
  val paddr          = UInt(paddrWidth.W)
  val write          = Bool()
  val prefetchSource = UInt(2.W) // TODO
}

class PrefetcherIO(
    implicit p: Parameters
) extends Bundle {
  val train = Flipped(ValidIO(new PrefetchTrain))
  val req   = DecoupledIO(new PrefetchRequest)
}

abstract class AbstractPrefetcher(
    implicit p: Parameters
) extends Module with DCacheParams {
  val io = IO(new PrefetcherIO)

  io.req.valid      := false.B
  io.req.bits       := DontCare
  io.req.bits.vaddr := 0.U(1.W)

}

case class NullPrefetcherParams() extends CanInstantiatePrefetcher {
  def desc() = "Null Prefetcher"
  def instantiate()(
      implicit p: Parameters
  ) = Module(new NullPrefetcher()(p))
}

class NullPrefetcher(
    implicit p: Parameters
) extends AbstractPrefetcher()(p)

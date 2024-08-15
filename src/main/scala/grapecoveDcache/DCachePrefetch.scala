package grapecoveDcache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Config, Field, Parameters}
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.tile.BaseTile
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.diplomacy._
import grapecoveDCache._

class DCachePrefetchWrapper(prefetcher: CanInstantiatePrefetcher)(
    implicit p: Parameters
) extends BaseDCache {
  val cache                = LazyModule(new GPCDCache()(p))
  override val node        = cache.node
  override lazy val module = new DCachePrefetchWrapperModule(prefetcher, this)
}

class DCachePrefetchWrapperModule(pP: CanInstantiatePrefetcher, outer: DCachePrefetchWrapper)
    extends BaseDCacheImp(outer)
    with DCacheParams with MemoryOpConstants {

  outer.cache.module.io <> io

  val cache = outer.cache.module
  require(nMSHRs > 0, "Prefetcher must be used with a non-blocking L1")

  val prefetcher = pP.instantiate()

  // s1 prefetcher train
  val s1_isNotSWPf = ShiftRegister(io.req.fire && !isPrefetch(io.req.bits.cmd), 1)
  val trainValid   = s1_isNotSWPf && io.resp.valid && (io.resp.bits.status =/= CacheRespStatus.replay)

  prefetcher.io.train.valid              := trainValid
  prefetcher.io.train.bits.pc            := 0.U                                 // FIXME
  prefetcher.io.train.bits.vaddr         := ShiftRegister(io.req.bits.paddr, 1) // FIXME
  prefetcher.io.train.bits.paddr         := ShiftRegister(io.req.bits.paddr, 1)
  prefetcher.io.train.bits.cmd           := ShiftRegister(io.req.bits.cmd, 1)
  prefetcher.io.train.bits.isMiss        := (io.resp.bits.status === CacheRespStatus.miss)
  prefetcher.io.train.bits.isPrefetchHit := false.B

  // prefetch queue
  val req = Queue(prefetcher.io.req, 1)

  val inFlight = RegInit(false.B)

  req.ready := false.B

  // send prefetch request to cache
  when(!io.req.valid) {
    cache.io.req.valid       := req.valid && !inFlight
    cache.io.req.bits        := DontCare
    cache.io.req.bits.paddr  := req.bits.paddr
    cache.io.req.bits.cmd    := Mux(req.bits.write, M_PFW, M_PFR)
    cache.io.req.bits.size   := 6.U
    cache.io.req.bits.signed := false.B
    cache.io.s0_kill         := false.B

    when(cache.io.req.fire)(inFlight := true.B)
  }

  val prefetchFire    = cache.io.req.fire && isPrefetch(cache.io.req.bits.cmd)
  val s1_prefetchAddr = RegEnable(req.bits.paddr, prefetchFire)

  when(ShiftRegister(prefetchFire, 1)) {
    cache.io.s1_kill := false.B
    val paddr = s1_prefetchAddr
    val legal = cache.edge.manager.findSafe(paddr).reduce(_ || _)
    val prefetchable = cache.edge.manager.fastProperty(
      paddr,
      _.supportsAcquireT,
      (b: TransferSizes) => (!b.none).B,
    )
    cache.io.s1_kill := !legal || !prefetchable
    req.ready        := (cache.io.resp.bits.status =/= CacheRespStatus.replay)
    inFlight         := false.B
  }
}

package coincreekDCache

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import org.scalatest.matchers.must.Matchers
import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

case class CacheReqBundle(
    source:    Int = 0,
    paddr:     String = "h0",
    size:      Int = 2,
    signed:    Boolean = false,
    wdata:     String = "h0",
    wmask:     String = "h0",
    noAlloc:   Boolean = false,
    dest:      Int = 0,
    isRefill:  Boolean = false,
    refillWay: Int = 0,
    cmd:       UInt = 0.U,
    refillCoh: UInt = 0.U,
)

trait BundleGenHelper {
  def genReq(r: CacheReqBundle) =
    (new DataExchangeReq).Lit(
      _.source    -> r.source.U,
      _.paddr     -> r.paddr.U,
      _.size      -> r.size.U,
      _.signed    -> r.signed.B,
      _.wdata     -> r.wdata.U,
      _.wmask     -> r.wmask.U,
      _.noAlloc   -> r.noAlloc.B,
      _.dest      -> r.dest.U,
      _.isRefill  -> r.isRefill.B,
      _.refillWay -> r.refillWay.U,
      _.cmd       -> r.cmd,
      _.refillCoh -> r.refillCoh,
    )
}

object DCacheInit extends BundleGenHelper {
  val initData =
    "h0123456789abcdef_fedcba9876543210_0011223344556677_8899aabbccddeeff_7766554433221100_ffeeddccbbaa9988_1010101010101010_2323232323232323"

  def initDut(dut: DCacheWrapperImp): Unit = {
    dut.io.req.valid.poke(false.B)
    dut.clock.step(150)

    val cacheRefillDefault = CacheReqBundle(
      size = 6,
      wdata = initData,
      isRefill = true,
    )

    val cacheRefillSeq = Seq(
      cacheRefillDefault.copy(
        paddr = "h80004000",
        refillWay = 0,
        refillCoh = ClientStates.Trunk,
      ),
      cacheRefillDefault.copy(
        paddr = "h80006000",
        refillWay = 1,
        refillCoh = ClientStates.Dirty,
      ),
      cacheRefillDefault.copy(
        paddr = "h80008000",
        refillWay = 2,
        refillCoh = ClientStates.Trunk,
      ),
      cacheRefillDefault.copy(
        paddr = "h8000a000",
        refillWay = 3,
        refillCoh = ClientStates.Branch,
      ),
    )

    cacheRefillSeq.foreach { req =>
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.poke(genReq(req))
      dut.clock.step(1)
    }

    dut.clock.step(10)
  }
}

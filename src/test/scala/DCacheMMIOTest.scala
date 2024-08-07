package grapecoveDCache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import MemoryOpConstants._

trait DCacheMMIOTestTrait {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val cacheReqDefault = CacheReqBundle()

  implicit val valName = ValName("DCacheMMIOTest")

  def cacheTest0(): Unit =
    it should "pass: cache mmio load" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReadReq = CacheReqBundle(
          paddr = "h6000c000",
          cmd = M_XRD,
          dest = 16,
        )

        // req miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)
        dut.clock.step(1)

        // refill resp
        while (!dut.io.resp.valid.peekBoolean()) {
          dut.clock.step(1)
        }
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.refill)
        dut.io.resp.bits.data.expect(0.U)

        dut.clock.step(10)

        // read miss after refill
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)
        dut.clock.step(1)

        // refill resp
        while (!dut.io.resp.valid.peekBoolean()) {
          dut.clock.step(1)
        }
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.refill)
        dut.io.resp.bits.data.expect(0.U)
        dut.clock.step(10)
      }
    }

  def cacheTest1(): Unit =
    it should "pass: cache mmio store" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReq = CacheReqBundle(
          paddr = "h6000c000",
          cmd = M_XRD,
        )

        // req miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          cmd = M_XWR,
          wdata = "h7890",
        )))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)
        dut.clock.step(1)

        // read miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)
        dut.clock.step(1)

        // refill resp
        while (!dut.io.resp.valid.peekBoolean()) {
          dut.clock.step(1)
        }
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.refill)
        dut.io.resp.bits.data.expect("h7890".U)
        dut.clock.step(10)
      }
    }
}

class DCacheMMIOTest extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with DCacheMMIOTestTrait {
  behavior of "DCache MMIO Test"

  it should behave like cacheTest0() //
  it should behave like cacheTest1() //
}

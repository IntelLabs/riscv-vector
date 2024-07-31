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

trait DCacheBypassTestTrait {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val cacheReqDefault = CacheReqBundle()

  implicit val valName = ValName("DCacheBypassTest")

  def cacheTest0(): Unit =
    it should "pass: cache bypass load" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReadReq = CacheReqBundle(
          size = 6,
          paddr = "h8000f000",
          noAlloc = true,
          cmd = M_XRD,
        )

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

        // read miss again because of no alloc
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
    it should "pass: cache bypass store" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReqDefault = CacheReqBundle(
          size = 6,
          paddr = "h8000f000",
          noAlloc = true,
          cmd = M_XRD,
        )

        // write no alloc
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReqDefault.copy(
          cmd = M_XWR,
          wdata = "h12345678",
        )))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        dut.clock.step(200)

        // read miss  because of no alloc
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReqDefault))

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
        dut.io.resp.bits.data.expect("h12345678".U)
        dut.clock.step(10)
      }
    }

  def cacheTest2(): Unit =
    it should "pass: cache bypass partial masked store" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReqDefault = CacheReqBundle(
          size = 6,
          paddr = "h8000f000",
          noAlloc = true,
          cmd = M_XRD,
        )

        // write no alloc
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReqDefault.copy(
          cmd = M_PWR,
          wdata = "h0101010112345678",
          wmask = "hf0",
        )))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        dut.clock.step(200)

        // read miss  because of no alloc
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReqDefault))

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
        dut.io.resp.bits.data.expect("h0101010100000000".U)
        dut.clock.step(10)
      }
    }
}
class DCacheBypassTest extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper
    with DCacheBypassTestTrait {
  behavior of "DCache Bypass DCache Test"

  it should behave like cacheTest0() //
  it should behave like cacheTest1() //
  it should behave like cacheTest2() //
}

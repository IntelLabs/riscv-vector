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

trait DCacheTestTrait {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val cacheReqDefault = CacheReqBundle()

  implicit val valName = ValName("DCacheTest")

  def cacheTest0(): Unit =
    it should "pass: cache load hit" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReadReq = CacheReqBundle(
          size = 6,
          paddr = "h80004000",
          cmd = M_XRD,
        )

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect(DCacheInit.initData.U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(10)

      }
    }

  def cacheTest1(): Unit =
    it should "pass: cache load hit (different sizes and sign/unsigned)" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReadReq = CacheReqBundle(
          paddr = "h80004000",
          cmd = M_XRD,
        )

        // read hit 512
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq.copy(size = 6, signed = false)))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect(DCacheInit.initData.U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(10)

        // read hit 64
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq.copy(size = 3, signed = false)))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h2323232323232323".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(10)

        // read hit 32
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq.copy(
          paddr = "h80004010",
          size = 2,
          signed = true,
        )))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect(
          "hffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffbbaa9988".U
        )
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(10)

      }
    }

  def cacheTest2(): Unit =
    it should "pass: cache load miss" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReadReq = CacheReqBundle(
          paddr = "h8000c000",
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
        dut.io.resp.bits.dest.expect(16.U)
        dut.clock.step(10)

        // read hit after refill
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect(0.U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(1)

        dut.clock.step(200) // writeback

        // read miss after replace
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq.copy(
          paddr = "h80004000",
          size = 0,
        )))

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
        dut.io.resp.bits.data.expect("h23".U)
        dut.io.resp.bits.dest.expect(16.U)
        dut.clock.step(10)

      }
    }

  def cacheTest3(): Unit =
    it should "pass: cache store miss" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        // req miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(CacheReqBundle(
          paddr = "h8000c001",
          size = 0,
          cmd = M_XWR,
          wdata = "h78",
        )))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)
        dut.clock.step(1)

        // wait store
        dut.clock.step(200)

        // read hit after refill
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(CacheReqBundle(
          paddr = "h8000c000",
          cmd = M_XRD,
        )))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h7800".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(1)
      }
    }

  def cacheTest4(): Unit =
    it should "pass: cache partial store miss" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        // req miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(CacheReqBundle(
          paddr = "h8000a000",
          cmd = M_PWR,
          size = 6,
          wdata = "h0101010112345678",
          wmask = "hf0",
        )))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)
        dut.clock.step(1)

        // wait store
        dut.clock.step(200)

        // read hit after refill
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(CacheReqBundle(
          paddr = "h8000a000",
          cmd = M_XRD,
          size = 3,
        )))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h0101010123232323".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
        dut.clock.step(1)
      }
    }

  def cacheTest5(): Unit =
    it should "pass: block miss that occurs in WBQ" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        val cacheReadReq = CacheReqBundle(
          paddr = "h80004000",
          cmd = M_XRD,
        )

        // refill
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq.copy(
          paddr = "h8000c000",
          isRefill = true,
          refillWay = 1,
          refillCoh = ClientStates.Dirty,
          size = 6,
          wdata = "h12345678",
        )))

        // req miss need replay
        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReadReq))

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.replay)
        dut.clock.step(50)

      }
    }

}
class DCacheTest extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with DCacheTestTrait {
  behavior of "DCache Test"

  it should behave like cacheTest0() //
  it should behave like cacheTest1() //
  it should behave like cacheTest2() //
  it should behave like cacheTest3() //
  it should behave like cacheTest4() //
  it should behave like cacheTest5() //
}

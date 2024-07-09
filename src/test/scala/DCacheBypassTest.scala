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

trait DCacheBypassTestTrait {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val cacheReqDefault = CacheReqBundle()

  implicit val valName = ValName("DCacheBypassTest")

  def cacheTest0(): Unit =
    it should "pass: N->Dirty store->load bypassing" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        val cacheReq = CacheReqBundle(
          paddr = "h80004000",
          cmd = MemoryOpConstants.M_XRD,
        )

        DCacheInit.initDut(dut)

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          cmd = MemoryOpConstants.M_XWR,
          wdata = "h12345678",
        )))

        dut.clock.step(1)
        // s2->s1 store load bypass
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h12345678".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)

        // s3->s1 store load bypass
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h12345678".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)

        // directly read meta array
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h12345678".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
      }
    }

  def cacheTest1(): Unit =
    it should "pass: T->Dirty store->load bypassing" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        val cacheReq = CacheReqBundle(
          paddr = "h80008000",
          cmd = MemoryOpConstants.M_XRD,
        )

        DCacheInit.initDut(dut)

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          cmd = MemoryOpConstants.M_XWR,
          wdata = "h12345678",
        )))

        dut.clock.step(1)
        // s2->s1 store load bypass
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h12345678".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)

        // s3->s1 store load bypass
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h12345678".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)

        // directly read meta array
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h12345678".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.hit)
      }
    }

  def cacheTest2(): Unit =
    it should "pass: B->Dirty store->load bypassing" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        val cacheReq = CacheReqBundle(
          paddr = "h8000a000",
          cmd = MemoryOpConstants.M_XRD,
        )

        DCacheInit.initDut(dut)

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          cmd = MemoryOpConstants.M_XWR,
          wdata = "h12345678",
        )))

        dut.clock.step(1)
        // s2->s1 store load bypass
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        // s3->s1 store load bypass
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect("h12345678".U)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        // directly read meta array
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)
      }
    }

  def cacheTest3(): Unit =
    it should "pass: replace->load bypassing" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        val cacheReq = CacheReqBundle(
          paddr = "h80004000",
          cmd = MemoryOpConstants.M_XRD,
        )

        DCacheInit.initDut(dut)

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.poke(genReq(cacheReq.copy(
          paddr = "h8000c000",
          isRefill = true,
          refillWay = 0,
          refillCoh = ClientStates.Dirty,
          wdata = "h12345678",
        )))

        dut.clock.step(1)
        // s2->s1 replace load bypass
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        // s3->s1 replace load bypass
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)

        // directly read meta array
        dut.io.req.bits.poke(genReq(cacheReq))

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.status.expect(CacheRespStatus.miss)
      }

    }

  def cacheTest4(): Unit =
    it should "pass: probe->load bypassing" in {
      test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        DCacheInit.initDut(dut)

        // TODO
      }
    }
}

class DCacheBypassTest extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with DCacheBypassTestTrait {
  behavior of "DCache Bypass Test"

  it should behave like cacheTest0() //
  it should behave like cacheTest1() //
  it should behave like cacheTest2() //
  it should behave like cacheTest3() //
  it should behave like cacheTest4() //
}

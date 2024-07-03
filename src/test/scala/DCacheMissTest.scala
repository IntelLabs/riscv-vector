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

class DcacheMissTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("DCacheWrapper")

  val initilizeData =
    "h0123456789abcdef_fedcba9876543210_0011223344556677_8899aabbccddeeff_7766554433221100_ffeeddccbbaa9988_1010101010101010_2323232323232323"

  def initializeDut(dut: DCacheWrapperImp): Unit = {
    dut.io.req.valid.poke(false.B)
    dut.clock.step(150)

    dut.io.req.valid.poke(true.B)
    dut.io.req.bits.source.poke(1.U)
    dut.io.req.bits.paddr.poke("h80004000".U)
    dut.io.req.bits.cmd.poke(0.U) // dontcare
    dut.io.req.bits.size.poke(6.U)
    dut.io.req.bits.signed.poke(false.B)
    dut.io.req.bits.wdata.poke(initilizeData.U)
    dut.io.req.bits.wmask.poke(0.U)
    dut.io.req.bits.noAlloc.poke(false.B)
    dut.io.req.bits.isRefill.poke(true.B)
    dut.io.req.bits.refillWay.poke(1.U)
    dut.io.req.bits.refillCoh.poke(ClientStates.Dirty)

    dut.clock.step(1)
    dut.io.req.bits.paddr.poke("h80006000".U)
    dut.io.req.bits.refillWay.poke(2.U)

    dut.clock.step(1)
    dut.io.req.bits.paddr.poke("h80008000".U)
    dut.io.req.bits.refillWay.poke(3.U)

    dut.clock.step(1)
    dut.io.req.bits.paddr.poke("h8000a000".U)
    dut.io.req.bits.refillWay.poke(0.U)

    dut.clock.step(1)
    dut.io.req.valid.poke(false.B)
    dut.io.req.bits.isRefill.poke(false.B)
    dut.clock.step(5)
  }

  it should "DCache miss" in {
    test(LazyModule(new DCacheWrapper()(Parameters.empty)).module).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      initializeDut(dut)

      // read hit
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.signed.poke(false.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke("h80004000".U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect(initilizeData.U)
      dut.io.resp.bits.status.expect(CacheRespStatus.hit)
      dut.clock.step(10)

      // read miss
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke("h8000c000".U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.status.expect(CacheRespStatus.miss)
      dut.clock.step(1)

      while (!dut.io.resp.valid.peekBoolean()) {
        dut.clock.step(1)
      }
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.status.expect(CacheRespStatus.refill)
      dut.io.resp.bits.data.expect("h22334455".U)
      dut.clock.step(10)

      // read hit after miss
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke("h8000c000".U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect("h22334455".U)
      dut.io.resp.bits.status.expect(CacheRespStatus.hit)
      dut.clock.step(1)
    }
  }

}

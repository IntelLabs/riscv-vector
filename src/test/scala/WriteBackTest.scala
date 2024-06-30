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

class WriteBackTest extends AnyFlatSpec with ChiselScalatestTester {
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
    dut.io.req.valid.poke(false.B)
    dut.io.req.bits.isRefill.poke(false.B)
    dut.clock.step(5)
  }

  it should "Replace" in {
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
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(10)

      // replace
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.isRefill.poke(true.B)
      dut.io.req.bits.paddr.poke("h80004000".U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.req.bits.isRefill.poke(false.B)
      dut.clock.step(10)

      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke("h80004000".U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.hit.expect(false.B)
      dut.clock.step(10)

      // check waveform
    }
  }

}

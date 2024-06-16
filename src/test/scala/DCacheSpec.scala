package coincreekDCache

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import org.scalatest.matchers.must.Matchers
import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}

class DCacheSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("DCache")

  it should "DCache hit" in {
    test(new DCache()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) {
      dut =>
        dut.clock.step(150)

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.source.poke(1.U)
        dut.io.req.bits.cmd.poke(MemoryOpConstants.M_FILL)
        dut.io.req.bits.paddr.poke(0x1234000.U)
        dut.io.req.bits.wdata.poke(0x5678.U)
        dut.io.req.bits.specifyValid.poke(true.B)
        dut.io.req.bits.specifyWay.poke(1.U)

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.clock.step(5)
        // dut.io.resp.valid.expect(true.B)

        // read hit
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.source.poke(1.U)
        dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
        dut.io.req.bits.paddr.poke(0x1234000.U)
        dut.io.req.bits.wdata.poke(0x5678.U)

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect(0x5678.U)
        dut.io.resp.bits.hit.expect(true.B)
        dut.clock.step(10)

        // read miss
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.source.poke(1.U)
        dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
        dut.io.req.bits.paddr.poke(0x2234000.U)
        dut.io.req.bits.wdata.poke(0x5678.U)

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.hit.expect(false.B)
        dut.clock.step(10)

    }
  }
  it should "Store -> Load Bypassing" in {
    test(new DCache()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) {
      dut =>
        dut.clock.step(150)

        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.source.poke(1.U)
        dut.io.req.bits.cmd.poke(MemoryOpConstants.M_FILL)
        dut.io.req.bits.paddr.poke(0x1234000.U)
        dut.io.req.bits.wdata.poke(0x5678.U)
        dut.io.req.bits.specifyValid.poke(true.B)
        dut.io.req.bits.specifyWay.poke(1.U)

        dut.clock.step(1)
        dut.io.req.valid.poke(false.B)
        dut.clock.step(5)
        // dut.io.resp.valid.expect(true.B)

        // write hit
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.source.poke(1.U)
        dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XWR)
        dut.io.req.bits.paddr.poke(0x1234000.U)
        dut.io.req.bits.wdata.poke(0x7890.U)

        dut.clock.step(1)
        // read hit
        dut.io.req.valid.poke(true.B)
        dut.io.req.bits.source.poke(1.U)
        dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
        dut.io.req.bits.paddr.poke(0x1234000.U)
        dut.io.req.bits.wdata.poke(0x7890.U)

        dut.clock.step(1)
        dut.io.resp.valid.expect(true.B)
        dut.io.resp.bits.data.expect(0x7890.U)
        dut.io.resp.bits.hit.expect(true.B)
        dut.clock.step(10)

    }
  }
}

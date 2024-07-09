package coincreekDCache

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import org.scalatest.matchers.must.Matchers
import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}

class MetaArrayTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("MetaArray")

  it should "pass: write and read metadata " in {
    test(new MetaArray[Metadata](Metadata.onReset)).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) {
      dut =>
        // for reset
        dut.clock.step(150)

        dut.io.write.valid.poke(true.B)
        dut.io.write.bits.setIdx.poke(10.U)
        dut.io.write.bits.wayEn.poke(2.U)
        // Assuming MetaWriteReq has a 'data' field of type Metadata or similar
        dut.io.write.bits.data.tag.poke(42.U)
        dut.io.write.bits.data.coh.poke(3.U)
        dut.clock.step(1)
        dut.io.write.valid.poke(false.B)

        dut.clock.step(10)
        // Simulate a read operation
        dut.io.read.valid.poke(true.B)
        dut.io.read.bits.setIdx.poke(10.U)
        dut.io.read.bits.wayEn.poke(2.U)

        dut.clock.step(1)
        dut.io.read.valid.poke(false.B)
        // Assuming the response is checked after the read operation
        dut.io.resp(1).tag.expect(42.U) // Assuming the expected tag value is 42
        dut.clock.step(10)
    }
  }

  it should "pass: write and read metadata simultaneously" in {
    test(new MetaArray[Metadata](Metadata.onReset)).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) {
      dut =>
        // for reset
        dut.clock.step(150)

        dut.io.write.valid.poke(true.B)
        dut.io.write.bits.setIdx.poke(10.U)
        dut.io.write.bits.wayEn.poke(2.U)
        // Assuming MetaWriteReq has a 'data' field of type Metadata or similar
        dut.io.write.bits.data.tag.poke(42.U)
        dut.io.write.bits.data.coh.poke(3.U)

        // Simulate a read operation
        dut.io.read.valid.poke(true.B)
        dut.io.read.bits.setIdx.poke(10.U)
        dut.io.read.bits.wayEn.poke(2.U)

        dut.clock.step(1)
        dut.io.write.valid.poke(false.B)
        dut.io.read.valid.poke(false.B)
        // Assuming the response is checked after the read operation
        dut.io.resp(1).tag.expect(42.U) // Assuming the expected tag value is 42
        dut.clock.step(10)
    }
  }

}

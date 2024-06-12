package coincreekDCache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}

class DataArrayTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("DataArray")

  it should "write and read data array " in {
    test(new DataArray()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) {
      dut =>
        dut.clock.step(5)

        dut.io.write.valid.poke(true.B)
        dut.io.write.bits.idx.poke(10.U)
        dut.io.write.bits.way.poke(2.U)
        dut.io.write.bits.bank.poke(2.U)

        dut.io.write.bits.data(1).poke(42.U)
        dut.clock.step(1)
        dut.io.write.valid.poke(false.B)

        dut.clock.step(10)
        dut.io.read.valid.poke(true.B)
        dut.io.read.bits.idx.poke(10.U)
        dut.io.read.bits.way.poke(2.U)
        dut.io.read.bits.bank.poke(2.U)

        dut.clock.step(1)
        dut.io.read.valid.poke(false.B)
        dut.io.resp(1)(1).expect(42.U)
    }
  }
}
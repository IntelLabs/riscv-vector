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

  val initilizeData =
    "h0123456789abcdef_fedcba9876543210_0011223344556677_8899aabbccddeeff_7766554433221100_ffeeddccbbaa9988_1010101010101010_2323232323232323"

  def initializeDut(dut: DCache): Unit = {
    dut.io.req.valid.poke(false.B)
    dut.clock.step(150)

    dut.io.req.valid.poke(true.B)
    dut.io.req.bits.source.poke(1.U)
    dut.io.req.bits.size.poke(6.U)
    dut.io.req.bits.signed.poke(false.B)
    dut.io.req.bits.cmd.poke(MemoryOpConstants.M_FILL)
    dut.io.req.bits.paddr.poke(0x1234000.U)
    dut.io.req.bits.wdata.poke(initilizeData.U)
    dut.io.req.bits.specifyValid.poke(true.B)
    dut.io.req.bits.specifyWay.poke(1.U)

    dut.clock.step(1)
    dut.io.req.valid.poke(false.B)
    dut.io.req.bits.specifyValid.poke(false.B)
    dut.clock.step(5)
  }

  it should "DCache hit" in {
    test(new DCache()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      initializeDut(dut)

      // read hit
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.signed.poke(false.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect(initilizeData.U)
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(10)

      // read miss
      dut.io.req.valid.poke(true.B)

      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke(0x2234000.U)

      dut.clock.step(1)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.hit.expect(false.B)
      dut.clock.step(10)
    }
  }

  it should "Store -> Load Bypassing" in {
    test(new DCache()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      initializeDut(dut)

      // write hit
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XWR)
      dut.io.req.bits.paddr.poke(0x1234000.U)
      dut.io.req.bits.wdata.poke(0x7890.U)

      dut.clock.step(1)
      // read hit
      dut.io.req.valid.poke(true.B)
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

  it should "Load different sizes" in {
    test(new DCache()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      initializeDut(dut)

      dut.clock.step(1)
      // read hit 512
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.size.poke(6.U)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.clock.step(1)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect(initilizeData.U)
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(10)

      // read hit 64
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.size.poke(3.U)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.clock.step(1)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect("h2323232323232323".U)
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(10)

      // read hit 32
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.size.poke(2.U)
      dut.io.req.bits.signed.poke(true.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke(0x1234010.U)

      dut.clock.step(1)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect(
        "hffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffbbaa9988".U
      )
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(10)
    }
  }

  it should "pass AMOSWAP" in {
    test(new DCache()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      initializeDut(dut)

      // AMOSWAP
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.size.poke(3.U)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XA_SWAP)
      dut.io.req.bits.paddr.poke(0x1234000.U)
      dut.io.req.bits.wdata.poke(0x7890.U)

      dut.clock.step(1)

      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect("h2323232323232323".U)
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(1)

      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect(0x7890.U)
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(10)
    }
  }

  it should "pass AMOADD" in {
    test(new DCache()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      initializeDut(dut)

      // AMOSWAP
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.size.poke(3.U)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XA_ADD)
      dut.io.req.bits.paddr.poke(0x1234000.U)
      dut.io.req.bits.wdata.poke("h0101010101010101".U)

      dut.clock.step(1)

      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect("h2323232323232323".U)
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(1)

      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect("h2424242424242424".U)
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(10)
    }
  }

  it should "lrsc success" in {
    test(new DCache()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      initializeDut(dut)

      // XLR
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.size.poke(3.U)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XLR)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect("h2323232323232323".U)
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(10)

      // XSC
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XSC)
      dut.io.req.bits.wdata.poke("h0101010101010101".U)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.hit.expect(true.B)

      // read hit 64
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.size.poke(3.U)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect("h101010101010101".U)
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(10)
    }
  }

  it should "lrsc fail" in {
    test(new DCache()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      initializeDut(dut)

      // XLR
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.size.poke(3.U)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XLR)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect("h2323232323232323".U)
      dut.io.resp.bits.hit.expect(true.B)

      dut.clock.step(80) // time out

      // XSC
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XSC)
      dut.io.req.bits.wdata.poke("h0101010101010101".U)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect("h1".U)
      dut.io.resp.bits.hit.expect(true.B)

      // read hit 64
      dut.io.req.valid.poke(true.B)
      dut.io.req.bits.size.poke(3.U)
      dut.io.req.bits.cmd.poke(MemoryOpConstants.M_XRD)
      dut.io.req.bits.paddr.poke(0x1234000.U)

      dut.clock.step(1)
      dut.io.req.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.data.expect("h2323232323232323".U)
      dut.io.resp.bits.hit.expect(true.B)
      dut.clock.step(10)
    }
  }
}

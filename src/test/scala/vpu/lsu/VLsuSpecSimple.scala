package smartVector.lsutest

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import smartVector.SVlsu
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.VFuParamsKey
import darecreek.exu.vfu.VFuParameters
import xiangshan.XSCoreParamsKey
import xiangshan.XSCoreParameters


class SVlsuTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "SVlsu"

  it should "perform basic load/store operations" in {
    implicit val p = Parameters.empty.alterPartial({case VFuParamsKey => VFuParameters()
                                                case XSCoreParamsKey => XSCoreParameters()})

    test(new SVlsu()(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Set up initial conditions
      dut.io.mUop.valid.poke(true.B)
      dut.io.mUop.bits.uopAttribute.ldest.poke(1.U)
      dut.io.mUop.bits.scalar_opnd_1.poke(0x1000.U)
      dut.io.mUop.bits.uopAttribute.scalarRegWriteEn.poke(false.B)

      // Execute some load/store operations
      dut.clock.step(4)

      // Set up Hellacache response data
      dut.io.dataExchange.resp.valid.poke(true.B)
      dut.io.dataExchange.resp.bits.has_data.poke(true.B)
      // Set the data to be returned by Hellacache
      dut.io.dataExchange.resp.bits.data.poke("h012345679abcdef".U)

      // Wait for some clock cycles to observe the results
      dut.clock.step(10)

      // Add assertions or print statements as needed
      dut.io.lsuOut.valid.expect(true.B)
      dut.io.lsuOut.bits.vd.expect("h012345679abcdef".U)
    }
  }
}

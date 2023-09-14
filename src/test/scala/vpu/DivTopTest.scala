package vpu

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import darecreek.{BundleGenHelper, SrcBundle}
import darecreek.exu.fu.div.{DivTop, VDivConfig, VIntSRT16TimeplexDivider}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import vpu.fp.VFDivSqrtBehavior

class DivTopTest extends AnyFlatSpec with ChiselScalatestTester
  with Matchers with BundleGenHelper with IDivOps with VFDivSqrtBehavior {
  behavior of "DivTop"
  it should "idiv and fdiv" in {
    implicit val p = (new VDivConfig).toInstance
    test(new DivTop).withAnnotations (Seq(
      WriteVcdAnnotation
    )) { dut =>
      val ops = Seq(
        op_divu64, op_divu8, op_div32, op_remu64, op_remu8, op_divu8, // idiv/rem
        op_fdiv_32, op_fdiv_64, op_frdiv_32, op_frdiv_64,op_fsqrt_32, op_fsqrt_64   // fdiv/sqrt
      )
      val srcs = Seq(
        // idiv
        SrcBundle(vs2 = "h8CBD5", vs1 = "hCC"),
        SrcBundle(vs2 = "hCECECECECECECECE", vs1 = "h0E0E0E0E0E0E0E0E"),
        SrcBundle(vs2 = "hFFFFD2E7FFA763E1", vs1 = "h000000CC000000CC"),
        SrcBundle(vs2 = "h8CBD5", vs1 = "hCC"),
        SrcBundle(vs2 = "hCECECECECECECECE", vs1 = "h0E0E0E0E0E0E0E0E"),
        SrcBundle(vs2 = "h000E1E5F7D8A90EE", vs1 = "h0E0E0E0E0E0E0E0E"),
        // fdiv
        SrcBundle(vs1 = "h360216D54477C000", vs2 = "hC129999A3F4CCCCD"),
        SrcBundle(vs1 = "h402ACCCCCCCCCCCD", vs2 = "h41CEFE3C0D800000"),  // 1039955995 / 13.4
        SrcBundle(vs2 = "h360216D54477C000", vs1 = "hC129999A3F4CCCCD"),
        SrcBundle(vs2 = "h402ACCCCCCCCCCCD", vs1 = "h41CEFE3C0D800000"),
        SrcBundle(vs1 = "h360216D54477C000", vs2 = "hC129999A3F4CCCCD"),
        SrcBundle(vs1 = "h402ACCCCCCCCCCCD", vs2 = "h41CEFE3C0D800000")
      )
      val results = Seq(
        "hB09", //  576469 / 204 = *2825* ... 169
        "h0E0E0E0E0E0E0E0E",  //   206 / 14 = *14* ... 10
        "hFFFFFFC8FFFF90CE", // LEFT: -11545 / 204 = *-56* ... -121
        "hA9", // 576469 / 204 = 2825 ... *169*
        "h0A0A0A0A0A0A0A0A", //  206 / 14 = 14 ... *10*
        "h0001020608090a11", //
        // fdiv
        "hcaa6e0543a539eaa","h419280db415f85bb",
        "hcaa6e0543a539eaa","h419280db415f85bb",
        "h7FC000003F64F92E","h40DF7E165162D937",
      )
      val fflags = Seq(
        "h0", "h0", "h0", "h0", "h0", "h0", // idiv
        "h1", "h1", "h1", "h1",  // fdiv
        "h11", "h1"  // fsqrt
      )
      dut.clock.setTimeout(2000)
      dut.io.in.initSource()
      dut.io.in.setSourceClock(dut.clock)
      dut.io.out.initSink()
      dut.io.out.setSinkClock(dut.clock)
      dut.io.out.ready.poke(true.B)
      dut.io.in.ready.expect(true.B)
      fork {
        dut.io.in.enqueueSeq(genFUInputs(ops, srcs))
      } .fork {
        //        dut.clock.step(80)
        dut.io.out.expectDequeueSeq(genFUOutputs(ops, results, fflags))
      } .join()
    }
  }
}

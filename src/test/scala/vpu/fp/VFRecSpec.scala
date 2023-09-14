package vpu.fp

import chiseltest._
import darecreek.exu.fp.VFRec
import darecreek._
import org.scalatest.flatspec.AnyFlatSpec

trait VFRecBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>
  val op_rec7_32 = VCtrlBundle(typeTag = 0, isRec7 = true)
  val op_rec7_64 = op_rec7_32.copy(typeTag = 1)
  val op_rec7_64_RTZ = op_rec7_64.copy(frm = 1)
  val op_rsqrt7_32 = VCtrlBundle(typeTag = 0, isRecSqrt7 = true)
  val op_rsqrt7_64 = op_rsqrt7_32.copy(typeTag = 1)

  def rsqrt7_test(): Unit = {
    it should "vfrsqrt7" in {
      val ops = Seq(op_rsqrt7_32, op_rsqrt7_32, op_rsqrt7_64, op_rsqrt7_64, op_rsqrt7_64)
      test(new VFRec).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val srcs = Seq(
            SrcBundle(vs2 = "h45E7028F80000000"), SrcBundle(vs2 = "hC04000007F800000"), // 7392.32, -0(DZ);  -3, +inf
            SrcBundle(vs2 = "h4030000000000000"), SrcBundle(vs2 = "h7FF0000000000001"),  // 16 ; sNaN
            SrcBundle(vs2 = "h0001800F80000000") // subnormal 2.0863356516981e-309
          )
          val ins = ops.zip(srcs).map(x => genFloatInput(x._2, x._1))
          dut.io.in.enqueueSeq(ins)
        }.fork {
          //    0.01165/-Inf   NaN/+0   0.2490234375  NaN     2.178768788615672e+154
          val results = Seq("h3C3F0000FF800000", "h7FC0000000000000",
            "h3FCFE00000000000", "h7FF8000000000000", "h5FFA000000000000")
          val fflags = Seq("b01000", "b10000", "b0", "b10000", "b0")
          dut.io.out.expectDequeueSeq(genFloatOutputs(ops,results, fflags))
        }.join()
      }
    }
  }
  def rec7_test(): Unit = {
    it should "vfrec7" in {
      val ops = Seq.fill(4)(op_rec7_32) ++ Seq.fill(2)(op_rec7_64) :+ op_rec7_64_RTZ
      test(new VFRec).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val srcs = Seq(
            SrcBundle(vs2 = "hFF8000007f765432"), // 32 -Inf,  3.274e38
            SrcBundle(vs2 = "h7EE65432C0800000"), // 32 1.5307980304356388e+38, -4
            SrcBundle(vs2 = "h00718abc80280000"), // 32 1.043e-38(subnormal sig10), -3.6734198463196485e-39(sub 01)
            SrcBundle(vs2 = "h7FC000007FA00000"), // 32 qNaN, sNAN
            SrcBundle(vs2 = "h8000000000000000"), // 64  -0
            SrcBundle(vs2 = "h0001800F80000000"), // 64  subnormal 2.0863356516981e-309, RNE(rm=0)
            SrcBundle(vs2 = "h0001800F80000000"), // 64  subnormal 2.0863356516981e-309, RTZ(rm=1)
          )
          val ins = ops.zip(srcs).map(x => genFloatInput(x._2, x._1))
          dut.io.in.enqueueSeq(ins)
        }.fork {
          val results = Seq(
            "h8000000000214000",  //  -0,   3.053e-39(out shift 2)
            "h00470000BE7F0000",  //  6.520320227217376e-39(out shif 1), -0.2490234375
            "h7e900000FF4C0000",  //  1.043e-38,  -2.7116251114012284e+38
            "h7FC000007FC00000",  //  nan, nan
            "hFFF0000000000000",  //  -inf
            "h7FF0000000000000",  //   +inf
            "h7FEFFFFFFFFFFFFF"   //   +greatest mag
          )
          //                                     NV       DZ      NX/OF      NX/OF
          val fflags = Seq("b0", "b0", "b0", "b10000", "b01000" , "b00101", "b00101")
          dut.io.out.expectDequeueSeq(genFloatOutputs(ops,results, fflags))
        }.join()
      }
    }
  }
  def inactive_test(): Unit = {
    it should "skip inactive fflag" in {
      val op_rec7_32_mask = op_rec7_32.copy(vm = false)
      val op_rsqrt7_32_mask = op_rsqrt7_32.copy(vm = false)
      val ops = Seq(op_rec7_32_mask, op_rec7_32_mask, op_rsqrt7_32_mask)
      test(new VFRec).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val srcs = Seq(
            SrcBundle(vs2 = "h7FA000007f765432", mask = "hff"), // 32 qNaN, 3.274e38
            SrcBundle(vs2 = "h7FA000007f765432", mask = "hfd"), // 32 qNaN, 3.274e38
            SrcBundle(vs2 = "h45E7028F80000000", mask = "hff"),  // 7392.32, -0(DZ)
            SrcBundle(vs2 = "h45E7028F80000000", mask = "hfe")  // 7392.32, -0(DZ)
          )
          val ins = ops.zip(srcs).map(x => genFloatInput(x._2, x._1))
          dut.io.in.enqueueSeq(ins)
        }.fork {
          val results = Seq(
            "h7FC0000000214000",  //  nan, 3.053e-39(out shift 2)
            "h7FC0000000214000",  //  nan, 3.053e-39(out shift 2)
            "h3C3F0000FF800000",
            "h3C3F0000FF800000",
          )
          val fflags = Seq("b10000", "b00000", "b01000", "b00000")
          dut.io.out.expectDequeueSeq(genFloatOutputs(ops,results, fflags))
        }.join()
      }
    }
  }

}
class VFRecSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VFRecBehavior {
  behavior of "VFRec"
  it should behave like rsqrt7_test
  it should behave like rec7_test
  it should behave like inactive_test
}
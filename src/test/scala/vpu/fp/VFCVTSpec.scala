package vpu.fp

import chisel3.stage.ChiselStage
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._

import darecreek.exu.fp._
import darecreek._ //to do change later
trait VFCVTBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  def f2i_test(): Unit = {
    val op_f2xu_32 = VCtrlBundle(typeTag = 0, isCvt = true, cvtCmd = 2)
    val op_f2xu_64 = op_f2xu_32.copy(typeTag = 1)
    val op_f2x_32 = op_f2xu_32.copy(cvtSigned = true)
    val op_f2x_64 = op_f2xu_64.copy(cvtSigned = true)
    val op_f2xu_rtz_32 = op_f2xu_32.copy(cvtRm = 2)
    val op_f2xu_rtz_64 = op_f2xu_64.copy(cvtRm = 2)
    val op_f2x_rtz_32 = op_f2x_32.copy(cvtRm = 2)
    val op_f2x_rtz_64 = op_f2x_64.copy(cvtRm = 2)
    val op_w_f2xu = op_f2xu_32.copy(wideningDst = true)
    val op_w_f2x = op_f2x_32.copy(wideningDst = true)
    val op_w_f2xu_rtz = op_f2xu_rtz_32.copy(wideningDst = true)
    val op_w_f2x_rtz = op_f2x_rtz_32.copy(wideningDst = true)
    val op_n_f2xu = op_f2xu_64.copy(narrowDst = true)
    val op_n_f2x = op_f2x_64.copy(narrowDst = true)
    val op_n_f2xu_rtz = op_f2xu_rtz_64.copy(narrowDst = true)
    val op_n_f2x_rtz = op_f2x_rtz_64.copy(narrowDst = true)
    
    // non-narrow/widen cvt
    it should "vfcvt.x(u).f" in {
      val ops_32 = Seq(op_f2xu_32, op_f2x_32, op_f2xu_rtz_32, op_f2x_rtz_32)
      val ops_64 = Seq(op_f2xu_64, op_f2x_64, op_f2xu_rtz_64, op_f2x_rtz_64)
      test(new VFCVT).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          // -10.6 & -0.8
          val in_32 = ops_32.map(op => genFloatInput(SrcBundle(vs2 = "hC129999ABF4CCCCD"), op))
          dut.io.in.enqueueSeq(in_32)
        }.fork {
          val fflags = Seq("h10", "h01", "h11", "h01")
          val results = Seq("h0", "hfffffff5ffffffff", "h0", "hfffffff600000000")
          val out_32 = genFloatOutputs(ops_32, results, fflags)
          dut.io.out.expectDequeueSeq(out_32)
        }.join()
      }
    }
    it should "vfwncvt.x(u).f" in {
      val ops_w = Seq(op_w_f2xu, op_w_f2x,op_w_f2xu_rtz,op_w_f2x_rtz)
      val ops_n = Seq(op_n_f2xu, op_n_f2x,op_n_f2xu_rtz,op_n_f2x_rtz)
      test(new VFCVT).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          // -10.6 & -0.8
          val in_w = dupElement(ops_w).map(op => genFloatInput(SrcBundle(vs2 = "hC129999ABF4CCCCD"), op))
          val in_n = dupElement(ops_n).zip( Seq.fill(4)(Seq("hC025333333333333", "hBFE999999999999A")).flatten
          ).map(x => genFloatInput(SrcBundle(vs2 = x._2), x._1))
          dut.io.in.enqueueSeq(in_w)
          dut.io.in.enqueueSeq(in_n)
        }.fork {
          val fflags = Seq("h10", "h10", "h01", "h01", "h01", "h10", "h01", "h01",
          "h10", "h01", "h11", "h01")
          val results = Seq(
            "h0", "h0", // w xu
            "hffffffffffffffff", "hfffffffffffffff5", // w x
            "h0", "h0", // w xu rtz
            "h0000000000000000", "hfffffffffffffff6", // w x rtz
            "h0", "hfffffffffffffff5", // n xu, x
            "h0", "h00000000fffffff6", // n xu, x  rtz
          )
          val out = genFloatOutputs(dupElement(ops_w) ++ ops_n, results, fflags)

          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
  }

  def i2f_test(): Unit = {
    val op_xu2f_32 = VCtrlBundle(typeTag = 0, isCvt = true, cvtCmd = 4)
    val op_xu2f_64 = op_xu2f_32.copy(typeTag = 1)
    val op_x2f_32 = op_xu2f_32.copy(cvtSigned = true)
    val op_x2f_64 = op_xu2f_64.copy(cvtSigned = true)
    val op_w_xu2f = op_xu2f_32.copy(wideningDst = true)
    val op_w_x2f = op_x2f_32.copy(wideningDst = true)
    val op_n_xu2f = op_xu2f_64.copy(narrowDst = true)
    val op_n_x2f = op_x2f_64.copy(narrowDst = true)
    // non-narrow/widen cvt
    it should "vfcvt.f.x(u)" in {
      val ops_32 = Seq(op_xu2f_32, op_x2f_32)
      val ops_64 = Seq(op_xu2f_64, op_x2f_64)
      test(new VFCVT).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          // -10(signed)/4294967286(unsigned) & 44
          val in_32 = ops_32.map(op => genFloatInput(SrcBundle(vs2 = "hfffffff60000002C"), op))
          dut.io.in.enqueueSeq(in_32)
        }.fork {
          val fflags = Seq("h01", "h00")
          val results = Seq("h4F80000042300000", "hC120000042300000")
          val out = genFloatOutputs(ops_32, results, fflags)
          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
    it should "vfwncvt.f.x(u)" in {
      val ops_w = Seq(op_w_xu2f, op_w_xu2f, op_w_x2f, op_w_x2f)
      val ops_n = Seq(op_n_xu2f, op_n_xu2f, op_n_x2f, op_n_x2f)
      test(new VFCVT).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          // -10(signed)/4294967286(unsigned32)/very large(unsigned64) & 44
          val in_w = ops_w.map(op => genFloatInput(SrcBundle(vs2 = "hfffffff60000002C"), op))
          val in_n = ops_n.zip(Seq.fill(2)(Seq("hfffffffffffffff6", "h000000000000002C")).flatten)
            .map(x => genFloatInput(SrcBundle(vs2 = x._2), x._1))
          dut.io.in.enqueueSeq(in_w)
          dut.io.in.enqueueSeq(in_n)
        }.fork {
          val fflags = Seq("h0","h0","h0","h0","h1","h0")
          val results = Seq(
            "h4046000000000000", "h41EFFFFFFEC00000", // w
            "h4046000000000000", "hC024000000000000", // w
            "h423000005F800000", "h42300000C1200000", // n
          )
          val out = genFloatOutputs(ops_w ++ Seq(op_n_xu2f, op_n_x2f), results, fflags)
          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
  }

  def f2f_test(): Unit = {
    val op_s2d = VCtrlBundle(typeTag = 0, isCvt = true, wideningDst = true, cvtCmd = 1)
    val op_d2s = VCtrlBundle(typeTag = 0, isCvt = true, narrowDst = true, cvtCmd = 1)
    val op_d2s_rod = op_d2s.copy(cvtRm = 1) // rod rounding

    // non-narrow/widen cvt
    it should "vfwcvt.f.f" in {
      val ops = Seq(op_s2d, op_s2d)
      test(new VFCVT).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          // -10 / 44
          val in_32 = ops.map(op => genFloatInput(SrcBundle(vs2 = "hC120000042300000"), op))
          dut.io.in.enqueueSeq(in_32)
        }.fork {
          // fflags should always be 0
          val out_32 = Seq("h4046000000000000", "hC024000000000000").zip(ops).map(x => genFloatOutput(x._1, x._2))
          dut.io.out.expectDequeueSeq(out_32)
        }.join()
      }
    }
    it should "vfncvt.f.f" in {
      val ops = Seq(op_d2s, op_d2s)
      test(new VFCVT).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          // 4046666666666666 / 7FE6666666666666
          val in_t1 = genFloatInput(SrcBundle(vs2 = "h4046666666666666"), op_d2s)
          val in_t2 = genFloatInput(SrcBundle(vs2 = "h7FE6666666666666"), op_d2s)
          val in_rod_t1 = genFloatInput(SrcBundle(vs2 = "h7FE6666666666666"), op_d2s_rod)
          val in_rod_t2 = genFloatInput(SrcBundle(vs2 = "h4047777770000000"), op_d2s_rod)
//          val in_32 = ops.zip(Seq("h4046666666666666", "h7FE6666666666666")).map(x => genInput(SrcBundle(vs2 = x._2), x._1))
          dut.io.in.enqueue(in_t1)
          dut.clock.step(2) // insert bubble
          dut.io.in.enqueue(in_t2)
          dut.io.in.enqueueSeq(Seq(in_rod_t1, in_rod_t2))
        }.fork {
          // fflags should be flag_cycle1 | flag_cycle2
          val fflags = Seq("h5", "h5") // inexact and overflow
          val results = Seq("h7F80000042333333", "h423BBBBB7F800000")
          val out_32 = genFloatOutputs(Seq(op_d2s, op_d2s_rod), results, fflags)
          dut.io.out.expectDequeueSeq(out_32)
        }.join()
      }
    }
  }
}

class VFCVTSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VFCVTBehavior {
  behavior of "VOPVFCVTVOP"
  it should behave like f2i_test
  it should behave like i2f_test
  it should behave like f2f_test
}
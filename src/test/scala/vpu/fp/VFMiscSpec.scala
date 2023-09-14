package vpu.fp

import chiseltest._
import darecreek.exu.fp.VFMisc
import org.scalatest.flatspec.AnyFlatSpec
import darecreek._ //to do change later
trait VFMiscBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>
  def sgnj_test():Unit = {
    val op_sgnj_32 = VCtrlBundle(typeTag = 0, isMisc = true, miscCmd = 1)
    val op_sgnj_64 = op_sgnj_32.copy(typeTag = 1)
    val op_sgnjn_32 = op_sgnj_32.copy(miscSubCmd = 1)
    val op_sgnjn_64 = op_sgnj_64.copy(miscSubCmd = 1)
    val op_sgnjx_32 = op_sgnj_32.copy(miscSubCmd = 2)
    val op_sgnjx_64 = op_sgnj_64.copy(miscSubCmd = 2)

    println("I:vfsgnj.vv vd, vs2, vs1, vm -sew32")
    println("I:vfsgnj.vv vd, vs2, vs1, vm -sew64")
    println("I:vfsgnjn.vv vd, vs2, vs1, vm -sew32")
    println("I:vfsgnjn.vv vd, vs2, vs1, vm -sew64")
    println("I:vfsgnjx.vv vd, vs2, vs1, vm -sew32")
    println("I:vfsgnjx.vv vd, vs2, vs1, vm -sew64")

    it should "vfsgnj" in {
      val ops = Seq(op_sgnj_32,op_sgnj_64,op_sgnjn_32,op_sgnjn_64,op_sgnjx_32,op_sgnjx_64)
      test(new VFMisc).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val srcs = Seq(
            SrcBundle(vs1 = "h360216D54477C000", vs2 = "hC129999A3F4CCCCD"),  // 32 srcs, sign(vs1):+,+ vs2:-10.6,+0.8
            SrcBundle(vs1 = "hCAD6F375E135BAE7", vs2 = "h4025333333333333"))  // 64 srcs sign(vs1):-  vs2: +10.6
          val ins = ops.zip(Seq.fill(3)(srcs).flatten).map(x => genFloatInput(x._2, x._1))
          dut.io.in.enqueueSeq(ins)
        }.fork {
          val outs = Seq("h4129999A3F4CCCCD","hC025333333333333",
            "hC129999ABF4CCCCD","h4025333333333333",
            "hC129999A3F4CCCCD","hC025333333333333"
          ).zip(ops).map(x => genFloatOutput(x._1, x._2))
          dut.io.out.expectDequeueSeq(outs)
        }.join()
      }
    }
  }
  def classify_test():Unit = {
    val op_32 = VCtrlBundle(typeTag = 0, isMisc = true, miscCmd = 2)
    val op_64 = VCtrlBundle(typeTag = 1, isMisc = true, miscCmd = 2)
    it should "vfclass" in {
      val ops = Seq(op_32, op_64)
      test(new VFMisc).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          // (neg 0, positive normal), signalingNaN
          val ins = ops.zip(Seq("h800000003F4CCCCD", "h7FF8000000000000")).map(x => genFloatInput(SrcBundle(vs2 = x._2), x._1))
          dut.io.in.enqueueSeq(ins)
        }.fork {
          val outs = Seq("h0000000800000040","h0000000000000200").zip(ops).map(x => genFloatOutput(x._1, x._2))
          dut.io.out.expectDequeueSeq(outs)
        }.join()
      }
    }
  }
  def cmp_test():Unit = {
    val eq_32 = VCtrlBundle(typeTag = 0, isMisc = true, miscCmd = 3)
    val eq_64 = eq_32.copy(typeTag = 1)
    val ne_32 = eq_32.copy(miscSubCmd = 4)
    val ne_64 = ne_32.copy(typeTag = 1)
    val le_32 = eq_32.copy(miscSubCmd = 1)
    val le_64 = le_32.copy(typeTag = 1)
    val lt_32 = eq_32.copy(miscSubCmd = 3)
    val lt_64 = lt_32.copy(typeTag = 1)
    val ge_32 = eq_32.copy(miscSubCmd = 7)
    val ge_64 = ge_32.copy(typeTag = 1)
    val gt_32 = eq_32.copy(miscSubCmd = 5)
    val gt_64 = gt_32.copy(typeTag = 1)
    it should "vfcmp" in {
      val ops = Seq(
        eq_32,eq_32,eq_64,
        ne_32,ne_32,ne_64,
        le_32,le_32,le_64,
        lt_32,lt_32,lt_64,
        ge_32,ge_32,ge_64,
        gt_32,gt_32,gt_64
      )
      test(new VFMisc).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val srcs = Seq(
            SrcBundle(vs2 = "hBE99999A7F800000", vs1 = "h42DE000042D00000"),  // 32 srcs, (-0.3, +inf),(111, 104)
            SrcBundle(vs2 = "h7FC0000000000000", vs1 = "h8000000080000000"),  // 32 srcs, (quietNaN, +0),(-0, -0)
            SrcBundle(vs2 = "h4025333333333333", vs1 = "h4025333333333333"))  // 64 srcs, +10.6, +10.6
          val ins = ops.zip(Seq.fill(6)(srcs).flatten).map(x => genFloatInput(x._2, x._1))
          dut.io.in.enqueueSeq(ins)
        }.fork {
          val fflags = Seq("h0","h0","h0","h0","h0","h0",
            "h0","h10","h0","h0","h10","h0",
            "h0","h10","h0","h0","h10","h0")
          val results = Seq(
            "h0000000000000000","h0000000000000001","h0000000000000001",  // invalid: 0 0 0
            "h0000000000000003","h0000000000000002","h0000000000000000",  // invalid: 0 0 0
            "h0000000000000002","h0000000000000001","h0000000000000001",  // invalid: 0 1 0
            "h0000000000000002","h0000000000000000","h0000000000000000",  // invalid: 0 1 0
            "h0000000000000001","h0000000000000001","h0000000000000001",  // invalid: 0 1 0
            "h0000000000000001","h0000000000000000","h0000000000000000",  // invalid: 0 1 0
          )
          val out = genFloatOutputs(ops, results, fflags)
          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
  }
  def minmax_test():Unit = {
    val min_32 = VCtrlBundle(typeTag = 0, isMisc = true, miscSubCmd = 4)
    val min_64 = min_32.copy(typeTag = 1)
    val max_32 = min_32.copy(miscSubCmd = 6)
    val max_64 = max_32.copy(typeTag = 1)
    it should "vfminmax" in {
      val ops = Seq(
        min_32,min_32,min_64,
        max_32,max_32,max_64
      )
      test(new VFMisc).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val srcs = Seq(
            SrcBundle(vs2 = "hBE99999A7F800000", vs1 = "h42DE000042D00000"),  // 32 srcs, (-0.3, +inf),(111, 104)
            SrcBundle(vs2 = "h7FC0000000000000", vs1 = "h8000000080000000"),  // 32 srcs, (quietNaN, +0),(-0, -0)
            SrcBundle(vs2 = "h7FF0000000000030", vs1 = "h7FF0000000000002"))  // 64 srcs, sNaN, sNaN
          val ins = ops.zip(Seq.fill(2)(srcs).flatten).map(x => genFloatInput(x._2, x._1))
          dut.io.in.enqueueSeq(ins)
        }.fork {
          val fflags = Seq("h0","h0","h10","h0","h0","h10")
          val results = Seq(
            "hBE99999A42D00000","h8000000080000000","h7FF8000000000000",  // invalid: 0 0 1
            "h42DE00007F800000","h8000000000000000","h7FF8000000000000",  // invalid: 0 0 1
          )
          val out = genFloatOutputs(ops, results, fflags)
          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
  }
}


class VFMiscSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VFMiscBehavior {
  behavior of "VFMisc"
  it should behave like sgnj_test
  it should behave like classify_test
  it should behave like cmp_test
  it should behave like minmax_test
}
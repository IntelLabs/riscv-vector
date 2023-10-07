package vpu.fp

import chisel3.stage.ChiselStage
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters
import chisel3._
import darecreek.exu.fp._
import darecreek._ //to do change later
import chiseltest.WriteVcdAnnotation


trait VFFMABehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val testf32Op1 = "4124cccd" // 10.3
  val testf32Op2 = "40b66666" // 5.7
  val testf32Op3 = "c111999a" // -9.1
  val testf32Op4 = "3ca3d70a" // 0.02

  // sign(1b),exp(11b),sig(52b = 13B)
  val testf64Op1 = "402499999999999a" // 10.3
  val testf64Op2 = "4016cccccccccccd" // 5.7
  val testf64Op3 = "403EAA7EF9DB22D1" // 30.666

  val src32: SrcBundle = SrcBundle(
    vs1 = "h" + testf32Op1 + testf32Op3,
    vs2 = "h" + testf32Op2 + testf32Op4,
    vd = "h" + testf32Op1 + testf32Op3,
  )

  val src64: SrcBundle = SrcBundle(
    vs1 = "h" + testf64Op1,
    vs2 = "h" + testf64Op2,
    vd = "h" + testf64Op3
  )
  val vf32mul = VCtrlBundle(typeTag = 0, fmaCmd = 1) // fp32
  val vf64mul = vf32mul.copy(typeTag = 1)
  val vf32sub = VCtrlBundle(typeTag = 0, fmaCmd = 2, negVs1 = true)
  val vf64sub = vf32sub.copy(typeTag = 1)
  val vf32rsub = VCtrlBundle(typeTag = 0, fmaCmd = 2, negVs2 = true)
  val vf64rsub = vf32rsub.copy(typeTag = 1)
  val macc32 = VCtrlBundle(typeTag = 0, fmaCmd = 3)
  val macc64 = macc32.copy(typeTag = 1)
  val madd32 = macc32.copy(switchVdVs2 = true)
  val madd64 = madd32.copy(typeTag = 1)
  val nmadd32 = VCtrlBundle(typeTag = 0, fmaCmd = 3, negVs1 = true, negVs2 = true, switchVdVs2 = true)
  val nmadd64 = nmadd32.copy(typeTag = 1)
  val waddvv32 = VCtrlBundle(typeTag = 0, fmaCmd = 2, wideningDst = true)
  val waddwv32 = VCtrlBundle(typeTag = 0, fmaCmd = 2, wideningSrc2 = true)
  val wmaccvv32 = VCtrlBundle(typeTag = 0, fmaCmd = 3, wideningDst = true)
  
  println("I:vfmul.vv vd, vs2, vs1, vm -sew32")
  println("I:vfmul.vv vd, vs2, vs1, vm -sew64")
  println("I:vfsub.vv vd, vs2, vs1, vm  -sew32")
  println("I:vfsub.vv vd, vs2, vs1, vm  -sew64")
  println("I:vfrsub.vf vd, vs2, rs1, vm -sew32")
  println("I:vfrsub.vf vd, vs2, rs1, vm -sew64")

  println("I:vfmacc.vv vd, vs1, vs2, vm  -sew32")
  println("I:vfmacc.vv vd, vs1, vs2, vm  -sew64")
  println("I:vfmadd.vv vd, vs1, vs2, vm  -sew32")
  println("I:vfmadd.vv vd, vs1, vs2, vm  -sew64")
  println("I:vfnmadd.vv vd, vs1, vs2, vm  -sew32")
  println("I:vfnmadd.vv vd, vs1, vs2, vm  -sew64")

  println("I:vfwadd.vv vd, vs2, vs1, vm  -sew32")
  println("I:vfwadd.wv vd, vs2, vs1, vm  -sew32")
  println("I:vfwmacc.vv vd, vs1, vs2, vm -sew32")


  def op_vfmul(): Unit = {
    it should "vfmul.vv" in {
      test(new VFFMA).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val in32 = genFloatInput(src32, vf32mul)
          val in64 = genFloatInput(src64, vf64mul)
          dut.io.in.enqueueSeq(Seq(in32, in64))
        }.fork {
          val out32 = genFloatOutput("h426ad70abe3a5e35", vf32mul, fflags = "h01")
          val out64 = genFloatOutput("h404d5ae147ae147c", vf64mul, fflags = "h01")
          dut.io.out.expectDequeueSeq(Seq(out32, out64))
        }.join()
      }
    }
  }

  // vs2 - vs1
  def op_vfsub(): Unit = {
    it should "vf(r)sub.vv" in {
      val ops = Seq(vf32sub, vf64sub, vf32rsub, vf64rsub)
      test(new VFFMA).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val inSeq = genFloatInputs(ops, Seq(src32, src64, src32, src64))
          dut.io.in.enqueueSeq(inSeq)
        }.fork {
          val fflags = Seq("h01", "h00", "h01", "h00")
          val results = Seq("hC09333344111EB86", "hC012666666666667",
            "h40933334C111EB86", "h4012666666666667")
          val out = genFloatOutputs(ops, results, fflags)
          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
  }

  def op_fma(): Unit = {
    it should "vffma.vv" in {
      val ops = Seq(macc32, macc64, madd32, madd64, nmadd32, nmadd64, waddvv32, waddvv32)
      test(new VFFMA).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val inSeq = genFloatInputs(ops, Seq.fill(3)(Seq(src32, src64)).flatten ++ Seq.fill(2)(src32))
          dut.io.in.enqueueSeq(inSeq.slice(0, inSeq.length-1))
          dut.clock.step(2)
          dut.io.in.enqueueSeq(inSeq.slice(inSeq.length-1, inSeq.length))
        }.fork {
          val fflags = Seq("h01", "h01", "h01", "h01", "h01", "h01", "h0", "h0")
          val results = Seq("h428A051FC1148313", "h40565810624DD2F2",
            "h42DF947B42A5A8F7", "h407418F4F0D844D1",
            "hC2DF947BC2A5A8F7", "hC07418F4F0D844D1",
            "hC02228F5CF600000", "h4030000000000000", // waddvv32, first tail32, then head32
          )
          // fake ops, test output only
          val waddvv64 = VCtrlBundle(fmaCmd = 2, wideningDst = true)
          val ops_fake = ops.slice(0, ops.length-2) ++ Seq(waddvv64,waddvv64)
          val out = genFloatOutputs(ops_fake, results, fflags)
          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
  }

  def mixed_precision_input(): Unit = {
    // vs2 is 64b while vs1 is 32b, refering to spec
    val src_waddwv_cycle1: SrcBundle = SrcBundle(
      vs2 = "h" + testf64Op1,
      vs1 = "h" + testf32Op2 + testf32Op4,
    )
    val src_waddwv_cycle2: SrcBundle = SrcBundle(
      vs2 = "h" + testf64Op2,
      vs1 = "h" + testf32Op2 + testf32Op4,
    )
    val src_wmacc_cycle1: SrcBundle = SrcBundle(
      vs1 = "h" + testf32Op1 + testf32Op3,
      vs2 = "h" + testf32Op2 + testf32Op4,
      vd = "h" + testf64Op3
    )
    val src_wmacc_cycle2: SrcBundle = SrcBundle(
      vs1 = "h" + testf32Op1 + testf32Op3,
      vs2 = "h" + testf32Op2 + testf32Op4,
      vd = "h" + testf64Op1,
    )
    it should "vfwaddwvwmacc.vv" in {
      val ops = Seq(waddwv32, wmaccvv32)
      test(new VFFMA).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarness.test_init(dut)
        fork {
          val inSeq = genFloatInputs(dupElement(ops), Seq(
            src_waddwv_cycle1, src_waddwv_cycle2,
            src_wmacc_cycle1, src_wmacc_cycle2))
          dut.io.in.enqueueSeq(inSeq)
        }.fork {
          // fake ops, output test only
          val waddwv64 = waddwv32.copy(typeTag = 1)
          val wmaccvv64 = wmaccvv32.copy(typeTag = 1)
          val fflags = Seq("h0", "h01", "h0", "h01")
          val results = Seq("h4024A3D70A39999A", "h4026CCCCC6666666",
                    "h403E7BE76C7BF7CF", "h405140A3D35C28F3")
          val out = genFloatOutputs(dupElement(Seq(waddwv64,wmaccvv64)), results, fflags)
          dut.io.out.expectDequeueSeq(out)
        }.join()
      }
    }
  }

  def out_of_order_output() = {}
}


class VFFMASpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VFFMABehavior {
  behavior of "VFMA"
  it should behave like op_vfmul
  it should behave like op_vfsub
  it should behave like op_fma
  it should behave like mixed_precision_input
  it should behave like out_of_order_output
}

object GenVerilog extends App {
//  implicit val p: Parameters = (new WithVFPUConfig).toInstance
  (new ChiselStage).emitVerilog(new VFPUTop)
}


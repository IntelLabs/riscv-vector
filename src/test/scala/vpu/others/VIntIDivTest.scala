/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package vpu

import org.chipsalliance.cde.config.Config
import chisel3.{assert, _}
import chiseltest._
import chiseltest.ChiselScalatestTester
import darecreek.{BundleGenHelper, DarecreekParameters, SrcBundle, VCtrlBundle}
import darecreek.exu.fp.{LANE_WIDTH, NAME, XLEN}
import darecreek.exu.fu.div.{SRT16DividerDataModule, VDivConfig, VIntSRT16TimeplexDivider}
//import chiseltest.experimental.TestOptionBuilder._
//import chiseltest.internal.{LineCoverageAnnotation, ToggleCoverageAnnotation, VerilatorBackendAnnotation}
//import chiseltest.legacy.backends.verilator.VerilatorFlags
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import firrtl.stage.RunFirrtlTransformAnnotation
//import xstransforms.PrintModuleName

//import xiangshan.backend.fu._

import scala.util.Random
import darecreek.exu.fu.div.VIntSRT16SpacePlexDivider

trait DivTestHelper {
  def testOp(m: SRT16DividerWrapper, dividend: Long, divisor: Long, isSign: Boolean) = {

    m.io.in_valid.poke(true.B)
    m.io.dividend.poke((s"b" + dividend.toBinaryString).asUInt(64.W))
    m.io.divisor.poke((s"b" + divisor.toBinaryString).asUInt(64.W))
    m.io.sign.poke(isSign.B)
    val (quotient, remainder) = (dividend / divisor, dividend % divisor)
    println(s"quotient is ${quotient.toHexString}, remainder is ${remainder.toHexString}")
    var timeTaken = 0
    while (!m.io.out_valid.peek().litToBoolean) {
      m.clock.step()
      timeTaken += 1
      if (timeTaken >= 62) assert(false, s"Timeout for single execution!!!")
    }

    m.io.in_valid.poke(false.B)
    m.io.out_ready.poke(true.B)
    m.io.isHi.poke(false.B)
    m.clock.step()

    m.io.result.expect((s"b" + quotient.toBinaryString).asUInt(64.W))
    m.io.isHi.poke(true.B)
    m.clock.step()

    m.io.result.expect((s"b" + remainder.toBinaryString).asUInt(64.W))
  }
}

class SRT16DividerWrapper extends Module {
  val io = IO(new Bundle{
    val dividend = Input(UInt(64.W))
    val divisor = Input(UInt(64.W))
    val sign = Input(Bool())
    val isHi = Input(Bool())
    val isW = Input(Bool())
    val in_valid = Input(Bool())
    val out_ready = Input(Bool())
    val in_ready = Output(Bool())
    val out_valid = Output(Bool())
    val result = Output(UInt(64.W))
  })
  val divider = Module(new SRT16DividerDataModule(len = 64))
  divider.io.src(0) := io.dividend
  divider.io.src(1) := io.divisor
  divider.io.kill_r := false.B
  divider.io.kill_w := false.B
  divider.io.sign := io.sign
  divider.io.isHi := io.isHi
  divider.io.isW := io.isW
  divider.io.out_ready := io.out_ready
  divider.io.valid := io.in_valid

  io.in_ready := divider.io.in_ready
  io.out_valid := divider.io.out_valid

  io.result := divider.io.out_data

}

trait IDivOps {
  val op_divu64 = VCtrlBundle(funct6 = Integer.parseInt("100000", 2), isDivSqrt = true)
  val op_divu32 = op_divu64.copy(vsew = 2)
  val op_divu16 = op_divu64.copy(vsew = 1)
  val op_divu8 = op_divu64.copy(vsew = 0)
  val op_div64 = VCtrlBundle(funct6 = Integer.parseInt("100001", 2), isDivSqrt = true)
  val op_div32 = op_div64.copy(vsew = 2)
  val op_div16 = op_div64.copy(vsew = 1)
  val op_div8 = op_div64.copy(vsew = 0)
  val op_remu64 = VCtrlBundle(funct6 = Integer.parseInt("100010", 2), isDivSqrt = true)
  val op_remu32 = op_remu64.copy(vsew = 2)
  val op_remu16 = op_remu64.copy(vsew = 1)
  val op_remu8 = op_remu64.copy(vsew = 0)
  val op_rem64 = VCtrlBundle(funct6 = Integer.parseInt("100011", 2), isDivSqrt = true)
  val op_rem32 = op_rem64.copy(vsew = 2)
  val op_rem16 = op_rem64.copy(vsew = 1)
  val op_rem8 = op_rem64.copy(vsew = 0)
}

// vs2 / vs1 = quotient ... remainder
class VIntIDivTest extends AnyFlatSpec with ChiselScalatestTester
  with Matchers with BundleGenHelper with IDivOps with DivTestHelper {
  behavior of "srt16 vector int divider"
  it should "all ops" in {
    implicit val p = (new VDivConfig).toInstance
    test(new VIntSRT16TimeplexDivider).withAnnotations (Seq(
      WriteVcdAnnotation
    )){ dut =>
      val ops = Seq(op_divu64, op_divu8, op_div32, op_remu64, op_remu8, op_divu8)
      // vs2 / vs1
      val srcs = Seq(
        SrcBundle(vs2 = "h8CBD5", vs1 = "hCC"),
        SrcBundle(vs2 = "hCECECECECECECECE", vs1 = "h0E0E0E0E0E0E0E0E"),
        SrcBundle(vs2 = "hFFFFD2E7FFA763E1", vs1 = "h000000CC000000CC"),
        SrcBundle(vs2 = "h8CBD5", vs1 = "hCC"),
        SrcBundle(vs2 = "hCECECECECECECECE", vs1 = "h0E0E0E0E0E0E0E0E"),
        SrcBundle(vs2 = "h000E1E5F7D8A90EE", vs1 = "h0E0E0E0E0E0E0E0E"),
      )
      val results = Seq(
        "hB09", //  576469 / 204 = *2825* ... 169
        "h0E0E0E0E0E0E0E0E",  //   206 / 14 = *14* ... 10
        "hFFFFFFC8FFFF90CE", // LEFT: -11545 / 204 = *-56* ... -121
        "hA9", // 576469 / 204 = 2825 ... *169*
        "h0A0A0A0A0A0A0A0A", //  206 / 14 = 14 ... *10*
        "h0001020608090a11" //
      )
      dut.clock.setTimeout(1000)
      dut.io.in.initSource()
      dut.io.in.setSourceClock(dut.clock)
      dut.io.out.initSink()
      dut.io.out.setSinkClock(dut.clock)
      dut.io.out.ready.poke(true.B)
      dut.io.in.ready.expect(true.B)
      fork {
        dut.io.in.enqueueSeq(genFUWithMaskInputs(ops, srcs))
      } .fork {
        dut.io.out.expectDequeueSeq(genFUWithMaskOutputs(ops, results))
      } .join()
    }
  }
}

class IntDividerTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with DivTestHelper {
  behavior of "srt16 divider"
  it should "unsigned test" in {
    test(new SRT16DividerWrapper).withAnnotations (Seq(
      WriteVcdAnnotation
    )){ dut =>
      dut.clock.step(20)
      dut.io.in_ready.expect(true.B)
      val dividend = 0xce // 1100_1110
      val divisor = 0xe // 1110

      testOp(dut, dividend, divisor, false)
      dut.clock.step(10)
      testOp(dut, dividend, divisor, true)
      dut.clock.step(10)
      testOp(dut, -dividend, divisor, true)
      dut.clock.step(10)
      testOp(dut, dividend, -divisor, true)
      dut.clock.step(10)
      testOp(dut, -dividend, -divisor, true)
    }
  }
  it should "random sign test" in {
    val rand = new Random(0x14226)
    val testNum = 1000
    test(new SRT16DividerWrapper).withAnnotations(Seq(
//      VerilatorBackendAnnotation,
//      LineCoverageAnnotation,
//      ToggleCoverageAnnotation,
//      VerilatorFlags(Seq("--output-split 5000", "--output-split-cfuncs 5000",
//        "+define+RANDOMIZE_REG_INIT", "+define+RANDOMIZE_MEM_INIT", "--trace")),
//      RunFirrtlTransformAnnotation(new PrintModuleName))
    ))  { m =>
      println("Test started!")
      m.clock.step(20)

      for (i <- 1 to testNum) {
        m.clock.step(3)
        m.io.in_ready.expect(true.B)
        val divisor = rand.nextLong()
        val dividend = rand.nextLong()
        // val sign = rand.nextBoolean()
        println(s"$i th iteration\n" + s"divisor is ${divisor.toHexString}, dividend is ${dividend.toHexString}")
        testOp(m, dividend, divisor, true)
      }
    }
  }
}

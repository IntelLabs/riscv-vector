package darecreek

import chisel3._
import chiseltest._
import org.scalatest._
import chisel3.stage.ChiselStage
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
import scala.math.BigInt
import org.scalatest.Assertions._
import darecreek.exu.fp.fudian.{FMULToFADD, FMULToFADD_fflags, FloatPoint}
import darecreek.exu.fp._
import chisel3.util.{Arbiter, DecoupledIO}

trait IntegerFPBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>
  val number = 16
   val data1: SrcBundle = SrcBundle(
    vs1 = "hC129999ABF4CCCCD",
    vs2 = "h0"
  )
  val Compareresult = new ArrayBuffer[String]()
  val QueueInput = new ArrayBuffer[LaneFUInput]()
  val Compareoutput = new ArrayBuffer[LaneFUOutput]()
  def FPTest(): Unit = {
    it should " Test with Lane input during FP funciton" in {
      
      val op_f2xu_32 = VCtrlBundle(funct6 = 18, vsew = 2)
      val op_f2x_32 = op_f2xu_32.copy(cvtSigned = true)
      val op_f2xu_rtz_32 = op_f2xu_32.copy(cvtRm = 2)
      val op_f2x_rtz_32 = op_f2x_32.copy(cvtRm = 2)
      val ControlArraynegative = new ArrayBuffer[VCtrlBundle]()
      ControlArraynegative += op_f2xu_32 
      ControlArraynegative += op_f2x_32
      ControlArraynegative += op_f2xu_rtz_32
      ControlArraynegative += op_f2x_rtz_32
      val fflags = Seq("h10", "h01", "h11", "h01")
      val results = Seq("h0", "hfffffff5ffffffff", "h0", "hfffffff600000000")
      test(new VFPUTop).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
        dut.io.in.initSource().setSourceClock(dut.clock)
        dut.io.out.initSink().setSinkClock(dut.clock)
        dut.clock.setTimeout(50)
        dut.io.out.ready.poke(true.B)
        println("I:vfcvt.xu.f.v vd, vs2, vm -sew32 from FPTop")
        fork {
          for(i <- 0 until ControlArraynegative.length){
            QueueInput += genInput(data1, ControlArraynegative(i))
          }
          dut.io.in.enqueueSeq(QueueInput
          )
        }.fork {
          for(i <- 0 until Compareresult.length-1){
            Compareoutput += new (LaneFUOutput).Lit(
              _.uop -> genVExpdUOp(ControlArraynegative(i)),
              _.vd -> results(i).asUInt,
              _.fflags -> fflags(i).asUInt,
              _.vxsat -> false.B
            )
          }
          dut.io.out.expectDequeueSeq(Compareoutput
          )
        }.join()
      }
    }
  }
    
  def VFCVTf2itest(): Unit = {
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
    println("U:vfcvt.xu.f.v vd, vs2, vm -sew32")
    println("U:vfcvt.xu.f.v vd, vs2, vm -sew64")
    println("U:vfcvt.x.f.v vd, vs2, vm  -sew32")
    println("U:vfcvt.x.f.v vd, vs2, vm  -sew64")
    println("U:vfcvt.rtz.xu.f.v vd, vs2, vm -sew32")
    println("U:vfcvt.rtz.xu.f.v vd, vs2, vm -sew64")
    println("U:vfcvt.rtz.x.f.v vd, vs2, vm  -sew32")
    println("U:vfcvt.rtz.x.f.v vd, vs2, vm  -sew64")

    println("U:vfwcvt.xu.f.v vd, vs2, vm ")
    println("U:vfwcvt.x.f.v vd, vs2, vm")
    println("U:vfwcvt.rtz.xu.f.v vd, vs2, vm ")
    println("U:vfwcvt.rtz.x.f.v vd, vs2, vm")

    println("U:vfncvt.xu.f.w vd, vs2, vm ")
    println("U:vfncvt.x.f.w vd, vs2, vm")
    println("U:vfncvt.rtz.xu.f.w vd, vs2, vm")
    println("U:vfncvt.rtz.x.f.w vd, vs2, vm")
    it should " Test with Lane input during FP funciton" in {
      val ops_32 = Seq(op_f2xu_32, op_f2x_32, op_f2xu_rtz_32, op_f2x_rtz_32)
      val ops_64 = Seq(op_f2xu_64, op_f2x_64, op_f2xu_rtz_64, op_f2x_rtz_64)
      test(new VFCVT).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
        dut.io.in.initSource().setSourceClock(dut.clock)
        dut.io.out.initSink().setSinkClock(dut.clock)
        dut.clock.setTimeout(50)
        dut.io.out.ready.poke(true.B)
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
  }
 }


class FloatTestTop extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with IntegerFPBehavior {
  behavior of "- FP"
  //it should behave like VFCVTf2itest
  it should behave like FPTest
}

class FloatTestCVT extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with IntegerFPBehavior {
  behavior of "- FP"
  it should behave like VFCVTf2itest
  //it should behave like FPTest
}
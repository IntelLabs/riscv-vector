package vpu.fp

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import darecreek.exu.fp._
import darecreek.{SrcBundle,VCtrlBundle,BundleGenHelper}
import darecreek.exu.fp.{LaneFloatFUIn, LaneFloatFUOut, MulToAddIOVec, VFPU, VFPUCtrlSigs, VFPUOp, VFPUSubModule, WithVFPUConfig}
import darecreek.{LaneFUInput,LaneFUOutput, VCtrl, VExcptInfo, VExpdUOp, VInfo, VPRegIdxWidth, VRobPtr, xLen}
import darecreek.exu.fp.fudian.{FMULToFADD, FMULToFADD_fflags, FloatPoint}

object TestHarness {

  def test_init(dut: VFPUSubModule): Unit = {
    dut.clock.setTimeout(50)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)
  }

  def test_simple(dut: VFPUSubModule, in: LaneFloatFUIn): Unit = {
    dut.io.in.enqueueNow(in)
    dut.clock.step(5)
  }

  def test_bubble(dut: VFPUSubModule, bubble: Int, in1: LaneFloatFUIn, in2: LaneFloatFUIn): Unit = {
    test_init(dut)
    dut.io.in.enqueueNow(in1)
    dut.clock.step(bubble)
    dut.io.in.enqueueNow(in2)
    dut.clock.step(5)
  }

  def test_bubble_seq(dut: VFPUSubModule, bubbles: Seq[Int], inSeq: Seq[LaneFloatFUIn]): Unit = {
    require(bubbles.length == inSeq.length - 1)
    test_init(dut)
    (0 +: bubbles).zip(inSeq).foreach(x => {
      dut.clock.step(x._1)
      dut.io.in.enqueueNow(x._2)
    })
    dut.clock.step(6)
  }
}
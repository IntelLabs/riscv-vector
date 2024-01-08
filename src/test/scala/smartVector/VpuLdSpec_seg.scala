package smartVector

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import smartVector._
import darecreek.ctrl.decode.VInstructions._
import SmartParam._

trait SmartVectorBehavior_ld_seg {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldstReqCtrl_default = CtrlBundle()
    val ldstReqSrc_default  = SrcBundleLdst()

    def VLSEG2E8_V  = "b001_000_1_00000_00001_000_00010_0000111"
    
    def VLSEG3E64_V = "b010_000_1_00000_00001_111_00010_0000111"

    def VLSEG4SE32_V = "b011_010_1_00010_00001_110_00010_0000111"

    def vLsuTest0(): Unit = {
        it should "pass: unit-stride segment load (uops=4, eew=8, vl=16, vstart=0, segment=2)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (CtrlBundle(instrn=VLSEG2E8_V), SrcBundleLdst()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            // dut.clock.step(100)
            dut.clock.step(1)
            dut.io.rfData(2).expect("hdc9854100f0f0f0fffffffff2367abef".U)
            dut.io.rfData(3).expect("h0".U)
            dut.io.rfData(4).expect("hfeba76320f0f0f0fffffffff014589cd".U)
            dut.io.rfData(5).expect("h0".U)
            dut.clock.step(100)
        }
        }
    }

    def vLsuTest1(): Unit = {
        it should "pass: unit-stride segment load (uops=4, eew=64, vl=16, vstart=0, segment=3)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (CtrlBundle(instrn=VLSEG3E64_V, vlmul=0, vl=2), SrcBundleLdst()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            // dut.clock.step(100)
            dut.clock.step(1)
            dut.io.rfData(2).expect("hfedcba98765432100123456789abcdef".U)
            dut.io.rfData(3).expect("h1234567890123456ffffffffffffffff".U)
            dut.io.rfData(4).expect("h01010101010101010f0f0f0f0f0f0f0f".U)
            dut.clock.step(100)
        }
        }
    }

    def vLsuTest2(): Unit = {
        it should "pass: strided segment load (uops=4, eew=64, vl=2, vstart=0, segment=4, stride=4)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (CtrlBundle(instrn=VLSEG4SE32_V, vlmul=0, vl=3), SrcBundleLdst(rs2="h4")),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            // dut.clock.step(100)
            dut.clock.step(1)
            dut.io.rfData(2).expect("hffffffff0123456789abcdef".U)
            dut.io.rfData(3).expect("hffffffffffffffff01234567".U)
            dut.io.rfData(4).expect("h0f0f0f0fffffffffffffffff".U)
            dut.io.rfData(5).expect("h0f0f0f0f0f0f0f0fffffffff".U)
            dut.clock.step(100)
        }
        }
    }

}

class VPULdSpec_seg extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with SmartVectorBehavior_ld_seg {
  behavior of "SmartVector Load test"
    it should behave like vLsuTest0()   //
    it should behave like vLsuTest1()   //
    it should behave like vLsuTest2()   //
}
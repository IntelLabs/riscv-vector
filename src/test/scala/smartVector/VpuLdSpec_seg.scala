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

    def VLE8_V  = "b000_000_1_00000_00001_000_00010_0000111"

    def VLE16_V = "b000_000_1_00000_00001_101_00010_0000111"

    def VLSEG2E8_V  = "b001_000_1_00000_00001_000_00010_0000111"
    
    def VLSEG3E64_V = "b010_000_1_00000_00001_111_00010_0000111"

    def VLSEG4SE32_V = "b011_010_1_00010_00001_110_00010_0000111"

    // vluexi8 v6, 0(x1), vs2
    def VLOXSEG2EI8_V  = "b001_001_1_00010_00001_000_00110_0000111"

    def VLOXSEG3EI8_V  = "b010_001_1_00010_00001_000_00110_0000111"

    def VLOXSEG2EI16_V  = "b001_001_1_00010_00001_101_00110_0000111"

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

    def vLsuTest3(): Unit = {
         it should "pass: indexed segment load (uops=6, sew=32, eew=6, vl=8, vstart=0, segment=3)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE8_V, vl=8, vlmul=0), SrcBundleLdst(rs1="h1078")),
                (ldstReqCtrl_default.copy(instrn=VLOXSEG3EI8_V, vl=8, vlmul=1, vsew=2), ldstReqSrc_default.copy()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            dut.io.rfData(2).expect("h081814100c1c0004".U)

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(1)._1, ldReqs(1)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            dut.io.rfData(6).expect("hfffffffffedcba9889abcdef01234567".U)
            dut.io.rfData(7).expect("hffffffff765432100f0f0f0f0f0f0f0f".U)
            dut.io.rfData(8).expect("h0f0f0f0f9012345601234567ffffffff".U)
            dut.io.rfData(9).expect("hfffffffffedcba98765432100f0f0f0f".U)
            
        }
        }
    }

    def vLsuTest4(): Unit = {
        it should "pass: indexed segment load (uops=8, sew=8, eew=16, vl=16, vstart=0, segment=2)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE16_V, vl=16, vlmul=1, vsew=1), SrcBundleLdst(rs1="h1080")),
                (ldstReqCtrl_default.copy(instrn=VLOXSEG2EI16_V, vl=16, vlmul=1, vsew=0), SrcBundleLdst()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            dut.io.rfData(2).expect("h000000080004001c00080000000c0004".U)
            dut.io.rfData(3).expect("h00380004000c002000080004000c0018".U)

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(1)._1, ldReqs(1)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            dut.io.rfData(6).expect("h1167ff56ff67ff10efff6798ffefff67".U)
            dut.io.rfData(8).expect("h1145ff34ff45ff32cdff45baffcdff45".U)
        }
        }
    }

}

class VPULdSpec_seg extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with SmartVectorBehavior_ld_seg {
  behavior of "SmartVector Load test"
    it should behave like vLsuTest0()   //
    it should behave like vLsuTest1()   //
    it should behave like vLsuTest2()   //
    it should behave like vLsuTest3()   //
    it should behave like vLsuTest4()   //
}
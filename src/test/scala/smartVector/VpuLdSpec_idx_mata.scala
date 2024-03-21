package smartVector

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chiseltest.WriteVcdAnnotation
import darecreek.exu.vfu.VFuParamsKey
import darecreek.exu.vfu.VFuParameters
import xiangshan.XSCoreParamsKey
import xiangshan.XSCoreParameters
import smartVector.lsutest.LSUFakeDCache
import smartVector._
import darecreek.ctrl.decode.VInstructions._
import SmartParam._


trait SmartVectorBehavior_ld_idx_mata {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldstReqCtrl_default = CtrlBundle()
    val ldstReqSrc_default  = SrcBundleLdst()

    // def VLE8_V             = BitPat("b???000?00000?????000?????0000111")
    // vle8 v2, 0(x1), 0x0
    def VLE8_V  = "b000_000_1_00000_00001_000_00010_0000111"

    def VLE16_V = "b000_000_1_00000_00001_101_00010_0000111"

    def VLE32_V = "b000_000_1_00000_00001_110_00010_0000111"
    
    def VLE64_V = "b000_000_1_00000_00001_111_00010_0000111"

    // def VLSE8_V            = BitPat("b???010???????????000?????0000111")
    def VLSE8_V  = "b000_010_1_00010_00001_000_00010_0000111"

    def VLSE16_V = "b000_010_1_00010_00001_101_00010_0000111"

    def VLSE32_V = "b000_010_1_00010_00001_110_00010_0000111"
    
    def VLSE64_V = "b000_010_1_00010_00001_111_00010_0000111"

    // BitPat("b???001???????????000?????0000111")
    // vluexi8 v6, 0(x1), vs2
    def VLUXEI8_V   = "b000_001_1_00010_00001_000_00110_0000111"

    def VLUXEI16_V  = "b000_001_1_00010_00001_101_00110_0000111"

  
    def vLsuTest0(): Unit = {
        it should "pass: indexed load (uops=1, sew=8, eew=8, vl=8, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE8_V, vl=8, vlmul=1, ma=true, ta=true), SrcBundleLdst(rs1="h1068")),
                (ldstReqCtrl_default.copy(instrn=VLUXEI8_V, vl=8, vlmul=1, ma=true, ta=true), ldstReqSrc_default.copy()),
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
            dut.io.rfData(2).expect("hffffffffffffffff0807060504030201".U)

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(1)._1, ldReqs(1)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            dut.io.rfData(6).expect("hffffffffffffffffff0123456789abcd".U)
        }
        }
    }

    def vLsuTest1(): Unit = {
        it should "pass: indexed load (uops=1, sew=32, eew=8, vl=8, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE8_V, vl=8, vlmul=1, ma=true, ta=true), SrcBundleLdst(rs1="h1078")),
                (ldstReqCtrl_default.copy(instrn=VLUXEI8_V, vl=8, vlmul=1, vsew=2, ma=true, ta=true), ldstReqSrc_default.copy()),
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
            dut.io.rfData(2).expect("hffffffffffffffff081814100c1c0004".U)

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
        }
        }
    }

    def vLsuTest2(): Unit = {
        it should "pass: indexed load (uops=1, sew=16, eew=16, vl=8, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE16_V, vl=8, vlmul=1, ma=true, ta=true), SrcBundleLdst(rs1="h1080")),
                (ldstReqCtrl_default.copy(instrn=VLUXEI16_V, vl=8, vlmul=1, vsew=1, ma=true, ta=true), ldstReqSrc_default.copy()),
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
            dut.io.rfData(2).expect("h00080004001c00080000000c0004".U)

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(1)._1, ldReqs(1)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            dut.io.rfData(6).expect("hcdefffff4567ba98ffffcdefffff4567".U)
        }
        }
    }

    def vLsuTest3(): Unit = {
        it should "pass: indexed load (uops=4, sew=8, eew=16, vl=16, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE16_V, vl=16, vlmul=1, ma=true, ta=true), SrcBundleLdst(rs1="h1080")),
                (ldstReqCtrl_default.copy(instrn=VLUXEI16_V, vl=16, vlmul=1, vsew=0, ma=true, ta=true), ldstReqSrc_default.copy()),
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
        }
        }
    }
}

class VPULdSpec_idx_mata extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with SmartVectorBehavior_ld_idx_mata {
  behavior of "SmartVector Load test"
    it should behave like vLsuTest0()   //
    it should behave like vLsuTest1()   // 
    it should behave like vLsuTest2()   // 
    it should behave like vLsuTest3()   //
}
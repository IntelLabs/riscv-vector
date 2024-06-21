package smartVector

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import smartVector._
import darecreek.ctrl.decode.VInstructions._
import SmartParam._

trait SmartVectorBehavior_ld {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldstReqCtrl_default = CtrlBundle()
    val ldstReqSrc_default  = SrcBundleLdst()

    // def VLE8_V             = BitPat("b???000?00000?????000?????0000111")
    // vle8 v4, 0(x1), 0x0
    def VLE8_V  = "b000_000_1_00000_00001_000_00100_0000111"

    def VLE16_V = "b000_000_1_00000_00001_101_00100_0000111"

    def VLE32_V = "b000_000_1_00000_00001_110_00100_0000111"
    
    def VLE64_V = "b000_000_1_00000_00001_111_00100_0000111"
    def VSE8_V  = "b000_000_1_00000_00001_000_01000_0100111"
    // def VLSE8_V            = BitPat("b???010???????????000?????0000111")
    def VLSE8_V  = "b000_010_1_00010_00001_000_00100_0000111"

    def VLSE16_V = "b000_010_1_00010_00001_101_00100_0000111"

    def VLSE32_V = "b000_010_1_00010_00001_110_00100_0000111"
    
    def VLSE64_V = "b000_010_1_00010_00001_111_00100_0000111"

    // BitPat("b???001???????????000?????0000111")
    // vluexi8 v8, 0(x1), vs4
    def VLUXEI8_V   = "b000_001_1_00100_00001_000_01000_0000111"

    def VLUXEI16_V  = "b000_001_1_00100_00001_101_01000_0000111"

  
    def vLsuTest0(): Unit = {
        it should "pass: unit-stride load (uops=1, eew=8, vl=16, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE8_V), ldstReqSrc_default.copy()),
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
            dut.io.rfData(4).expect("hffffffffffffffff0123456789abcdef".U)
            dut.clock.step(100)
        }
        }
    }

    def vLsuTest1(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=8, vl=19, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE8_V, vl=19, vlmul=2, ma=true, ta=true), ldstReqSrc_default.copy()),
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
            // dut.clock.step(100)
            dut.io.rfData(4).expect("hffffffffffffffff0123456789abcdef".U)
            dut.io.rfData(5).expect("h000000000000000000000000000f0f0f".U)
        }
        }
    }

    def vLsuTest2(): Unit = {
        it should "pass: unit-stride load (uops=4, eew=16, vl=27, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE16_V, vl=27, vlmul=2, vsew=1), ldstReqSrc_default.copy()),
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
            // dut.clock.step(100)
            dut.io.rfData(4).expect("hffffffffffffffff0123456789abcdef".U)
            dut.io.rfData(5).expect("hfedcba98765432100f0f0f0f0f0f0f0f".U)
            dut.io.rfData(6).expect("h01010101010101011234567890123456".U)
            dut.io.rfData(7).expect("h00000000000000000000678901234567".U)
        }
        }
    }

    def vLsuTest3(): Unit = {
        it should "pass: unit-stride load (uops=3, eew=32, vl=10, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE32_V, vl=10, vlmul=2, vsew=2), ldstReqSrc_default.copy()),
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
            // dut.clock.step(100)
            dut.io.rfData(4).expect("hffffffffffffffff0123456789abcdef".U)
            dut.io.rfData(5).expect("hfedcba98765432100f0f0f0f0f0f0f0f".U)
            dut.io.rfData(6).expect("h00000000000000001234567890123456".U)
        }
        }
    }

    def vLsuTest4(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=64, vl=3, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE64_V, vl=3, vlmul=1, vsew=3), ldstReqSrc_default.copy()),
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
            // dut.clock.step(100)
            dut.io.rfData(4).expect("hffffffffffffffff0123456789abcdef".U)
            dut.io.rfData(5).expect("h00000000000000000f0f0f0f0f0f0f0f".U)
        }
        }
    }

    def vLsuTest5(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=64, vl=3, vstart=1)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE64_V, vl=3, vlmul=1, vstart=1, vsew=3), ldstReqSrc_default.copy()),
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
            // dut.clock.step(100)
            dut.io.rfData(4).expect("hffffffffffffffff0000000000000000".U)
            dut.io.rfData(5).expect("h00000000000000000f0f0f0f0f0f0f0f".U)
        }
        }
    }

    def vLsuTest6(): Unit = {
        it should "pass: strided load (uops=1, eew=8, vl=6, vstart=0, stride=-5)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLSE8_V, vl=6, vlmul=1, vsew=0), ldstReqSrc_default.copy(rs2="hffffffff_fffffffb")),
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
            // dut.clock.step(100)
            dut.io.rfData(4).expect("h0000000000000000000020103478eeef".U)
        }
        }
    }

    def vLsuTest7(): Unit = {
        it should "pass: strided load (uops=2, eew=64, vl=3, vstart=0, stride=-1)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLSE32_V, vl=3, vlmul=1, vsew=2), ldstReqSrc_default.copy(rs2="hffffffff_ffffffff")),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.exception_vld.peekBoolean()) {
                dut.clock.step(1)
            }

            dut.io.rvuCommit.exception_vld.expect(true.B)
            dut.io.rvuCommit.xcpt_addr.expect("h0fff".U)
            dut.io.rvuCommit.update_vl.expect(false.B)
            dut.io.rvuCommit.update_vl_data.expect(1.U)
            dut.clock.step(1)
            // dut.clock.step(100)
            dut.io.rfData(4).expect("h89abcdef".U)
        }
        }
    }

    def vLsuTest8(): Unit = {
        it should "pass: strided load (uops=2, eew=16, vl=10, vstart=0, stride=4)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLSE16_V, vl=10, vlmul=1, vsew=1), ldstReqSrc_default.copy(rs2="h8")),
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
            // dut.clock.step(100)
            dut.io.rfData(4).expect("h111145670101345632100f0fffffcdef".U)
            dut.io.rfData(5).expect("h00000000000000000000000033332222".U)
        }
        }
    }

    def vLsuTest9(): Unit = {
        it should "pass: unit-stride exception" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VSE8_V, vl=0, vlmul=1, vstart=1, vsew=0), ldstReqSrc_default.copy(rs1="h1058")),
                (ldstReqCtrl_default.copy(instrn=VLE8_V, vl=19, vlmul=1, vstart=1, vsew=0), ldstReqSrc_default.copy(rs1="h1058")),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }

            dut.io.rvuCommit.commit_vld.expect(true.B)

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(1)._1, ldReqs(1)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.exception_vld.peekBoolean()) {
                dut.clock.step(1)
            }

            dut.io.rvuCommit.exception_vld.expect(true.B)
            dut.io.rvuCommit.update_vl.expect(false.B)
            dut.io.rvuCommit.update_vl_data.expect(8.U)
            dut.clock.step(1)
            dut.io.rfData(4).expect("h00000000000000005555555555555500".U)
        }
        }
    }

    // def vLsuTest10(): Unit = {
    //     it should "pass: unit-stride vstart >= vl" in {
    //     test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
    //         dut.clock.setTimeout(200)
    //         dut.clock.step(1)
    //         val ldReqs = Seq(
    //             (ldstReqCtrl_default.copy(instrn=VLE8_V, vl=30, vlmul=1, vstart=32, vsew=0), ldstReqSrc_default.copy()),
    //         )

    //         dut.io.rvuIssue.valid.poke(true.B)
    //         dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(0)._1, ldReqs(0)._2))
    //         dut.clock.step(1)
    //         dut.io.rvuIssue.valid.poke(false.B)

    //         while (!dut.io.rvuCommit.exception_vld.peekBoolean()) {
    //             dut.clock.step(1)
    //         }

    //         dut.io.rvuCommit.exception_vld.expect(true.B)
    //         dut.io.rvuCommit.update_vl.expect(true.B)
    //         dut.io.rvuCommit.update_vl_data.expect(8.U)
    //         dut.clock.step(1)
    //         dut.io.rfData(4).expect("hffffffffffffffff55555555555555ff".U)
    //     }
    //     }
    // }
}

class VPULdSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with SmartVectorBehavior_ld {
  behavior of "SmartVector Load test"
    it should behave like vLsuTest0()   //
    it should behave like vLsuTest1()   //
    it should behave like vLsuTest2()   // 
    it should behave like vLsuTest3()   //
    it should behave like vLsuTest4()   //
    it should behave like vLsuTest5()   //
    it should behave like vLsuTest6()   //
    it should behave like vLsuTest7()   //
    it should behave like vLsuTest8()   //
    it should behave like vLsuTest9()   //
    // it should behave like vLsuTest10()   //
}
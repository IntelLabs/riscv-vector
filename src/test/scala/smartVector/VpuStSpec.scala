package smartVector

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chiseltest.WriteVcdAnnotation
import smartVector._
import darecreek.ctrl.decode.VInstructions._
import SmartParam._

trait SmartVectorBehavior_st {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldstReqCtrl_default = CtrlBundle()
    val ldstReqSrc_default  = SrcBundleLdst()

    // def VLE8_V             = BitPat("b???000?00000?????000?????0000111")
    // vle8 v8, 0(x1), 0x0
    def VLE8_V  = "b000_000_1_00000_00001_000_01000_0000111"

    def VLE16_V = "b000_000_1_00000_00001_101_01000_0000111"

    def VLE32_V = "b000_000_1_00000_00001_110_01000_0000111"
    
    def VLE64_V = "b000_000_1_00000_00001_111_01000_0000111"

    // def VLSE8_V            = BitPat("b???010???????????000?????0000111")
    def VLSE8_V  = "b000_010_1_00010_00001_000_01000_0000111"

    def VLSE16_V = "b000_010_1_00010_00001_101_01000_0000111"

    def VLSE32_V = "b000_010_1_00010_00001_110_01000_0000111"
    
    def VLSE64_V = "b000_010_1_00010_00001_111_01000_0000111"

    // def VLE8_V             = BitPat("b???000?00000?????000?????0000111")
    // vle8 v2, 0(x1), 0x0
    def VSE8_V  = "b000_000_1_00000_00001_000_01000_0100111"

    def VSE16_V = "b000_000_1_00000_00001_101_01000_0100111"

    def VSE32_V = "b000_000_1_00000_00001_110_01000_0100111"
    
    def VSE64_V = "b000_000_1_00000_00001_111_01000_0100111"

    // def VSSE8_V            = BitPat("b???010???????????000?????0100111")
    def VSSE8_V  = "b000_010_1_00010_00001_000_01000_0100111"

    def VSSE16_V = "b000_010_1_00010_00001_101_01000_0100111"

    def VSSE32_V = "b000_010_1_00010_00001_110_01000_0100111"
    
    def VSSE64_V = "b000_010_1_00010_00001_111_01000_0100111"


    var addressMap = Map(
        "h0fd0" -> 0,
        "h0fd8" -> 1,
        "h0fe0" -> 2,
        "h0fe8" -> 3,
        "h0ff0" -> 4,
        "h0ff8" -> 5,
        "h1000" -> 6,
        "h1008" -> 7,
        "h1010" -> 8,
        "h1018" -> 9,
        "h1020" -> 10,
        "h1028" -> 11,
        "h1030" -> 12,
        "h1038" -> 13,
        "h1040" -> 14,
        "h1048" -> 15,
        "h1050" -> 16,
        "h1058" -> 17,
        "h1060" -> 18,
        "h1068" -> 19,
        "h1070" -> 20,
    )

    val index0fd0 = addressMap("h0fd0")
    val index0fd8 = addressMap("h0fd8")
    val index0fe0 = addressMap("h0fe0")
    val index0fe8 = addressMap("h0fe8")
    val index0ff0 = addressMap("h0ff0")
    val index0ff8 = addressMap("h0ff8")
    val index1000 = addressMap("h1000")
    val index1008 = addressMap("h1008")
    val index1010 = addressMap("h1010")
    val index1018 = addressMap("h1018")
    val index1020 = addressMap("h1020")
    val index1028 = addressMap("h1028")
    val index1030 = addressMap("h1030")
    val index1038 = addressMap("h1038")
    val index1040 = addressMap("h1040")
    val index1048 = addressMap("h1048")
    val index1050 = addressMap("h1050")
    val index1058 = addressMap("h1058")
    val index1060 = addressMap("h1060")
    val index1068 = addressMap("h1068")
    val index1070 = addressMap("h1070")
  
    def vLsuTest0(): Unit = {
        it should "pass: unit-stride store (uops=1, eew=8, vl=16, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE8_V), ldstReqSrc_default.copy()),
            )
            val stReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VSE8_V, vl=4), SrcBundleLdst(rs1="h1008")),
            )

            /**************load*********************/
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
            dut.io.rfData(8).expect("hffffffffffffffff0123456789abcdef".U)

            /*************store*****************/

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(stReqs(0)._1, stReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            // dut.clock.step(100)
            dut.clock.step(1)
            
            dut.io.memInfo(index1008).expect("hffffffff89abcdef".U)
            // dut.io.memInfo(index1010).expect("hffffffffffffffff".U)
        }
        }
    }

    def vLsuTest1(): Unit = {
        it should "pass: unit-stride store (uops=2, eew=8, vl=19, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE8_V, vl=19, vlmul=2), ldstReqSrc_default.copy()),
            )
            val stReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VSE8_V, vl=19, vlmul=2), SrcBundleLdst(rs1="h1008")),
            )

            /**************load*********************/
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
            dut.io.rfData(8).expect("hffffffffffffffff0123456789abcdef".U)
            dut.io.rfData(9).expect("h000000000000000000000000000f0f0f".U)

            /*************store*****************/

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(stReqs(0)._1, stReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            // dut.clock.step(100)
            dut.clock.step(1)
            
            dut.io.memInfo(index1008).expect("h0123456789abcdef".U)
            dut.io.memInfo(index1010).expect("hffffffffffffffff".U)
            dut.io.memInfo(index1018).expect("hfedcba98760f0f0f".U)
        }
        }
    }

    def vLsuTest2(): Unit = {
        it should "pass: unit-stride store (uops=4, eew=16, vl=27, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE16_V, vl=27, vlmul=2, vsew=1), ldstReqSrc_default.copy()),
            )
            val stReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VSE16_V, vl=27, vlmul=2, vsew=1), SrcBundleLdst(rs1="h1008")),
            )

            /**************load*********************/
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
            dut.io.rfData( 8).expect("hffffffffffffffff0123456789abcdef".U)
            dut.io.rfData( 9).expect("hfedcba98765432100f0f0f0f0f0f0f0f".U)
            dut.io.rfData(10).expect("h01010101010101011234567890123456".U)
            dut.io.rfData(11).expect("h00000000000000000000678901234567".U)

            /*************store*****************/

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(stReqs(0)._1, stReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            // dut.clock.step(100)
            dut.clock.step(1)
            
            dut.io.memInfo(index1008).expect("h0123456789abcdef".U)
            dut.io.memInfo(index1010).expect("hffffffffffffffff".U)
            dut.io.memInfo(index1018).expect("h0f0f0f0f0f0f0f0f".U)
            dut.io.memInfo(index1020).expect("hfedcba9876543210".U)
            dut.io.memInfo(index1028).expect("h1234567890123456".U)
            dut.io.memInfo(index1030).expect("h0101010101010101".U)
            dut.io.memInfo(index1038).expect("h1111678901234567".U)
        }
        }
    }

    def vLsuTest3(): Unit = {
        it should "pass: strided store (uops=2, eew=16, vl=10, vstart=0, stride=4)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLSE16_V, vl=10, vlmul=1, vsew=1), ldstReqSrc_default.copy(rs2="h8")),
            )
            val stReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VSSE16_V, vl=10, vlmul=1, vsew=1), SrcBundleLdst(rs1="h1008", rs2="h8")),
            )

            /**************load*********************/
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
            dut.io.rfData(8).expect("h111145670101345632100f0fffffcdef".U)
            dut.io.rfData(9).expect("h00000000000000000000000033332222".U)
            /*************store*****************/

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(stReqs(0)._1, stReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            // dut.clock.step(100)
            dut.clock.step(1)
            dut.io.memInfo(index1008).expect("hffffffffffffcdef".U)
            dut.io.memInfo(index1010).expect("h0f0f0f0f0f0fffff".U)
            dut.io.memInfo(index1018).expect("hfedcba9876540f0f".U)
            dut.io.memInfo(index1020).expect("h1234567890123210".U)
            dut.io.memInfo(index1028).expect("h0101010101013456".U)

            dut.io.memInfo(index1050).expect("h4444444444443333".U)
        }
        }
    }
}

class VPUStSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with SmartVectorBehavior_st {
  behavior of "SmartVector Store test"
    it should behave like vLsuTest0()   //
    it should behave like vLsuTest1()   //
    it should behave like vLsuTest2()   // 
    it should behave like vLsuTest3()   //
}
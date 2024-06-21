package smartVector

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import smartVector._
import darecreek.ctrl.decode.VInstructions._
import SmartParam._

trait SmartVectorBehavior_ld_iex {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldstReqCtrl_default = CtrlBundle()
    val ldstReqSrc_default  = SrcBundleLdst()

    // def VLE8_V             = BitPat("b???000?00000?????000?????0000111")
    // vle8 v4, 0(x1), 0x0
    def VLE8_V  = "b000_000_1_00000_00001_000_00100_0000111"

    def VLE16_V = "b000_000_1_00000_00001_101_00100_0000111"

    def VLE32_V = "b000_000_1_00000_00001_110_00100_0000111"
    
    def VLE64_V = "b000_000_1_00000_00001_111_00100_0000111"

    // def VLSE8_V            = BitPat("b???010???????????000?????0000111")
    def VLSE8_V  = "b000_010_1_00010_00001_000_00100_0000111"

    def VLSE16_V = "b000_010_1_00010_00001_101_00100_0000111"

    def VLSE32_V = "b000_010_1_00010_00001_110_00100_0000111"
    
    def VLSE64_V = "b000_010_1_00010_00001_111_00100_0000111"

    // BitPat("b???001???????????000?????0000111")
    // vluexi8 v8, 0(x1), vs4
    def VLUXEI8_V   = "b000_001_1_00100_00001_000_01000_0000111"

    def VLUXEI16_V  = "b000_001_1_00100_00001_101_01000_0000111"

    // vaddu.vv v8, v4, v2
    // "b000000???????????000?????1010111"
    def VADDU_VV    = "b000_000_1_00100_00010_000_01000_1010111"


  
    def vLsuTest0(): Unit = {
        it should "pass: unit-stride load (uops=1, eew=8, vl=16, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (ldstReqCtrl_default.copy(instrn=VLE8_V, vlmul=0), ldstReqSrc_default.copy()),
                (ldstReqCtrl_default.copy(instrn=VADDU_VV, vlmul=0), ldstReqSrc_default.copy()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(1)._1, ldReqs(1)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            // dut.clock.step(100)
            dut.clock.step(1)
            dut.io.rfData(4).expect("hffffffffffffffff0123456789abcdef".U)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            dut.io.rfData(8).expect("hffffffffffffffff0123456789abcdef".U)
            dut.clock.step(100)
        }
        }
        // fork {
        //     for ((c, s, r, m) <- ldReqs) {
        //         while (!dut.io.lsuReady.peekBoolean()) {
        //             dut.clock.step(1)
        //         }
        //         dut.io.mUop.valid.poke(true.B)
        //         dut.io.mUop.bits.poke(genLdInput(c, s))
        //         dut.clock.step(1)
        //         dut.io.mUop.valid.poke(false.B)
        //     }
        // }.fork {
        //     for ((c, s, r, m) <- ldReqs) {
                
        //         while (!dut.io.lsuOut.valid.peekBoolean()) {
        //             dut.clock.step(1)
        //         }
        //         dut.io.lsuOut.valid.expect(true.B)
        //         dut.io.lsuOut.bits.data.expect(r)
        //         dut.io.lsuOut.bits.rfWriteMask.expect(m)
        //         dut.clock.step(1)
        //     }
        // }.join()
    }

    // def vLsuTest1(): Unit = {
    //     it should "pass: unit-stride load (uops=2, eew=8, vl=19, vstart=0)" in {
    //     test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
    //         dut.clock.setTimeout(200)
    //         dut.clock.step(1)
    //         val ldReqs = Seq(
    //             (ldstReqCtrl_default.copy(instrn=VLE8_V, vl=19, vlmul=2, ma=true, ta=true), ldstReqSrc_default.copy()),
    //         )

    //         dut.io.rvuIssue.valid.poke(true.B)
    //         dut.io.rvuIssue.bits.poke(genLdstInput(ldReqs(0)._1, ldReqs(0)._2))
    //         dut.clock.step(1)
    //         dut.io.rvuIssue.valid.poke(false.B)

    //         while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
    //             dut.clock.step(1)
    //         }
    //         dut.io.rvuCommit.commit_vld.expect(true.B)
    //         dut.clock.step(1)
    //         // dut.clock.step(100)
    //         dut.io.rfData(4).expect("hffffffffffffffff0123456789abcdef".U)
    //         dut.io.rfData(5).expect("h000000000000000000000000000f0f0f".U)
    //     }
    //     }
    // }

}

class VPULdIEXSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with SmartVectorBehavior_ld_iex {
  behavior of "SmartVector Load test"
    it should behave like vLsuTest0()   //
    // it should behave like vLsuTest1()   //
    // it should behave like vLsuTest2()   // 
    // it should behave like vLsuTest3()   //
    // it should behave like vLsuTest4()   //
    // it should behave like vLsuTest5()   //
    // it should behave like vLsuTest6()   //
    // it should behave like vLsuTest7()   //
    // it should behave like vLsuTest8()   //
    // it should behave like vLsuTest9()   //
}

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
import smartVector.lsutest.FakeLdDCache
import smartVector._
import darecreek.ctrl.decode.VInstructions._
import SmartParam._

class SmartVectorTestWrapper extends Module {
    val io = IO(new Bundle{
        val rvuIssue = Flipped(Decoupled(new VIssueTest))
        val rvuCommit = Output(new RVUCommit)
        val rfData = Output(Vec(NVPhyRegs, UInt(VLEN.W)))
    })

    val p = Parameters.empty.alterPartial({
        case SmartParamsKey => SmartParameters(VLEN = 128)
        case VFuParamsKey   => VFuParameters(XLEN = 64, VLEN = 128)
        case XSCoreParamsKey => XSCoreParameters()
    })

    val smartVector = Module(new SmartVector())
  
    smartVector.io.in.valid := io.rvuIssue.valid
    smartVector.io.in.bits.inst := io.rvuIssue.bits.ctrl.inst
    smartVector.io.in.bits.rs1 := io.rvuIssue.bits.rs1
    smartVector.io.in.bits.rs2 := io.rvuIssue.bits.rs2
    smartVector.io.in.bits.vInfo.vl := io.rvuIssue.bits.ctrl.info_vl
    smartVector.io.in.bits.vInfo.vstart := io.rvuIssue.bits.ctrl.info_vstart
    smartVector.io.in.bits.vInfo.vma := io.rvuIssue.bits.ctrl.info_ma
    smartVector.io.in.bits.vInfo.vta := io.rvuIssue.bits.ctrl.info_ta
    smartVector.io.in.bits.vInfo.vsew := io.rvuIssue.bits.ctrl.info_vsew
    smartVector.io.in.bits.vInfo.vlmul := io.rvuIssue.bits.ctrl.info_vlmul
    smartVector.io.in.bits.vInfo.vxrm := 0.U
    smartVector.io.in.bits.vInfo.frm := 0.U
    

    io.rvuIssue.ready := smartVector.io.in.ready
    io.rfData := smartVector.io.rfData
    io.rvuCommit <> smartVector.io.out.rvuCommit

    val dcache = Module(new FakeLdDCache)
    smartVector.io.rvuMemory <> dcache.io
}


trait SmartVectorBehavior_ld {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldReqCtrl_default = CtrlBundle()
    val ldReqSrc_default  = SrcBundleLd()

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

  
    def vLsuTest0(): Unit = {
        it should "pass: unit-stride load (uops=1, eew=8, vl=8, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            
            next_is_load_and_step(dut)

            val ldReqs = Seq(
                (ldReqCtrl_default.copy(instrn=VLE8_V), ldReqSrc_default.copy()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            // dut.clock.step(100)
            dut.clock.step(1)
            dut.io.rfData(2).expect("hffffffffffffffff0123456789abcdef".U)
            dut.clock.step(100)
        }
        }
    }

    def vLsuTest1(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=8, vl=19, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            
            next_is_load_and_step(dut)

            val ldReqs = Seq(
                (ldReqCtrl_default.copy(instrn=VLE8_V, vl=19, vlmul=2), ldReqSrc_default.copy()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            // dut.clock.step(100)
            dut.io.rfData(2).expect("hffffffffffffffff0123456789abcdef".U)
            dut.io.rfData(3).expect("h000000000000000000000000000f0f0f".U)
        }
        }
    }

    def vLsuTest2(): Unit = {
        it should "pass: unit-stride load (uops=4, eew=16, vl=27, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            
            next_is_load_and_step(dut)

            val ldReqs = Seq(
                (ldReqCtrl_default.copy(instrn=VLE16_V, vl=27, vlmul=2, vsew=1), ldReqSrc_default.copy()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            // dut.clock.step(100)
            dut.io.rfData(2).expect("hffffffffffffffff0123456789abcdef".U)
            dut.io.rfData(3).expect("hfedcba98765432100f0f0f0f0f0f0f0f".U)
            dut.io.rfData(4).expect("h01010101010101011234567890123456".U)
            dut.io.rfData(5).expect("h00000000000000000000678901234567".U)
        }
        }
    }

    def vLsuTest3(): Unit = {
        it should "pass: unit-stride load (uops=3, eew=32, vl=10, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            
            next_is_load_and_step(dut)

            val ldReqs = Seq(
                (ldReqCtrl_default.copy(instrn=VLE32_V, vl=10, vlmul=2, vsew=2), ldReqSrc_default.copy()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            // dut.clock.step(100)
            dut.io.rfData(2).expect("hffffffffffffffff0123456789abcdef".U)
            dut.io.rfData(3).expect("hfedcba98765432100f0f0f0f0f0f0f0f".U)
            dut.io.rfData(4).expect("h00000000000000001234567890123456".U)
        }
        }
    }

    def vLsuTest4(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=64, vl=3, vstart=0)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            
            next_is_load_and_step(dut)

            val ldReqs = Seq(
                (ldReqCtrl_default.copy(instrn=VLE64_V, vl=3, vlmul=1, vsew=3), ldReqSrc_default.copy()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            // dut.clock.step(100)
            dut.io.rfData(2).expect("hffffffffffffffff0123456789abcdef".U)
            dut.io.rfData(3).expect("h00000000000000000f0f0f0f0f0f0f0f".U)
        }
        }
    }

    def vLsuTest5(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=64, vl=3, vstart=1)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            
            next_is_load_and_step(dut)

            val ldReqs = Seq(
                (ldReqCtrl_default.copy(instrn=VLE64_V, vl=3, vlmul=1, vstart=1, vsew=3), ldReqSrc_default.copy()),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            // dut.clock.step(100)
            dut.io.rfData(2).expect("hffffffffffffffff0000000000000000".U)
            dut.io.rfData(3).expect("h00000000000000000f0f0f0f0f0f0f0f".U)
        }
        }
    }

    def vLsuTest6(): Unit = {
        it should "pass: strided load (uops=1, eew=8, vl=6, vstart=0, stride=-5)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            
            next_is_load_and_step(dut)

            val ldReqs = Seq(
                (ldReqCtrl_default.copy(instrn=VLSE8_V, vl=6, vlmul=1, vsew=0), ldReqSrc_default.copy(rs2="hffffffff_fffffffb")),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            // dut.clock.step(100)
            dut.io.rfData(2).expect("h0000000000000000000020103478eeef".U)
        }
        }
    }

    def vLsuTest7(): Unit = {
        it should "pass: strided load (uops=2, eew=64, vl=3, vstart=0, stride=-1)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            
            next_is_load_and_step(dut)

            val ldReqs = Seq(
                (ldReqCtrl_default.copy(instrn=VLSE32_V, vl=3, vlmul=1, vsew=2), ldReqSrc_default.copy(rs2="hffffffff_ffffffff")),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            // dut.clock.step(100)
            dut.io.rfData(2).expect("h00000000eeeeeeeeeeeeeeee89abcdef".U)
        }
        }
    }

    def vLsuTest8(): Unit = {
        it should "pass: strided load (uops=2, eew=16, vl=10, vstart=0, stride=4)" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            
            next_is_load_and_step(dut)

            val ldReqs = Seq(
                (ldReqCtrl_default.copy(instrn=VLSE16_V, vl=10, vlmul=1, vsew=1), ldReqSrc_default.copy(rs2="h8")),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.commit_vld.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.rvuCommit.commit_vld.expect(true.B)
            dut.clock.step(1)
            // dut.clock.step(100)
            dut.io.rfData(2).expect("h111145670101345632100f0fffffcdef".U)
            dut.io.rfData(3).expect("h00000000000000000000000033332222".U)
        }
        }
    }

    def vLsuTest9(): Unit = {
        it should "pass: unit-strde exception" in {
        test(new SmartVectorTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            
            next_is_load_and_step(dut)

            val ldReqs = Seq(
                (ldReqCtrl_default.copy(instrn=VLE8_V, vl=19, vlmul=1, vstart=1, vsew=0), ldReqSrc_default.copy(rs1="h1058")),
            )

            dut.io.rvuIssue.valid.poke(true.B)
            dut.io.rvuIssue.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.rvuIssue.valid.poke(false.B)

            while (!dut.io.rvuCommit.exception_vld.peekBoolean()) {
                dut.clock.step(1)
            }

            dut.io.rvuCommit.exception_vld.expect(true.B)
            dut.io.rvuCommit.update_vl.expect(true.B)
            dut.io.rvuCommit.update_vl_data.expect(8.U)
            dut.clock.step(1)
            dut.io.rfData(2).expect("h00000000000000005555555555555500".U)
        }
        }
    }
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
}
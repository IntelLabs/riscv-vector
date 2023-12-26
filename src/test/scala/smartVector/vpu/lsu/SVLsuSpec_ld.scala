package smartVector.lsutest

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import smartVector._
import darecreek.ctrl.decode.VInstructions._
import darecreek._
import darecreek.lsu._

trait VLsuBehavior_ld {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldReqSrc_default = SrcBundleLd()
    val vle8    = CtrlBundle(VLE8_V)
    val vle16   = CtrlBundle(VLE16_V)
    val vle32   = CtrlBundle(VLE32_V)
    val vle64   = CtrlBundle(VLE64_V)
    val vlm     = CtrlBundle(VLM_V)
    val vle8ff  = CtrlBundle(VLE8FF_V)
    val vle16ff = CtrlBundle(VLE16FF_V)
    val vle32ff = CtrlBundle(VLE32FF_V)
    val vle64ff = CtrlBundle(VLE64FF_V)
    val vl1re8  = CtrlBundle(VL1RE8_V)

    val vlse8   = CtrlBundle(VLSE8_V)
    val vlse16  = CtrlBundle(VLSE16_V)
    val vlse32  = CtrlBundle(VLSE32_V)
    val vlse64  = CtrlBundle(VLSE64_V)
    val vse8    = CtrlBundle(VSE8_V)
  
    def vLsuTest0(): Unit = {
        it should "pass: unit-stride load (uops=1, eew=8, vl=8, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle8.copy(vl=8, uopIdx=0, uopEnd=true), ldReqSrc_default, "h201f1e1d1c1b1a190123456789abcdef".U),
            )

            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                // dut.clock.step(100)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest1(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=8, vl=19, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle8.copy(vl=19, uopIdx=0, uopEnd=false), ldReqSrc_default, "hffffffffffffffff0123456789abcdef".U),
                (vle8.copy(vl=19, uopIdx=1, uopEnd=true),  ldReqSrc_default, "h201f1e1d1c1b1a1918171615140f0f0f".U),
            )
            
            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                // dut.clock.step(50)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }


    def vLsuTest2(): Unit = {
        it should "pass: unit-stride load (uops=4, eew=16, vl=27, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle16.copy(vl=27, uopIdx=0, uopEnd=false), ldReqSrc_default, "hffffffffffffffff0123456789abcdef".U),
                (vle16.copy(vl=27, uopIdx=1, uopEnd=false), ldReqSrc_default, "hfedcba98765432100f0f0f0f0f0f0f0f".U),
                (vle16.copy(vl=27, uopIdx=2, uopEnd=false), ldReqSrc_default, "h01010101010101011234567890123456".U),
                (vle16.copy(vl=27, uopIdx=3, uopEnd=true),  ldReqSrc_default, "h201f1e1d1c1b1a191817678901234567".U),
            )
            
            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                // dut.clock.step(100)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest3(): Unit = {
        it should "pass: unit-stride load (uops=3, eew=32, vl=10, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle32.copy(vl=10, uopIdx=0, uopEnd=false), ldReqSrc_default, "hffffffffffffffff0123456789abcdef".U),
                (vle32.copy(vl=10, uopIdx=1, uopEnd=false), ldReqSrc_default, "hfedcba98765432100f0f0f0f0f0f0f0f".U),
                (vle32.copy(vl=10, uopIdx=2, uopEnd=true),  ldReqSrc_default, "h201f1e1d1c1b1a191234567890123456".U),
            )
            
            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest4(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=64, vl=3, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut => 
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle64.copy(vl=3, uopIdx=0, uopEnd=false), ldReqSrc_default, "hffffffffffffffff0123456789abcdef".U),
                (vle64.copy(vl=3, uopIdx=1, uopEnd=true),  ldReqSrc_default, "h201f1e1d1c1b1a190f0f0f0f0f0f0f0f".U),
            )

            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest5(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=64, vl=3, vstart=1)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle64.copy(vl=3, vstart=1, uopIdx=0, uopEnd=false), ldReqSrc_default, "hffffffffffffffff1817161514131211".U),
                (vle64.copy(vl=3, vstart=1, uopIdx=1, uopEnd=true),  ldReqSrc_default, "h201f1e1d1c1b1a190f0f0f0f0f0f0f0f".U),
            )

            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest6(): Unit = {
        it should "pass: unit-stride mask load (uops=1, eew=8, vl=8, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlm.copy(vl=27, uopIdx=0, uopEnd=true), ldReqSrc_default, "h201f1e1d1c1b1a191817161589abcdef".U),
            )
            

            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                // dut.clock.step(100)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest7(): Unit = {
        it should "pass: unit-strde fault-first-only (uops=1, eew=8, vl=19, vstart=1)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle8ff.copy(vl=19, uopIdx=0, uopEnd=false, vstart=1), SrcBundleLd(scalar_opnd_1="h1058"), "h201f1e1d1c1b1a195555555555555511".U),
            )

            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.io.xcpt.exception_vld.expect(false.B)
                dut.io.xcpt.update_vl.expect(true.B)
                dut.io.xcpt.xcpt_cause.ma.ld.expect(false.B)
                dut.io.xcpt.update_data.expect(8.U)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest8(): Unit = {
        it should "pass: unit-strde fault-first-only (uops=1, eew=16, vl=4, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle16ff.copy(vl=4, uopIdx=0, uopEnd=true, vstart=0), SrcBundleLd(scalar_opnd_1="h1060"), "h201f1e1d1c1b1a191817161514131211".U),
            )

            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.io.xcpt.exception_vld.expect(true.B)
                dut.io.xcpt.update_vl.expect(true.B)
                dut.io.xcpt.xcpt_cause.ma.ld.expect(true.B)
                dut.io.xcpt.update_data.expect(0.U)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest9(): Unit = {
        it should "pass: unit-strde fault-first-only (uops=1, eew=32, vl=7, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle32ff.copy(vl=7, uopIdx=0, uopEnd=false, vstart=0), SrcBundleLd(scalar_opnd_1="h105c"), "h201f1e1d1c1b1a191817161555555555".U),
            )
            
            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                 dut.io.lsuOut.valid.expect(true.B)
                dut.io.xcpt.exception_vld.expect(false.B)
                dut.io.xcpt.update_vl.expect(true.B)
                dut.io.xcpt.xcpt_cause.ma.ld.expect(false.B)
                dut.io.xcpt.update_data.expect(1.U)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest10(): Unit = {
        it should "pass: unit-strde fault-first-only (uops=1, eew=64, vl=3, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle64ff.copy(vl=3, uopIdx=0, uopEnd=false, vstart=0), SrcBundleLd(scalar_opnd_1="h1050"), "h55555555555555554444444444444444".U),
                (vle64ff.copy(vl=3, uopIdx=1, uopEnd=true,  vstart=0), SrcBundleLd(scalar_opnd_1="h1050"), "h201f1e1d1c1b1a191817161514131211".U),
            )

            // req0
            while (!dut.io.lsuReady.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.mUop.valid.poke(true.B)
            dut.io.mUop.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.mUop.valid.poke(false.B)
            while (!dut.io.lsuOut.valid.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.lsuOut.bits.data.expect(ldReqs(0)._3)

            // req1
            while (!dut.io.lsuReady.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.mUop.valid.poke(true.B)
            dut.io.mUop.bits.poke(genLdInput(ldReqs(1)._1, ldReqs(1)._2))
            dut.clock.step(1)
            dut.io.mUop.valid.poke(false.B)
            while (!dut.io.lsuOut.valid.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.lsuOut.valid.expect(true.B)
            dut.io.xcpt.exception_vld.expect(false.B)
            dut.io.xcpt.update_vl.expect(true.B)
            dut.io.xcpt.xcpt_cause.ma.ld.expect(false.B)
            dut.io.xcpt.update_data.expect(2.U)
            dut.io.lsuOut.bits.data.expect(ldReqs(1)._3)
        }
        }
    }

    def vLsuTest11(): Unit = {
        it should "pass: strided load (uops=1, eew=8, vl=6, vstart=0, stride=-5)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                // 1000~1001(cdef), 1008~1009(ffff), 1010~1011(0f0f), 1018~1019(3210)
                // 1020~1021(3456), 1028~1029(0101), 1030~1031(4567), 1038~1039(1111)
                (vlse8.copy(vl=6, uopIdx=0, uopEnd=true), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "h201f1e1d1c1b1a19181720103478eeef".U),
                // (vlse8.copy(vl=10, uopIdx=1, uopEnd=true),  SrcBundleLd(scalar_opnd_2="h4"), "h201f1e1d1c1b1a191817161533332222".U),
            )
            
            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest12(): Unit = {
        it should "pass: strided load (uops=2, eew=64, vl=3, vstart=0, stride=-1)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut => 
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlse32.copy(vl=3, uopIdx=0, uopEnd=true), SrcBundleLd(scalar_opnd_2="hffffffff_ffffffff"), "h201f1e1deeeeeeeeeeeeeeee89abcdef".U),
            )
            
            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest13(): Unit = {
        it should "pass: strided load (uops=2, eew=16, vl=10, vstart=0, stride=4)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut => 
            dut.clock.step(1)
            val ldReqs = Seq(
                // 1000~1001(cdef), 1008~1009(ffff), 1010~1011(0f0f), 1018~1019(3210)
                // 1020~1021(3456), 1028~1029(0101), 1030~1031(4567), 1038~1039(1111)
                (vlse16.copy(vl=10, uopIdx=0, uopEnd=false), SrcBundleLd(scalar_opnd_2="h8"), "h111145670101345632100f0fffffcdef".U),
                (vlse16.copy(vl=10, uopIdx=1, uopEnd=true),  SrcBundleLd(scalar_opnd_2="h8"), "h201f1e1d1c1b1a191817161533332222".U),
            )

            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest14(): Unit = {
        it should "pass: strided load (uops=2, eew=16, vl=10, vstart=0, stride=4) with mask" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.step(1)
            val ldReqs = Seq(
                // 1000~1001(cdef), 1008~1009(ffff), 1010~1011(0f0f), 1018~1019(3210)
                // 1020~1021(3456), 1028~1029(0101), 1030~1031(4567), 1038~1039(1111)
                (vlse16.copy(vm=false, vl=10, uopIdx=0, uopEnd=false), SrcBundleLd(scalar_opnd_2="h8", mask="hffff_ffff_ffff_ffff_ffff_ffff_ffff_fefe"), "h111145670101345632100f0fffff1211".U),
                (vlse16.copy(vm=false, vl=10, uopIdx=1, uopEnd=true),  SrcBundleLd(scalar_opnd_2="h8", mask="hffff_ffff_ffff_ffff_ffff_ffff_ffff_fefe"), "h201f1e1d1c1b1a191817161533331211".U),
            )

            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                // dut.clock.step(100)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest15(): Unit = {
        it should "pass: unit-strde exception" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut => 
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle8.copy(vl=19, uopIdx=0, uopEnd=false, vstart=1), SrcBundleLd(scalar_opnd_1="h1058"), "h201f1e1d1c1b1a195555555555555511".U),
                (vle8.copy(vl=8, uopIdx=0, uopEnd=true), ldReqSrc_default, "h201f1e1d1c1b1a190123456789abcdef".U),
            )

            for ((c, s, r) <- ldReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genLdInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                // dut.clock.step(100)
                if(dut.io.xcpt.update_vl.peekBoolean()) {
                    dut.io.xcpt.update_vl.expect(true.B)
                } 
                dut.io.xcpt.update_data.expect(8.U)
                dut.io.lsuOut.bits.data.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }
}

class VLsuSpec_ld extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_ld {
  behavior of "LSU test"
    it should behave like vLsuTest0()   // unit-stride load
    it should behave like vLsuTest1()   // unit-stride load
    it should behave like vLsuTest2()   // unit-stride load
    it should behave like vLsuTest3()   // unit-stride load
    it should behave like vLsuTest4()   // unit-stride load
    it should behave like vLsuTest5()   // unit-stride load
    it should behave like vLsuTest6()   // unit-stride mask load
    it should behave like vLsuTest7()   // unit-strde fault-first-only
    it should behave like vLsuTest8()   // unit-strde fault-first-only
    it should behave like vLsuTest9()   // unit-strde fault-first-only
    it should behave like vLsuTest10()  // unit-strde fault-first-only
    it should behave like vLsuTest11()  // strided load
    it should behave like vLsuTest12()  // strided load
    it should behave like vLsuTest13()  // strided load
    it should behave like vLsuTest14()  // strided load with mask enabled
    it should behave like vLsuTest15()  // unit-stride exception
}
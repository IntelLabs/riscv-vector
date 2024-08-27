package smartVector.lsutest

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chiseltest.WriteVcdAnnotation
import smartVector._
import darecreek.ctrl.decode.VInstructions._
import darecreek._
import darecreek.lsu._

trait VLsuBehavior_st {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val stReqSrc_default = SrcBundleSt()
    val vse8    = CtrlBundle(VSE8_V)
    val vse16   = CtrlBundle(VSE16_V)
    val vse32   = CtrlBundle(VSE32_V)
    val vse64   = CtrlBundle(VSE64_V)
    val vsm     = CtrlBundle(VSM_V)
    val vs2r    = CtrlBundle(VS2R_V)
    val vsse8   = CtrlBundle(VSSE8_V)
    val vsse16  = CtrlBundle(VSSE16_V)
    val vsse32  = CtrlBundle(VSSE32_V)
    val vsse64  = CtrlBundle(VSSE64_V)

    
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
        it should "pass: unit-stride store (uops=1, eew=8, vl=8, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vse8.copy(vl=8, uopIdx=0, uopEnd=true, isLoad=false, destVRegStart = true, destVRegEnd = true), stReqSrc_default),
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)
                dut.io.mUop.bits.uop.ctrl_store.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                // dut.clock.step(100)
                dut.clock.step(1)
            }
            dut.io.memInfo(index1000).expect("h1817161514131211".U)
        }
        }
    }

    def vLsuTest1(): Unit = {
        it should "pass: unit-stride store (uops=2, eew=8, vl=19, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vse8.copy(vl=19, uopIdx=0, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), stReqSrc_default),
                (vse8.copy(vl=19, uopIdx=1, uopEnd=true,  isLoad=false, destVRegStart = true, destVRegEnd = true), stReqSrc_default),
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h1817161514131211".U)
            dut.io.memInfo(index1008).expect("h201f1e1d1c1b1a19".U)
            dut.io.memInfo(index1010).expect("hf0f0f0f0f131211".U)
        }
        }
    }

    def vLsuTest2(): Unit = {
        it should "pass: unit-stride store (uops=4, eew=16, vl=27, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vse16.copy(vl=27, uopIdx=0, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), stReqSrc_default),
                (vse16.copy(vl=27, uopIdx=1, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(vs3="hfedcba98765432100f0f0f0f0f0f0f0f")),
                (vse16.copy(vl=27, uopIdx=2, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(vs3="h01010101010101011234567890123456")),
                (vse16.copy(vl=27, uopIdx=3, uopEnd=true, isLoad=false, destVRegStart = true, destVRegEnd = true),  SrcBundleSt(vs3="h201f1e1d1c1b1a191817678901234567")),
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h1817161514131211".U)
            dut.io.memInfo(index1008).expect("h201f1e1d1c1b1a19".U)
            dut.io.memInfo(index1010).expect("h0f0f0f0f0f0f0f0f".U)
            dut.io.memInfo(index1018).expect("hfedcba9876543210".U)
            dut.io.memInfo(index1020).expect("h1234567890123456".U)
            dut.io.memInfo(index1028).expect("h0101010101010101".U)
            dut.io.memInfo(index1030).expect("h2345678901234567".U)
        }
        }
    }

    def vLsuTest3(): Unit = {
        it should "pass: unit-stride store (uops=3, eew=32, vl=10, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vse32.copy(vl=10, uopIdx=0, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), stReqSrc_default),
                (vse32.copy(vl=10, uopIdx=1, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(vs3="hfedcba98765432100f0f0f0f0f0f0f0f")),
                (vse32.copy(vl=10, uopIdx=2, uopEnd=true, isLoad=false, destVRegStart = true, destVRegEnd = true),  SrcBundleSt(vs3="h01010101010101011234567890123456")),
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h1817161514131211".U)
            dut.io.memInfo(index1008).expect("h201f1e1d1c1b1a19".U)
            dut.io.memInfo(index1010).expect("h0f0f0f0f0f0f0f0f".U)
            dut.io.memInfo(index1018).expect("hfedcba9876543210".U)
            dut.io.memInfo(index1020).expect("h1234567890123456".U)
        }
        }
    }

    def vLsuTest4(): Unit = {
        it should "pass: unit-stride store (uops=2, eew=64, vl=3, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vse64.copy(vl=3, uopIdx=0, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), stReqSrc_default),
                (vse64.copy(vl=3, uopIdx=1, uopEnd=true, isLoad=false, destVRegStart = true, destVRegEnd = true),  SrcBundleSt(vs3="hfedcba98765432100f0f0f0f0f0f0f0f")),
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h1817161514131211".U)
            dut.io.memInfo(index1008).expect("h201f1e1d1c1b1a19".U)
            dut.io.memInfo(index1010).expect("h0f0f0f0f0f0f0f0f".U)
        }
        }
    }

    def vLsuTest5(): Unit = {
        it should "pass: unit-stride store (uops=2, eew=64, vl=3, vstart=1)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vse64.copy(vl=3, vstart=1, uopIdx=0, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), stReqSrc_default),
                (vse64.copy(vl=3, vstart=1, uopIdx=1, uopEnd=true, isLoad=false, destVRegStart = true, destVRegEnd = true),  SrcBundleSt(vs3="hfedcba98765432100f0f0f0f0f0f0f0f")),
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h123456789abcdef".U)
            dut.io.memInfo(index1008).expect("h201f1e1d1c1b1a19".U)
            dut.io.memInfo(index1010).expect("h0f0f0f0f0f0f0f0f".U)
        }
        }
    }

    def vLsuTest6(): Unit = {
        it should "pass: unit-stride mask store (uops=1, eew=8, vl=8, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vsm.copy(vl=27, uopIdx=0, uopEnd=true, isLoad=false, destVRegStart = true, destVRegEnd = true), stReqSrc_default),
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h123456714131211".U)
        }
        }
    }

    def vLsuTest7(): Unit = {
        it should "pass: strided store (uops=1, eew=8, vl=6, vstart=0, stride=-5)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vsse8.copy(vl=6, uopIdx=0, uopEnd=true, isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(scalar_opnd_2="hffffffff_fffffffb"))
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h0123456789abcd11".U)
            dut.io.memInfo(index0ff8).expect("heeeeeeee12eeeeee".U)
            dut.io.memInfo(index0ff0).expect("h5613901234501489".U)
            dut.io.memInfo(index0fe8).expect("h1010101510101010".U)
        }
        }
    }

    def vLsuTest8(): Unit = {
        it should "pass: strided store (uops=2, eew=64, vl=2, vstart=0, stride=-1)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vsse32.copy(vl=2, uopIdx=0, uopEnd=true, isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(scalar_opnd_2="hffffffff_ffffffff"))
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.io.xcpt.update_vl.expect(false.B)
                dut.io.xcpt.update_data.expect(1.U)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h123456714131211".U)
            dut.io.memInfo(index0ff8).expect("heeeeeeeeeeeeeeee".U)     
        }
        }
    }

    def vLsuTest9(): Unit = {
        it should "pass: strided store (uops=2, eew=16, vl=10, vstart=0, stride=4)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vsse16.copy(vl=10, uopIdx=0, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(scalar_opnd_2="h8")),
                (vsse16.copy(vl=10, uopIdx=1, uopEnd=true, isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(scalar_opnd_2="h8"))
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h0123456789ab1211".U)
            dut.io.memInfo(index1008).expect("hffffffffffff1413".U)
            dut.io.memInfo(index1010).expect("h0f0f0f0f0f0f1615".U)   
            dut.io.memInfo(index1018).expect("hfedcba9876541817".U)   
            dut.io.memInfo(index1048).expect("h3333333333331413".U)
        }
        }
    }

    def vLsuTest10(): Unit = {
        it should "pass: strided store (uops=2, eew=16, vl=10, vstart=0, stride=4) with mask" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vsse16.copy(vm=false, vl=10, uopIdx=0, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(scalar_opnd_2="h8", mask="hffff_ffff_ffff_ffff_ffff_ffff_ffff_fefe")),
                (vsse16.copy(vm=false, vl=10, uopIdx=1, uopEnd=true, isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(scalar_opnd_2="h8", mask="hffff_ffff_ffff_ffff_ffff_ffff_ffff_fefe"))
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h0123456789abcdef".U)
            dut.io.memInfo(index1008).expect("hffffffffffff1413".U)
            dut.io.memInfo(index1010).expect("h0f0f0f0f0f0f1615".U)   
            dut.io.memInfo(index1018).expect("hfedcba9876541817".U)   
            dut.io.memInfo(index1040).expect("h2222222222222222".U)
            dut.io.memInfo(index1048).expect("h3333333333331413".U)
        }
        }
    }

    def vLsuTest11(): Unit = {
        it should "pass: unit-stride store exception" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vse8.copy(vl=19, uopIdx=0, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(scalar_opnd_1="h1058")),
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.io.xcpt.update_data.expect(8.U)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1058).expect("h1817161514131211".U)
            dut.io.memInfo(index1060).expect("h6666666666666666".U)
        }
        }
    }

    def vLsuTest12(): Unit = {
        it should "pass: unit-stride whole register store (uops=2, eew=8, vl=19)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vs2r.copy(vl=19, uopIdx=0, uopEnd=false, isLoad=false, destVRegStart = true, destVRegEnd = true), stReqSrc_default),
                (vs2r.copy(vl=19, uopIdx=1, uopEnd=true,  isLoad=false, destVRegStart = true, destVRegEnd = true), SrcBundleSt(vs3="hfedcba98765432100f0f0f0f0f0f0f0f")),
            )

            for ((c, s) <- stReqs) {
                while (!dut.io.lsuReady.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.mUop.valid.poke(true.B)
                dut.io.mUop.bits.poke(genStInput(c, s))
                dut.clock.step(1)
                dut.io.mUop.valid.poke(false.B)

                while (!dut.io.lsuOut.valid.peekBoolean()) {
                    dut.clock.step(1)
                }
                dut.io.lsuOut.valid.expect(true.B)
                dut.clock.step(1)
            }

            dut.io.memInfo(index1000).expect("h1817161514131211".U)
            dut.io.memInfo(index1008).expect("h201f1e1d1c1b1a19".U)
            dut.io.memInfo(index1010).expect("h0f0f0f0f0f0f0f0f".U)
            dut.io.memInfo(index1018).expect("hfedcba9876543210".U)
        }
        }
    }
}

class VLsuSpec_st extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_st {
  behavior of "LSU test"
    it should behave like vLsuTest0()   // unit-stride store
    it should behave like vLsuTest1()   // unit-stride store
    it should behave like vLsuTest2()   // unit-stride store
    it should behave like vLsuTest3()   // unit-stride store
    it should behave like vLsuTest4()   // unit-stride store
    it should behave like vLsuTest5()   // unit-stride store 
    it should behave like vLsuTest6()   // unit-stride mask store
    it should behave like vLsuTest7()   // strided store
    it should behave like vLsuTest8()   // strided store
    it should behave like vLsuTest9()   // strided store
    it should behave like vLsuTest10()  // strided store with mask enabled
    it should behave like vLsuTest11()  // unit-stride exception
    it should behave like vLsuTest12()
}
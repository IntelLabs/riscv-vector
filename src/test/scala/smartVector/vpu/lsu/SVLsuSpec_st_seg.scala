package smartVector.lsutest

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
import smartVector._
import darecreek.ctrl.decode.VInstructions._
import darecreek._
import darecreek.lsu._


trait VLsuBehavior_st_seg {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val stReqSrc_default = SrcBundleSt()
    val vsseg2e8    = CtrlBundle(BitPat("b001") ## VSE8_V(28, 0))
    val vsseg3e8    = CtrlBundle(BitPat("b010") ## VSE8_V(28, 0))
    val vsseg3e16   = CtrlBundle(BitPat("b010") ## VSE16_V(28, 0))
    val vsseg2e64   = CtrlBundle(BitPat("b001") ## VSE64_V(28, 0))

    val vssseg2e8   = CtrlBundle(BitPat("b001") ## VSSE8_V(28, 0))
    val vssseg3e8   = CtrlBundle(BitPat("b010") ## VSSE8_V(28, 0))
    val vssseg3e16  = CtrlBundle(BitPat("b010") ## VSSE16_V(28, 0))
    val vssseg2e64  = CtrlBundle(BitPat("b001") ## VSSE64_V(28, 0))
    
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
        it should "pass: unit-stride segment store (eew=8, vl=3, vstart=0, segments=2)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)

            val stReqs = Seq(
                (vsseg2e8.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg2e8.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg2e8.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg2e8.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg2e8.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg2e8.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=true , vsew=1, isLoad=false), stReqSrc_default),
            )

            for ((c, s) <- stReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genStInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                    dut.clock.step(1)
            }

            dut.clock.step(10)
            dut.io.memInfo(index1000).expect("h0123131312121111".U)
        }
        }
    }

    def vLsuTest1(): Unit = {
        it should "pass: unit-stride segment store (eew=16, vl=3, vstart=0, segments=3)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)

            val stReqs = Seq(
                (vsseg3e16.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg3e16.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg3e16.copy(vl=3, segIdx=2, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg3e16.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg3e16.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg3e16.copy(vl=3, segIdx=2, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg3e16.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg3e16.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg3e16.copy(vl=3, segIdx=2, uopIdx=2, uopEnd=true , vsew=1, isLoad=false), stReqSrc_default),
            )

            for ((c, s) <- stReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genStInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                    dut.clock.step(1)
            }

            dut.clock.step(10)
            dut.io.memInfo(index1000).expect("h1413121112111211".U)
            dut.io.memInfo(index1008).expect("h1615161514131413".U)
            dut.io.memInfo(index1010).expect("h0f0f0f0f0f0f1615".U)
        }
        }
    }

    def vLsuTest2(): Unit = {
        it should "pass: unit-stride segment store (eew=64, vl=3, vstart=0, segments=2)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vsseg2e64.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg2e64.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg2e64.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg2e64.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg2e64.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1, isLoad=false), stReqSrc_default),
                (vsseg2e64.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=true , vsew=1, isLoad=false), stReqSrc_default),
            )

            for ((c, s) <- stReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genStInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                    dut.clock.step(1)
            }
            dut.clock.step(10)
            dut.io.memInfo(index1000).expect("h1817161514131211".U)
            dut.io.memInfo(index1008).expect("h1817161514131211".U)
            dut.io.memInfo(index1010).expect("h201f1e1d1c1b1a19".U)
            dut.io.memInfo(index1018).expect("h201f1e1d1c1b1a19".U)
        }
        }
    }

    def vLsuTest3(): Unit = {
        it should "pass: strided segment store (uops=2, eew=8, vl=4, vstart=0, segments=2, stride=-5)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val stReqs = Seq(
                (vssseg2e8.copy(vl=4, segIdx=0, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="hffffffff_fffffffb")),
                (vssseg2e8.copy(vl=4, segIdx=1, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="hffffffff_fffffffb")),
                (vssseg2e8.copy(vl=4, segIdx=0, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="hffffffff_fffffffb")),
                (vssseg2e8.copy(vl=4, segIdx=1, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="hffffffff_fffffffb")),
                (vssseg2e8.copy(vl=4, segIdx=0, uopIdx=2, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="hffffffff_fffffffb")),
                (vssseg2e8.copy(vl=4, segIdx=1, uopIdx=2, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="hffffffff_fffffffb")),
                (vssseg2e8.copy(vl=4, segIdx=0, uopIdx=3, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="hffffffff_fffffffb")),
                (vssseg2e8.copy(vl=4, segIdx=1, uopIdx=3, uopEnd=true , vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="hffffffff_fffffffb")),
            )

            for ((c, s) <- stReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genStInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                    dut.clock.step(1)
            }

            dut.clock.step(10)
            dut.io.memInfo(index1000).expect("h0123456789ab1111".U)
            dut.io.memInfo(index0ff8).expect("heeeeee1212eeeeee".U)
            dut.io.memInfo(index0ff0).expect("h1313901234141489".U)
        }
        }
    }

    def vLsuTest4(): Unit = {
        it should "pass: unit-stride segment store (uops=3, eew=8, vl=2, vstart=0, segments=2, stride=6)" in {
        test(new SmartVectorLsuTestWrapper(false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)

            val stReqs = Seq(
                (vssseg3e16.copy(vl=2, segIdx=0, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="h06")),
                (vssseg3e16.copy(vl=2, segIdx=1, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="h06")),
                (vssseg3e16.copy(vl=2, segIdx=2, uopIdx=0, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="h06")),
                (vssseg3e16.copy(vl=2, segIdx=0, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="h06")),
                (vssseg3e16.copy(vl=2, segIdx=1, uopIdx=1, uopEnd=false, vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="h06")),
                (vssseg3e16.copy(vl=2, segIdx=2, uopIdx=1, uopEnd=true,  vsew=1, isLoad=false), SrcBundleSt(scalar_opnd_2="h06")),

            )

            for ((c, s) <- stReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genStInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                    dut.clock.step(1)
            }

            dut.clock.step(10)
            dut.io.memInfo(index1000).expect("h1413121112111211".U)
            dut.io.memInfo(index1008).expect("hffffffff14131413".U)
        }
        }
    }
}

class VLsuSpec_st_seg extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_st_seg {
  behavior of "LSU test"
    it should behave like vLsuTest0() 
    it should behave like vLsuTest1() 
    it should behave like vLsuTest2()  
    it should behave like vLsuTest3() 
    it should behave like vLsuTest4()   
}
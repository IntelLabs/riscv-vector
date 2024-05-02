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


trait VSegLsuBehavior_ld_seg_mutu {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldReqSrc_default = SrcBundleLd()
    val vlseg2e8    = CtrlBundle(BitPat("b001") ## VLE8_V(28, 0))
    val vlseg3e8    = CtrlBundle(BitPat("b010") ## VLE8_V(28, 0))
    val vlseg3e16   = CtrlBundle(BitPat("b010") ## VLE16_V(28, 0))
    val vlseg2e64   = CtrlBundle(BitPat("b001") ## VLE64_V(28, 0))

    val vlsseg2e8   = CtrlBundle(BitPat("b001") ## VLSE8_V(28, 0))
    val vlsseg3e8   = CtrlBundle(BitPat("b010") ## VLSE8_V(28, 0))
    val vlsseg3e16  = CtrlBundle(BitPat("b010") ## VLSE16_V(28, 0))
    val vlsseg2e64  = CtrlBundle(BitPat("b001") ## VLSE64_V(28, 0))


    

    def VSegLsuTest0(): Unit = {
        it should "pass: unit-stride segment load (uops=2, eew=8, vl=16, vstart=0, segments=2)" in {
        test(new SmartVectorSegLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg2e8.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hef".U, "h1".U),
                (vlseg2e8.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hcd".U, "h1".U),
                (vlseg2e8.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "hab00".U, "h2".U),
                (vlseg2e8.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h8900".U, "h2".U),
                (vlseg2e8.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "h670000".U, "h4".U),
                (vlseg2e8.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=true , vsew=1), ldReqSrc_default, "h450000".U, "h4".U),
            )

            fork {
                for ((c, s, r, m) <- ldReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genLdInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                }
            }.fork {
                for ((c, s, r, m) <- ldReqs) {
                    
                    while (!dut.io.lsuOut.valid.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.lsuOut.valid.expect(true.B)
                    dut.io.lsuOut.bits.data.expect(r)
                    dut.io.lsuOut.bits.rfWriteMask.expect(m)
                    dut.clock.step(1)
                }
            }.join()

            // for ((c, s, r, m) <- ldReqs) {
            //     while (!dut.io.lsuReady.peekBoolean()) {
            //         dut.clock.step(1)
            //     }
            //     dut.io.mUop.valid.poke(true.B)
            //     dut.io.mUop.bits.poke(genLdInput(c, s))
            //     dut.clock.step(1)
            //     dut.io.mUop.valid.poke(false.B)

            //     while (!dut.io.lsuOut.valid.peekBoolean()) {
            //         dut.clock.step(1)
            //     }
            //     dut.io.lsuOut.valid.expect(true.B)
            //     dut.io.lsuOut.bits.data.expect(r)
            //     dut.io.lsuOut.bits.rfWriteMask.expect(m)
            //     dut.clock.step(4)
            // }
        }
        }
    }

    def VSegLsuTest1(): Unit = {
        it should "pass: unit-stride segment load (uops=3, eew=8, vl=16, vstart=0, segments=3)" in {
        test(new SmartVectorSegLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg3e8.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hef".U, "h1".U),
                (vlseg3e8.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hcd".U, "h1".U),
                (vlseg3e8.copy(vl=3, segIdx=2, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hab".U, "h1".U),
                (vlseg3e8.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h8900".U, "h2".U),
                (vlseg3e8.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h6700".U, "h2".U),
                (vlseg3e8.copy(vl=3, segIdx=2, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h4500".U, "h2".U),
                (vlseg3e8.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "h230000".U, "h4".U),
                (vlseg3e8.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "h010000".U, "h4".U),
                (vlseg3e8.copy(vl=3, segIdx=2, uopIdx=2, uopEnd=true , vsew=1), ldReqSrc_default, "hff0000".U, "h4".U),
            )

            fork {
                for ((c, s, r, m) <- ldReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genLdInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                }
            }.fork {
                for ((c, s, r, m) <- ldReqs) {
                    
                    while (!dut.io.lsuOut.valid.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.lsuOut.valid.expect(true.B)
                    dut.io.lsuOut.bits.data.expect(r)
                    dut.io.lsuOut.bits.rfWriteMask.expect(m)
                    dut.clock.step(1)
                }
            }.join()
        }
        }
    }

    def VSegLsuTest2(): Unit = {
        it should "pass: unit-stride segment load (uops=2, eew=16, vl=3, vstart=0, segments=3)" in {
        test(new SmartVectorSegLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg3e16.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hcdef".U, "h3".U),
                (vlseg3e16.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "h89ab".U, "h3".U),
                (vlseg3e16.copy(vl=3, segIdx=2, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "h4567".U, "h3".U),
                (vlseg3e16.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h01230000".U, "hc".U),
                (vlseg3e16.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "hffff0000".U, "hc".U),
                (vlseg3e16.copy(vl=3, segIdx=2, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "hffff0000".U, "hc".U),
                (vlseg3e16.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "hffff00000000".U, "h30".U),
                (vlseg3e16.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "hffff00000000".U, "h30".U),
                (vlseg3e16.copy(vl=3, segIdx=2, uopIdx=2, uopEnd=true , vsew=1), ldReqSrc_default, "h0f0f00000000".U, "h30".U),
            )

            fork {
                for ((c, s, r, m) <- ldReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genLdInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                }
            }.fork {
                for ((c, s, r, m) <- ldReqs) {
                    
                    while (!dut.io.lsuOut.valid.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.lsuOut.valid.expect(true.B)
                    dut.io.lsuOut.bits.data.expect(r)
                    dut.io.lsuOut.bits.rfWriteMask.expect(m)
                    dut.clock.step(1)
                }
            }.join()
        }
        }
    }

    def VSegLsuTest3(): Unit = {
        it should "pass: unit-stride segment load (uops=4, eew=64, vl=3, vstart=0, segments=2)" in {
        test(new SmartVectorSegLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg2e64.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "h0123456789abcdef".U, "hff".U),
                (vlseg2e64.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hffffffffffffffff".U, "hff".U),
                (vlseg2e64.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h0f0f0f0f0f0f0f0f0000000000000000".U, "hff00".U),
                (vlseg2e64.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "hfedcba98765432100000000000000000".U, "hff00".U),
                (vlseg2e64.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "h1234567890123456".U, "hff".U),
                (vlseg2e64.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "h0101010101010101".U, "hff".U),
            )

            fork {
                for ((c, s, r, m) <- ldReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genLdInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                }
            }.fork {
                for ((c, s, r, m) <- ldReqs) {
                    
                    while (!dut.io.lsuOut.valid.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.lsuOut.valid.expect(true.B)
                    dut.io.lsuOut.bits.data.expect(r)
                    dut.io.lsuOut.bits.rfWriteMask.expect(m)
                    dut.clock.step(1)
                }
            }.join()
        }
        }
    }

    def VSegLsuTest4(): Unit = {
        it should "pass: strided segment load (uops=4, eew=64, vl=4, vstart=0, segments=2, stride=-5)" in {
        test(new SmartVectorSegLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlsseg2e8.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "hef".U, "h1".U),
                (vlsseg2e8.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "hcd".U, "h1".U),
                (vlsseg2e8.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "hee00".U, "h2".U),
                (vlsseg2e8.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "hee00".U, "h2".U),
                (vlsseg2e8.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "h780000".U, "h4".U),
                (vlsseg2e8.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=true , vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "h560000".U, "h4".U),
            )

            fork {
                for ((c, s, r, m) <- ldReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genLdInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                }
            }.fork {
                for ((c, s, r, m) <- ldReqs) {
                    
                    while (!dut.io.lsuOut.valid.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.lsuOut.valid.expect(true.B)
                    dut.io.lsuOut.bits.data.expect(r)
                    dut.io.lsuOut.bits.rfWriteMask.expect(m)
                    dut.clock.step(1)
                }
            }.join()
        }
        }
    }

    def VSegLsuTest5(): Unit = {
        it should "pass: strided segment load (uops=3, eew=16, vl=2, vstart=0, segments=3, stride=4)" in {
        test(new SmartVectorSegLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlsseg3e16.copy(vl=2, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "hcdef".U, "h3".U),
                (vlsseg3e16.copy(vl=2, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h89ab".U, "h3".U),
                (vlsseg3e16.copy(vl=2, segIdx=2, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h4567".U, "h3".U),
                (vlsseg3e16.copy(vl=2, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h45670000".U, "hc".U),
                (vlsseg3e16.copy(vl=2, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h01230000".U, "hc".U),
                (vlsseg3e16.copy(vl=2, segIdx=2, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "hffff0000".U, "hc".U),
            )

            fork {
                for ((c, s, r, m) <- ldReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genLdInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                }
            }.fork {
                for ((c, s, r, m) <- ldReqs) {
                    
                    while (!dut.io.lsuOut.valid.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.lsuOut.valid.expect(true.B)
                    dut.io.lsuOut.bits.data.expect(r)
                    dut.io.lsuOut.bits.rfWriteMask.expect(m)
                    dut.clock.step(1)
                }
            }.join()
        }
        }
    }

    def VSegLsuTest5(): Unit = {
        it should "pass: strided segment load (uops=3, eew=16, vl=2, vstart=0, segments=3, stride=4)" in {
        test(new SmartVectorSegLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlsseg3e16.copy(vl=2, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "hcdef".U, "h3".U),
                (vlsseg3e16.copy(vl=2, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h89ab".U, "h3".U),
                (vlsseg3e16.copy(vl=2, segIdx=2, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h4567".U, "h3".U),
                (vlsseg3e16.copy(vl=2, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h45670000".U, "hc".U),
                (vlsseg3e16.copy(vl=2, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h01230000".U, "hc".U),
                (vlsseg3e16.copy(vl=2, segIdx=2, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "hffff0000".U, "hc".U),
            )

            fork {
                for ((c, s, r, m) <- ldReqs) {
                    while (!dut.io.lsuReady.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.mUop.valid.poke(true.B)
                    dut.io.mUop.bits.poke(genLdInput(c, s))
                    dut.clock.step(1)
                    dut.io.mUop.valid.poke(false.B)
                }
            }.fork {
                for ((c, s, r, m) <- ldReqs) {
                    
                    while (!dut.io.lsuOut.valid.peekBoolean()) {
                        dut.clock.step(1)
                    }
                    dut.io.lsuOut.valid.expect(true.B)
                    dut.io.lsuOut.bits.data.expect(r)
                    dut.io.lsuOut.bits.rfWriteMask.expect(m)
                    dut.clock.step(1)
                }
            }.join()
        }
        }
    }
}

class VSegLsuSpec_ld_seg_mutu extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VSegLsuBehavior_ld_seg_mutu {
  behavior of "LSU test"
    it should behave like VSegLsuTest0() 
    it should behave like VSegLsuTest1() 
    it should behave like VSegLsuTest2() 
    it should behave like VSegLsuTest3() 
    it should behave like VSegLsuTest4()
    it should behave like VSegLsuTest5() 
}
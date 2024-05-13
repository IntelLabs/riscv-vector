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


trait VLsuBehavior_ld_seg {
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
    
    def vLsuTest0(): Unit = {
        it should "pass: unit-stride segment load (uops=2, eew=8, vl=16, vstart=0, segments=2)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(100)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg2e8.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hef".U, "hfffe".U),
                (vlseg2e8.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hcd".U, "hfffe".U),
                (vlseg2e8.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "hab00".U, "hfffd".U),
                (vlseg2e8.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h8900".U, "hfffd".U),
                (vlseg2e8.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "h670000".U, "hfffb".U),
                (vlseg2e8.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=true , vsew=1), ldReqSrc_default, "h450000".U, "hfffb".U),
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

    def vLsuTest1(): Unit = {
        it should "pass: unit-stride segment load (uops=3, eew=8, vl=16, vstart=0, segments=3)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg3e8.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hef".U, "hfffe".U),
                (vlseg3e8.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hcd".U, "hfffe".U),
                (vlseg3e8.copy(vl=3, segIdx=2, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hab".U, "hfffe".U),
                (vlseg3e8.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h8900".U, "hfffd".U),
                (vlseg3e8.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h6700".U, "hfffd".U),
                (vlseg3e8.copy(vl=3, segIdx=2, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h4500".U, "hfffd".U),
                (vlseg3e8.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "h230000".U, "hfffb".U),
                (vlseg3e8.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "h010000".U, "hfffb".U),
                (vlseg3e8.copy(vl=3, segIdx=2, uopIdx=2, uopEnd=true , vsew=1), ldReqSrc_default, "hff0000".U, "hfffb".U),
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

    def vLsuTest2(): Unit = {
        it should "pass: unit-stride segment load (uops=2, eew=16, vl=3, vstart=0, segments=3)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg3e16.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hcdef".U, "hfffc".U),
                (vlseg3e16.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "h89ab".U, "hfffc".U),
                (vlseg3e16.copy(vl=3, segIdx=2, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "h4567".U, "hfffc".U),
                (vlseg3e16.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h01230000".U, "hfff3".U),
                (vlseg3e16.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "hffff0000".U, "hfff3".U),
                (vlseg3e16.copy(vl=3, segIdx=2, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "hffff0000".U, "hfff3".U),
                (vlseg3e16.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "hffff00000000".U, "hffcf".U),
                (vlseg3e16.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "hffff00000000".U, "hffcf".U),
                (vlseg3e16.copy(vl=3, segIdx=2, uopIdx=2, uopEnd=true , vsew=1), ldReqSrc_default, "h0f0f00000000".U, "hffcf".U),
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

    def vLsuTest3(): Unit = {
        it should "pass: unit-stride segment load (uops=4, eew=64, vl=3, vstart=0, segments=2)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg2e64.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "h0123456789abcdef".U, "hff00".U),
                (vlseg2e64.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), ldReqSrc_default, "hffffffffffffffff".U, "hff00".U),
                (vlseg2e64.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "h0f0f0f0f0f0f0f0f0000000000000000".U, "h00ff".U),
                (vlseg2e64.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), ldReqSrc_default, "hfedcba98765432100000000000000000".U, "h00ff".U),
                (vlseg2e64.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "h1234567890123456".U, "hff00".U),
                (vlseg2e64.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=false, vsew=1), ldReqSrc_default, "h0101010101010101".U, "hff00".U),
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

    def vLsuTest4(): Unit = {
        it should "pass: strided segment load (uops=4, eew=64, vl=4, vstart=0, segments=2, stride=-5)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlsseg2e8.copy(vl=3, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "hef".U, "hfffe".U),
                (vlsseg2e8.copy(vl=3, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "hcd".U, "hfffe".U),
                (vlsseg2e8.copy(vl=3, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "hee00".U, "hfffd".U),
                (vlsseg2e8.copy(vl=3, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "hee00".U, "hfffd".U),
                (vlsseg2e8.copy(vl=3, segIdx=0, uopIdx=2, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "h780000".U, "hfffb".U),
                (vlsseg2e8.copy(vl=3, segIdx=1, uopIdx=2, uopEnd=true , vsew=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "h560000".U, "hfffb".U),
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

    def vLsuTest5(): Unit = {
        it should "pass: strided segment load (uops=3, eew=16, vl=2, vstart=0, segments=3, stride=4)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(1000)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlsseg3e16.copy(vl=2, segIdx=0, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "hcdef".U, "hfffc".U),
                (vlsseg3e16.copy(vl=2, segIdx=1, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h89ab".U, "hfffc".U),
                (vlsseg3e16.copy(vl=2, segIdx=2, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h4567".U, "hfffc".U),
                (vlsseg3e16.copy(vl=2, segIdx=0, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h45670000".U, "hfff3".U),
                (vlsseg3e16.copy(vl=2, segIdx=1, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "h01230000".U, "hfff3".U),
                (vlsseg3e16.copy(vl=2, segIdx=2, uopIdx=1, uopEnd=false, vsew=1), SrcBundleLd(scalar_opnd_2="h04"), "hffff0000".U, "hfff3".U),
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

class VLsuSpec_ld_seg extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_ld_seg {
  behavior of "LSU test"
    it should behave like vLsuTest0() 
    it should behave like vLsuTest1() 
    it should behave like vLsuTest2() 
    it should behave like vLsuTest3() 
    it should behave like vLsuTest4()
    it should behave like vLsuTest5() 
}
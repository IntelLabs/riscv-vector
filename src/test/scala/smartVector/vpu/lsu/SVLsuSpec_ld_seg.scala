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
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg2e8.copy(vl=16, uopIdx=0, uopEnd=false, vsew=1, segIdx=0), ldReqSrc_default, "hdc9854100f0f0f0fffffffff2367abef".U),
                (vlseg2e8.copy(vl=16, uopIdx=0, uopEnd=true , vsew=1, segIdx=1), ldReqSrc_default, "hfeba76320f0f0f0fffffffff014589cd".U),
            )

            next_is_load_and_step(dut)

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

    def vLsuTest1(): Unit = {
        it should "pass: unit-stride segment load (uops=3, eew=8, vl=16, vstart=0, segments=3)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg3e8.copy(vl=16, uopIdx=0, uopEnd=false, vsew=1, segIdx=0), ldReqSrc_default, "h0101127834dc76100f0fffffff2389ef".U),
                (vlseg3e8.copy(vl=16, uopIdx=0, uopEnd=false, vsew=1, segIdx=1), ldReqSrc_default, "h0101015612fe98320f0f0fffff0167cd".U),
                (vlseg3e8.copy(vl=16, uopIdx=0, uopEnd=true,  vsew=1, segIdx=2), ldReqSrc_default, "h010101349056ba540f0f0fffffff45ab".U),
            )

            next_is_load_and_step(dut)

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

    def vLsuTest2(): Unit = {
        it should "pass: unit-stride segment load (uops=2, eew=16, vl=3, vstart=0, segments=3)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg3e16.copy(vl=3, uopIdx=0, uopEnd=false, vsew=1, segIdx=0), ldReqSrc_default, "h201f1e1d1c1b1a191817ffff0123cdef".U),
                (vlseg3e16.copy(vl=3, uopIdx=0, uopEnd=false, vsew=1, segIdx=1), ldReqSrc_default, "h201f1e1d1c1b1a191817ffffffff89ab".U),
                (vlseg3e16.copy(vl=3, uopIdx=0, uopEnd=true,  vsew=1, segIdx=2), ldReqSrc_default, "h201f1e1d1c1b1a1918170f0fffff4567".U),
            )

            next_is_load_and_step(dut)

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

    def vLsuTest3(): Unit = {
        it should "pass: unit-stride segment load (uops=4, eew=64, vl=3, vstart=0, segments=2)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlseg2e64.copy(vl=3, uopIdx=0, uopEnd=false, vsew=1, segIdx=0), ldReqSrc_default, "h0f0f0f0f0f0f0f0f0123456789abcdef".U),
                (vlseg2e64.copy(vl=3, uopIdx=1, uopEnd=false, vsew=1, segIdx=0), ldReqSrc_default, "h201f1e1d1c1b1a191234567890123456".U),
                (vlseg2e64.copy(vl=3, uopIdx=0, uopEnd=false, vsew=1, segIdx=1), ldReqSrc_default, "hfedcba9876543210ffffffffffffffff".U),
                (vlseg2e64.copy(vl=3, uopIdx=1, uopEnd=true,  vsew=1, segIdx=1), ldReqSrc_default, "h201f1e1d1c1b1a190101010101010101".U),
            )

            next_is_load_and_step(dut)

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
        it should "pass: strided segment load (uops=4, eew=64, vl=4, vstart=0, segments=2, stride=-5)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlsseg2e8.copy(vl=4, uopIdx=0, uopEnd=false, vsew=1, segIdx=0), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "h201f1e1d1c1b1a19181716153478eeef".U),
                (vlsseg2e8.copy(vl=4, uopIdx=0, uopEnd=true,  vsew=1, segIdx=1), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "h201f1e1d1c1b1a19181716155056eecd".U),
            )

            next_is_load_and_step(dut)

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
        it should "pass: strided segment load (uops=4, eew=64, vl=4, vstart=0, segments=3, stride=-1)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlsseg3e8.copy(vl=16, uopIdx=0, uopEnd=false, vsew=1, segIdx=0), SrcBundleLd(scalar_opnd_2="hffffffff_ffffffff"), "h34503412907856eeeeeeeeeeeeeeeeef".U),
                (vlsseg3e8.copy(vl=16, uopIdx=0, uopEnd=false, vsew=1, segIdx=1), SrcBundleLd(scalar_opnd_2="hffffffff_ffffffff"), "h503412907856eeeeeeeeeeeeeeeeefcd".U),
                (vlsseg3e8.copy(vl=16, uopIdx=0, uopEnd=true,  vsew=1, segIdx=2), SrcBundleLd(scalar_opnd_2="hffffffff_ffffffff"), "h3412907856eeeeeeeeeeeeeeeeefcdab".U),
            )

            next_is_load_and_step(dut)

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
        it should "pass: strided segment load (uops=3, eew=16, vl=2, vstart=0, segments=3, stride=4)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlsseg3e16.copy(vl=2, uopIdx=0, uopEnd=false, vsew=1, segIdx=0), SrcBundleLd(scalar_opnd_2="h04"), "h201f1e1d1c1b1a19181716154567cdef".U),
                (vlsseg3e16.copy(vl=2, uopIdx=0, uopEnd=false, vsew=1, segIdx=1), SrcBundleLd(scalar_opnd_2="h04"), "h201f1e1d1c1b1a1918171615012389ab".U),
                (vlsseg3e16.copy(vl=2, uopIdx=0, uopEnd=true,  vsew=1, segIdx=2), SrcBundleLd(scalar_opnd_2="h04"), "h201f1e1d1c1b1a1918171615ffff4567".U),
            )

            next_is_load_and_step(dut)

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
}

class VLsuSpec_ld_seg extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_ld_seg {
  behavior of "LSU test"
    it should behave like vLsuTest0() 
    it should behave like vLsuTest1() 
    it should behave like vLsuTest2()  
    it should behave like vLsuTest3() 
    it should behave like vLsuTest4()   
    it should behave like vLsuTest5()  
    it should behave like vLsuTest6() 
}
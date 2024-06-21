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


trait VLsuBehavior_ld_idx {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldReqSrc_default = SrcBundleLd()
    val vluxei8 = CtrlBundle(VLUXEI8_V)
    val vluxei16 = CtrlBundle(VLUXEI16_V)
    val vluxei32 = CtrlBundle(VLUXEI32_V)
    val vluxei64 = CtrlBundle(VLUXEI64_V)

    def vLsuTest0(): Unit = {
        it should "pass: indexed load (uops=2, eew=8, sew=16, vl=16, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vluxei8.copy(vl=16, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(vs2="h010004"), "h201f1e1d1c1b1a1918171615cdef4567".U, "hfff0".U),
                // (vluxei8.copy(vl=16, uopIdx=1, uopEnd=true,  vsew=1), SrcBundleLd(vs2="h010004"), "hcdefcdefcdefcdefcdefcdefcdefcdef".U),
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
                    // dut.clock.step(100)
                    dut.io.xcpt.update_vl.expect(false.B)
                    dut.io.xcpt.update_data.expect(2.U)
                    dut.io.lsuOut.bits.data.expect(r)
                    dut.io.lsuOut.bits.rfWriteMask.expect(m)
                    dut.clock.step(1)
                }
            }.join()
        }
        }
    }


    def vLsuTest1(): Unit = {
        it should "pass: indexed load (uops=2, eew=8, sew=64, vl=3, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vluxei8.copy(vl=3, uopIdx=0, uopEnd=false, vsew=3), SrcBundleLd(vs2="h01000"), "h0f0f0f0f0f0f0f0f0123456789abcdef".U, "h0000".U),
                (vluxei8.copy(vl=3, uopIdx=1, uopEnd=true, vsew=3),  SrcBundleLd(vs2="h01000"), "h201f1e1d1c1b1a190123456789abcdef".U, "hff00".U),
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
        it should "pass: indexed load (uops=3, eew=16, sew=64, vl=5, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vluxei16.copy(vl=5, uopIdx=0, uopEnd=false, vsew=3), SrcBundleLd(vs2="h000000180000001000080000"), "hffffffffffffffff0123456789abcdef".U, "h0000".U),
                (vluxei16.copy(vl=5, uopIdx=1, uopEnd=false, vsew=3), SrcBundleLd(vs2="h000000180000001000080000"), "h0123456789abcdef0f0f0f0f0f0f0f0f".U, "h0000".U),
                (vluxei16.copy(vl=5, uopIdx=2, uopEnd=true,  vsew=3), SrcBundleLd(vs2="h000000180000001000080000"), "h201f1e1d1c1b1a19fedcba9876543210".U, "hff00".U),
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
        it should "pass: indexed load (uops=2, eew=32, sew=16, vl=5, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vluxei32.copy(vl=5, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(vs2="h000000100000000000000008"), "h0".U, "hffff".U),
                (vluxei32.copy(vl=5, uopIdx=1, uopEnd=true, vsew=1),  SrcBundleLd(vs2="h08"), "h201f1e1d1c1bffffcdef0f0fcdefffff".U, "hfc00".U),
            )

            while (!dut.io.lsuReady.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.mUop.valid.poke(true.B)
            dut.io.mUop.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.mUop.valid.poke(false.B)

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
            dut.io.lsuOut.bits.data.expect(ldReqs(1)._3)
            dut.io.lsuOut.bits.rfWriteMask.expect(ldReqs(1)._4)
        }
        }
    }

    def vLsuTest4(): Unit = {
        it should "pass: indexed load (uops=2, eew=64, sew=16, vl=3, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vluxei64.copy(vl=3, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(vs2="h00010"), "h0".U, "hffff".U),
                (vluxei64.copy(vl=3, uopIdx=1, uopEnd=true,  vsew=1), SrcBundleLd(vs2="h00020"), "h201f1e1d1c1b1a1918173456cdef0f0f".U, "hffc0".U),
            )

            while (!dut.io.lsuReady.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.mUop.valid.poke(true.B)
            dut.io.mUop.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.mUop.valid.poke(false.B)

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
            dut.io.lsuOut.bits.data.expect(ldReqs(1)._3)
            dut.io.lsuOut.bits.rfWriteMask.expect(ldReqs(1)._4)
        }
        }
    }

    def vLsuTest5(): Unit = {
        it should "pass: indexed load (uops=2, eew=64, sew=16, vl=3, vstart=0) neg index value" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vluxei64.copy(vl=3, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(vs2="hffff_ffff_ffff_fff0_ffff_ffff_ffff_fff8"), "h0".U, "hffff".U),
                (vluxei64.copy(vl=3, uopIdx=1, uopEnd=true,  vsew=1), SrcBundleLd(vs2="h00020"), "h201f1e1d1c1b1a19181734563489eeee".U, "hffc0".U),
            )

            while (!dut.io.lsuReady.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.mUop.valid.poke(true.B)
            dut.io.mUop.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.mUop.valid.poke(false.B)

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
            dut.io.lsuOut.bits.data.expect(ldReqs(1)._3)
            dut.io.lsuOut.bits.rfWriteMask.expect(ldReqs(1)._4)
        }
        }
    }

    def vLsuTest6(): Unit = {
        it should "pass: indexed load (uops=2, eew=32, sew=16, vl=5, vstart=0) exception" in {
        test(new SmartVectorLsuTestWrapper(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.clock.setTimeout(200)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vluxei32.copy(vl=5, uopIdx=0, uopEnd=false, vsew=1), SrcBundleLd(vs2="h000000100000000000000008"), "h0".U, "hffff".U),
                (vluxei32.copy(vl=5, uopIdx=1, uopEnd=true, vsew=1),  SrcBundleLd(vs2="h60"), "h201f1e1d1c1b1a19cdef0f0fcdefffff".U, "hff00".U),
            )

            while (!dut.io.lsuReady.peekBoolean()) {
                dut.clock.step(1)
            }
            dut.io.mUop.valid.poke(true.B)
            dut.io.mUop.bits.poke(genLdInput(ldReqs(0)._1, ldReqs(0)._2))
            dut.clock.step(1)
            dut.io.mUop.valid.poke(false.B)

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
            dut.io.xcpt.update_vl.expect(false.B)
            dut.io.xcpt.update_data.expect(4.U)

            dut.io.lsuOut.bits.data.expect(ldReqs(1)._3)
            dut.io.lsuOut.bits.rfWriteMask.expect(ldReqs(1)._4)
        }
        }
    }
}

class VLsuSpec_ld_idx extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_ld_idx {
  behavior of "LSU test"
    it should behave like vLsuTest0() 
    it should behave like vLsuTest1() 
    it should behave like vLsuTest2()  
    it should behave like vLsuTest3() 
    it should behave like vLsuTest4()   
    it should behave like vLsuTest5()  
    it should behave like vLsuTest6() 
}
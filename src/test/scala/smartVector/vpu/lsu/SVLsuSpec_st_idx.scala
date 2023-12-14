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

trait VLsuBehavior_st_idx {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val stReqSrc_default = SrcBundleSt()
    val vsuxei8  = CtrlBundle(VSUXEI8_V)
    val vsuxei16 = CtrlBundle(VSUXEI16_V)
    val vsuxei32 = CtrlBundle(VSUXEI32_V)
    val vsuxei64 = CtrlBundle(VSUXEI64_V)
    
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
        it should "pass: indexed store (uops=2, eew=8, sew=16, vl=16, vstart=0)" in {
        test(new SmartVectorLsuStoreTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init_store(dut)
            dut.clock.step(1)
            val stReqs = Seq(
                (vsuxei8.copy(vl=16, uopIdx=0, uopEnd=false, vsew=1), SrcBundleSt(vs2="h010004")),
                (vsuxei8.copy(vl=16, uopIdx=1, uopEnd=true,  vsew=1), SrcBundleSt(vs2="h010004")),
            )

            next_is_store_and_step(dut)

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
            dut.io.memInfo(index1000).expect("h12110000201f".U)
        }
        }
    }

    def vLsuTest1(): Unit = {
        it should "pass: indexed store (uops=2, eew=8, sew=64, vl=3, vstart=0)" in {
        test(new SmartVectorLsuStoreTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init_store(dut)
            dut.clock.step(1)
            val stReqs = Seq(
                (vsuxei8.copy(vl=3, uopIdx=0, uopEnd=false, vsew=3), SrcBundleSt(vs2="h01000")),
                (vsuxei8.copy(vl=3, uopIdx=0, uopEnd=false, vsew=3), SrcBundleSt(vs2="h01000")),
            )

            next_is_store_and_step(dut)

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
            dut.io.memInfo(index1010).expect("h201f1e1d1c1b1a19".U)
        }
        }
    }

    def vLsuTest2(): Unit = {
        it should "pass: indexed store (uops=2, eew=8, sew=64, vl=3, vstart=0)" in {
        test(new SmartVectorLsuStoreTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init_store(dut)
            dut.clock.step(1)
            val stReqs = Seq(
                (vsuxei16.copy(vl=5, uopIdx=0, uopEnd=false, vsew=3), SrcBundleSt(vs2="h000000180000001000080000")),
                (vsuxei16.copy(vl=5, uopIdx=1, uopEnd=false, vsew=3), SrcBundleSt(vs2="h000000180000001000080000")),
                (vsuxei16.copy(vl=5, uopIdx=2, uopEnd=true,  vsew=3), SrcBundleSt(vs2="h000000180000001000080000")),
            )

            next_is_store_and_step(dut)

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
            dut.io.memInfo(index1000).expect("h201f1e1d1c1b1a19".U)
            dut.io.memInfo(index1008).expect("h201f1e1d1c1b1a19".U)
            dut.io.memInfo(index1010).expect("h1817161514131211".U)
            dut.io.memInfo(index1018).expect("h1817161514131211".U)
        }
        }
    }

    def vLsuTest3(): Unit = {
        it should "pass: indexed store (uops=2, eew=8, sew=64, vl=3, vstart=0)" in {
        test(new SmartVectorLsuStoreTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init_store(dut)
            dut.clock.step(1)
            val stReqs = Seq(
                (vsuxei32.copy(vl=5, uopIdx=0, uopEnd=false, vsew=3), SrcBundleSt(vs2="h00000010_00000000_00000008")),
                (vsuxei32.copy(vl=5, uopIdx=1, uopEnd=true,  vsew=3), SrcBundleSt(vs2="h18")),
            )

            next_is_store_and_step(dut)

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
            dut.io.memInfo(index1000).expect("h201f1e1d1c1b1a19".U)
            dut.io.memInfo(index1008).expect("h1817161514131211".U)
            dut.io.memInfo(index1010).expect("h1817161514131211".U)
            dut.io.memInfo(index1018).expect("h1817161514131211".U)
        }
        }
    }
}

class VLsuSpec_st_idx extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_st_idx {
  behavior of "LSU test"
    // it should behave like vLsuTest0()   // unit-stride store
    // it should behave like vLsuTest1()   // unit-stride store
    // it should behave like vLsuTest2()   // unit-stride store
    it should behave like vLsuTest3()   // unit-stride store
    // it should behave like vLsuTest4()   // unit-stride store
    // it should behave like vLsuTest5()   // unit-stride store 
    // it should behave like vLsuTest6()   // unit-stride mask store
    // it should behave like vLsuTest7()   // strided store
    // it should behave like vLsuTest8()   // strided store
    // it should behave like vLsuTest9()   // strided store
    // it should behave like vLsuTest10()  // strided store with mask enabled
    // it should behave like vLsuTest11()  // unit-stride exception
}
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

class VUopTest extends Bundle {
    val ctrl_funct6 = UInt(6.W)
    val ctrl_funct3 = UInt(3.W)
    val ctrl_load   = Bool()
    val ctrl_store  = Bool()
    val ctrl_vm     = Bool()
    val info_ma     = Bool() // vector mask agnostic, data unknown or undisturbed
    val info_ta     = Bool() // vector tail agnostic, data unknown or undisturbed
    val info_vsew   = UInt(3.W)
    val info_vlmul  = UInt(3.W)
    val info_vl     = UInt(bVL.W)
    val info_vstart = UInt(bVstart.W)
    val splitUopIdx = UInt(3.W)
    val splitUopEnd = Bool()
}

class MuopTest extends Bundle {
    val uop           = new VUopTest
    val oldVd         = Input(UInt(VLEN.W))
    val scalar_opnd_1 = UInt(64.W)
    val scalar_opnd_2 = UInt(64.W)
    val vs1           = UInt(128.W)
    val vs2           = UInt(128.W)
}

class FakeDCache extends Module {
    val io = IO(Flipped(new RVUMemory))

    val s0 :: s1 :: s2 :: Nil = Enum(3)
    val state = RegInit(s0)
    val nextState = WireInit(s0)

    val ldResp = RegInit(0.U(64.W))
    val queueIdx = RegNext(RegNext(io.req.bits.idx))
    val addr = RegNext(RegNext(io.req.bits.addr))

    val dataTable = Seq(
        // addr, data, exception
        (0x0fd0.U, BigInt("4040404040404404", 16).U, false.B),
        (0x0fd8.U, BigInt("3030303030303030", 16).U, false.B),
        (0x0fe0.U, BigInt("2020202020202020", 16).U, false.B),
        (0x0fe8.U, BigInt("1010101010101010", 16).U, false.B),
        (0x0ff0.U, BigInt("5678901234503489", 16).U, false.B),
        (0x0ff8.U, BigInt("eeeeeeeeeeeeeeee", 16).U, false.B),
        (0x1000.U, BigInt("0123456789abcdef", 16).U, false.B),
        (0x1008.U, BigInt("ffffffffffffffff", 16).U, false.B),
        (0x1010.U, BigInt("0f0f0f0f0f0f0f0f", 16).U, false.B),
        (0x1018.U, BigInt("fedcba9876543210", 16).U, false.B),
        (0x1020.U, BigInt("1234567890123456", 16).U, false.B),
        (0x1028.U, BigInt("0101010101010101", 16).U, false.B),
        (0x1030.U, BigInt("2345678901234567", 16).U, false.B),
        (0x1038.U, BigInt("1111111111111111", 16).U, false.B),
        (0x1040.U, BigInt("2222222222222222", 16).U, false.B),
        (0x1048.U, BigInt("3333333333333333", 16).U, false.B),
        (0x1050.U, BigInt("4444444444444444", 16).U, false.B),
        (0x1058.U, BigInt("5555555555555555", 16).U, false.B),
        (0x1060.U, BigInt("6666666666666666", 16).U, true.B),
    )
    
    io.xcpt                 := DontCare
    io.req.ready            := true.B

    io.resp.valid           := false.B
    io.resp.bits.nack       := false.B
    io.resp.bits.has_data   := false.B
    io.resp.bits.data       := 0.U
    io.resp.bits.mask       := 0.U
    io.resp.bits.idx        := 0.U

    // fsm transition logic
    when(state === s0) {
        when(io.req.valid) {
            nextState := s1
        }.otherwise {
            nextState := s0
        }
    }.elsewhen(state === s1) {
        nextState := s2
    }.otherwise {
        nextState := s0
    }
    state := nextState

    // fsm output logic
    when(state === s2) {
        io.resp.valid           := true.B
        io.resp.bits.nack       := false.B
        io.resp.bits.has_data   := true.B
        io.resp.bits.mask       := 0.U
        io.resp.bits.idx        := queueIdx

        for(i <- 0 until dataTable.length) {
            when(dataTable(i)._1 === addr) {
                io.resp.bits.data := dataTable(i)._2

                when(dataTable(i)._3) {
                    io.xcpt.ma.ld := true.B
                }.otherwise {
                    io.xcpt.ma.ld := false.B
                }
            }
        }
    }
}

class SmartVectorLsuTestWrapper extends Module {
    val io = IO(new Bundle {
        val mUop = Flipped(ValidIO(new MuopTest))
        val lsuOut = ValidIO(new VLSUOutput)
        val xcpt = Output(new VLSUXcpt)
        val lsuReady = Output(Bool())
    })

    val p = Parameters.empty.alterPartial({
        case SmartParamsKey => SmartParameters(VLEN = 128)
        case VFuParamsKey   => VFuParameters(XLEN = 64, VLEN = 128)
        case XSCoreParamsKey => XSCoreParameters()
    })

    val vLsu = Module(new SVlsu()(p))
  
    io.lsuReady                             := vLsu.io.lsuReady
    vLsu.io.oldVd                           := io.mUop.bits.oldVd
    vLsu.io.mUop.valid                      := io.mUop.valid

    vLsu.io.mUop.bits.scalar_opnd_1         := io.mUop.bits.scalar_opnd_1
    vLsu.io.mUop.bits.scalar_opnd_2         := io.mUop.bits.scalar_opnd_2
    vLsu.io.mUop.bits.uopRegInfo.vs1        := io.mUop.bits.vs1
    vLsu.io.mUop.bits.uopRegInfo.vs2        := io.mUop.bits.vs2
    vLsu.io.mUop.bits.uopRegInfo.vxsat      := false.B
    
    vLsu.io.mUop.bits.uop.sysUop            := DontCare
    vLsu.io.mUop.bits.uop.uopIdx            := io.mUop.bits.uop.splitUopIdx
    vLsu.io.mUop.bits.uop.uopEnd            := io.mUop.bits.uop.splitUopEnd

    vLsu.io.mUop.bits.uop.ctrl.funct6       := io.mUop.bits.uop.ctrl_funct6
    vLsu.io.mUop.bits.uop.ctrl.funct3       := io.mUop.bits.uop.ctrl_funct3
    vLsu.io.mUop.bits.uop.ctrl.load         := io.mUop.bits.uop.ctrl_load
    vLsu.io.mUop.bits.uop.ctrl.store        := io.mUop.bits.uop.ctrl_store
    vLsu.io.mUop.bits.uop.ctrl.vm           := io.mUop.bits.uop.ctrl_vm
    vLsu.io.mUop.bits.uop.ctrl.vs1_imm      := DontCare
    vLsu.io.mUop.bits.uop.ctrl.narrow       := DontCare
    vLsu.io.mUop.bits.uop.ctrl.narrow_to_1  := DontCare
    vLsu.io.mUop.bits.uop.ctrl.widen        := DontCare
    vLsu.io.mUop.bits.uop.ctrl.widen2       := DontCare

    vLsu.io.mUop.bits.uop.info.ma           := io.mUop.bits.uop.info_ma
    vLsu.io.mUop.bits.uop.info.ta           := io.mUop.bits.uop.info_ta
    vLsu.io.mUop.bits.uop.info.vsew         := io.mUop.bits.uop.info_vsew
    vLsu.io.mUop.bits.uop.info.vlmul        := io.mUop.bits.uop.info_vlmul
    vLsu.io.mUop.bits.uop.info.vl           := io.mUop.bits.uop.info_vl
    vLsu.io.mUop.bits.uop.info.vstart       := io.mUop.bits.uop.info_vstart
    vLsu.io.mUop.bits.uop.info.vxrm         := DontCare
    vLsu.io.mUop.bits.uop.info.frm          := DontCare

    io.lsuOut.valid                         := vLsu.io.lsuOut.valid
    io.lsuOut.bits.vd                       := vLsu.io.lsuOut.bits.vd
    io.lsuOut.bits.uopQueueIdx              := 0.U

    io.xcpt <> vLsu.io.xcpt

    val dcache = Module(new FakeDCache)
    vLsu.io.dataExchange <> dcache.io

}


trait VLsuBehavior_ld {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldReqSrc_default = SrcBundleLd()
    val vle8    = CtrlBundle(VLE8_V)
    val vle16   = CtrlBundle(VLE16_V)
    val vle32   = CtrlBundle(VLE32_V)
    val vle64   = CtrlBundle(VLE64_V)
    val vlm     = CtrlBundle(VLM_V)
    val vlse8   = CtrlBundle(VLSE8_V)
    val vlse16  = CtrlBundle(VLSE16_V)
    val vlse32  = CtrlBundle(VLSE32_V)
    val vlse64  = CtrlBundle(VLSE64_V)
    val vse8    = CtrlBundle(VSE8_V)
  
    def vLsuTest0(): Unit = {
        it should "pass: unit-stride load (uops=1, eew=8, vl=8, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle8.copy(vl=8, uopIdx=0, uopEnd=true), ldReqSrc_default, "h201f1e1d1c1b1a190123456789abcdef".U),
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
                    dut.io.lsuOut.bits.vd.expect(r)
                    dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest1(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=8, vl=19, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle8.copy(vl=19, uopIdx=0, uopEnd=false), ldReqSrc_default, "hffffffffffffffff0123456789abcdef".U),
                (vle8.copy(vl=19, uopIdx=1, uopEnd=true),  ldReqSrc_default, "h201f1e1d1c1b1a1918171615140f0f0f".U),
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
                dut.io.lsuOut.bits.vd.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }


    def vLsuTest2(): Unit = {
        it should "pass: unit-stride load (uops=4, eew=16, vl=27, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle16.copy(vl=27, uopIdx=0, uopEnd=false), ldReqSrc_default, "hffffffffffffffff0123456789abcdef".U),
                (vle16.copy(vl=27, uopIdx=1, uopEnd=false), ldReqSrc_default, "hfedcba98765432100f0f0f0f0f0f0f0f".U),
                (vle16.copy(vl=27, uopIdx=2, uopEnd=false), ldReqSrc_default, "h01010101010101011234567890123456".U),
                (vle16.copy(vl=27, uopIdx=3, uopEnd=true),  ldReqSrc_default, "h201f1e1d1c1b1a191817678901234567".U),
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
                dut.io.lsuOut.bits.vd.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest3(): Unit = {
        it should "pass: unit-stride load (uops=3, eew=32, vl=10, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle32.copy(vl=10, uopIdx=0, uopEnd=false), ldReqSrc_default, "hffffffffffffffff0123456789abcdef".U),
                (vle32.copy(vl=10, uopIdx=1, uopEnd=false), ldReqSrc_default, "hfedcba98765432100f0f0f0f0f0f0f0f".U),
                (vle32.copy(vl=10, uopIdx=2, uopEnd=true),  ldReqSrc_default, "h201f1e1d1c1b1a191234567890123456".U),
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
                dut.io.lsuOut.bits.vd.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest4(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=64, vl=3, vstart=0)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle64.copy(vl=3, uopIdx=0, uopEnd=false), ldReqSrc_default, "hffffffffffffffff0123456789abcdef".U),
                (vle64.copy(vl=3, uopIdx=1, uopEnd=true),  ldReqSrc_default, "h201f1e1d1c1b1a190f0f0f0f0f0f0f0f".U),
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
                dut.io.lsuOut.bits.vd.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest5(): Unit = {
        it should "pass: unit-stride load (uops=2, eew=64, vl=3, vstart=1)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle64.copy(vl=3, vstart=1, uopIdx=0, uopEnd=false), ldReqSrc_default, "hffffffffffffffff1817161514131211".U),
                (vle64.copy(vl=3, vstart=1, uopIdx=1, uopEnd=true),  ldReqSrc_default, "h201f1e1d1c1b1a190f0f0f0f0f0f0f0f".U),
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
                dut.io.lsuOut.bits.vd.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest6(): Unit = {
        it should "pass: strided load (uops=1, eew=8, vl=6, vstart=0, stride=-5)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                // 1000~1001(cdef), 1008~1009(ffff), 1010~1011(0f0f), 1018~1019(3210)
                // 1020~1021(3456), 1028~1029(0101), 1030~1031(4567), 1038~1039(1111)
                (vlse8.copy(vl=6, uopIdx=0, uopEnd=true), SrcBundleLd(scalar_opnd_2="hffffffff_fffffffb"), "h201f1e1d1c1b1a19181720103478eeef".U),
                // (vlse8.copy(vl=10, uopIdx=1, uopEnd=true),  SrcBundleLd(scalar_opnd_2="h4"), "h201f1e1d1c1b1a191817161533332222".U),
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
                dut.io.lsuOut.bits.vd.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest7(): Unit = {
        it should "pass: strided load (uops=2, eew=64, vl=3, vstart=0, stride=-1)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vlse32.copy(vl=3, uopIdx=0, uopEnd=true), SrcBundleLd(scalar_opnd_2="hffffffff_ffffffff"), "h201f1e1deeeeeeeeeeeeeeee89abcdef".U),
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
                dut.io.lsuOut.bits.vd.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest8(): Unit = {
        it should "pass: strided load (uops=2, eew=16, vl=10, vstart=0, stride=4)" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                // 1000~1001(cdef), 1008~1009(ffff), 1010~1011(0f0f), 1018~1019(3210)
                // 1020~1021(3456), 1028~1029(0101), 1030~1031(4567), 1038~1039(1111)
                (vlse16.copy(vl=10, uopIdx=0, uopEnd=false), SrcBundleLd(scalar_opnd_2="h4"), "h111145670101345632100f0fffffcdef".U),
                (vlse16.copy(vl=10, uopIdx=1, uopEnd=true),  SrcBundleLd(scalar_opnd_2="h4"), "h201f1e1d1c1b1a191817161533332222".U),
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
                dut.io.lsuOut.bits.vd.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }

    def vLsuTest9(): Unit = {
        it should "pass: unit-strde exception" in {
        test(new SmartVectorLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle8.copy(vl=19, uopIdx=0, uopEnd=false, vstart=1), SrcBundleLd(scalar_opnd_1="h1058"), "h201f1e1d1c1b1a195555555555555511".U),
                (vle8.copy(vl=8, uopIdx=0, uopEnd=true), ldReqSrc_default, "h201f1e1d1c1b1a190123456789abcdef".U),
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
                if(dut.io.xcpt.update_vl.peekBoolean()) {
                    dut.io.xcpt.update_vl.expect(true.B)
                } 
                dut.io.xcpt.update_data.expect(8.U)
                dut.io.lsuOut.bits.vd.expect(r)
                dut.clock.step(4)
            }
        }
        }
    }
}

class VLsuSpec_ld extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_ld {
  behavior of "LSU test"
    it should behave like vLsuTest0()  // unit-stride load
    it should behave like vLsuTest1()  // unit-stride load
    it should behave like vLsuTest2()  // unit-stride load
    it should behave like vLsuTest3()  // unit-stride load
    it should behave like vLsuTest4()  // unit-stride load
    it should behave like vLsuTest5()  // unit-stride load
    it should behave like vLsuTest6()  // strided load
    it should behave like vLsuTest7()  // strided load
    it should behave like vLsuTest8()  // strided load
    it should behave like vLsuTest9()  // strided load
}
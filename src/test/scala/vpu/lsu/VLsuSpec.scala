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
    val ctrl_load = Bool()
    val ctrl_store = Bool()
    val ctrl_vm = Bool()
    val info_ma = Bool()
    val info_ta = Bool()
    val info_vsew = UInt(3.W)
    val info_vlmul = UInt(3.W)
    val info_vl = UInt(bVL.W)
    val info_vstart = UInt(bVstart.W)
    val splitUopIdx = UInt(3.W)
    val splitUopEnd = Bool()
}

class MuopTest extends Bundle {
    val uop = new VUopTest
    val oldVd = Input(UInt(VLEN.W))
    val scalar_opnd_1 = UInt(64.W)
    val scalar_opnd_2 = UInt(64.W)
    val vs1           = UInt(128.W)
    val vs2           = UInt(128.W)
}

class FakeDCache(ldResps: Seq[(UInt, UInt)]) extends Module {
    val io = IO(Flipped(new RVUMemory))

    val s0 :: s1 :: s2 :: Nil = Enum(3)
    val state = RegInit(s0)
    val nextState = WireInit(s0)

    val ldResp = RegInit(0.U(64.W))
    val ldRespSeqId = RegInit(0.U(log2Ceil(ldResps.length).W))
    
    io.xcpt := DontCare
    io.busy := false.B
    io.req.ready := true.B
    io.resp.valid := false.B
    io.resp.bits.replay := false.B
    io.resp.bits.has_data := false.B
    io.resp.bits.data := 0.U
    io.resp.bits.mask := 0.U

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


    when(state === s1) {
        io.resp.valid := false.B
        io.resp.bits.replay := false.B
        io.resp.bits.has_data := false.B
        io.resp.bits.mask := 0.U
        io.req.ready := false.B
    }.elsewhen(state === s2) {
        io.resp.valid := true.B
        io.resp.bits.replay := false.B
        io.resp.bits.has_data := true.B
        io.resp.bits.mask := 0.U

        for(i <- 0 until ldResps.length) {
            when(i.U === ldRespSeqId) {
                io.resp.bits.data := ldResps(i)._1
            }
        }

        ldRespSeqId := ldRespSeqId + 1.U
        
    }
}

class SmartVectorLsuTestWrapper(ldResps: Seq[(UInt, UInt)]) extends Module {
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
  
    io.lsuReady := vLsu.io.lsuReady

    vLsu.io.mUop.valid := io.mUop.valid
    
    vLsu.io.oldVd := io.mUop.bits.oldVd

    vLsu.io.mUop.bits.scalar_opnd_1 := io.mUop.bits.scalar_opnd_1
    vLsu.io.mUop.bits.scalar_opnd_2 := io.mUop.bits.scalar_opnd_2
    vLsu.io.mUop.bits.uopRegInfo.vs1 := io.mUop.bits.vs1
    vLsu.io.mUop.bits.uopRegInfo.vs2 := io.mUop.bits.vs2
    vLsu.io.mUop.bits.uopRegInfo.vxsat := false.B
    vLsu.io.mUop.bits.uop.sysUop := DontCare


    vLsu.io.mUop.bits.uop.ctrl.funct6 := io.mUop.bits.uop.ctrl_funct6
    vLsu.io.mUop.bits.uop.ctrl.funct3 := io.mUop.bits.uop.ctrl_funct3
    vLsu.io.mUop.bits.uop.ctrl.load := io.mUop.bits.uop.ctrl_load
    vLsu.io.mUop.bits.uop.ctrl.store := io.mUop.bits.uop.ctrl_store
    vLsu.io.mUop.bits.uop.ctrl.vm := io.mUop.bits.uop.ctrl_vm
    vLsu.io.mUop.bits.uop.ctrl.vs1_imm := DontCare
    vLsu.io.mUop.bits.uop.ctrl.narrow := DontCare
    vLsu.io.mUop.bits.uop.ctrl.narrow_to_1 := DontCare
    vLsu.io.mUop.bits.uop.ctrl.widen := DontCare
    vLsu.io.mUop.bits.uop.ctrl.widen2 := DontCare

    vLsu.io.mUop.bits.uop.info.ma := io.mUop.bits.uop.info_ma
    vLsu.io.mUop.bits.uop.info.ta := io.mUop.bits.uop.info_ta
    vLsu.io.mUop.bits.uop.info.vsew := io.mUop.bits.uop.info_vsew
    vLsu.io.mUop.bits.uop.info.vlmul := io.mUop.bits.uop.info_vlmul
    vLsu.io.mUop.bits.uop.info.vl := io.mUop.bits.uop.info_vl
    vLsu.io.mUop.bits.uop.info.vstart := io.mUop.bits.uop.info_vstart
    vLsu.io.mUop.bits.uop.info.vxrm := DontCare
    vLsu.io.mUop.bits.uop.info.frm := DontCare
    vLsu.io.mUop.bits.uop.uopIdx := io.mUop.bits.uop.splitUopIdx
    vLsu.io.mUop.bits.uop.uopEnd := io.mUop.bits.uop.splitUopEnd

    io.lsuOut.valid := vLsu.io.lsuOut.valid
    io.lsuOut.bits.vd := vLsu.io.lsuOut.bits.vd
    io.lsuOut.bits.uopQueueIdx := 0.U

    val dcache = Module(new FakeDCache(ldResps))
    vLsu.io.dataExchange <> dcache.io


    io.xcpt := DontCare

}


trait VLsuBehavior_ld {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

    val ldReqSrc_default = SrcBundleLd()
    val vle8 = CtrlBundle(VLE8_V)
    val vle16 = CtrlBundle(VLE16_V)
    val vle32 = CtrlBundle(VLE32_V)
    val vle64 = CtrlBundle(VLE64_V)
    val vlm = CtrlBundle(VLM_V)
    val vlse8 = CtrlBundle(VLSE8_V)
    val vlse16 = CtrlBundle(VLSE16_V)
    val vlse32 = CtrlBundle(VLSE32_V)
    val vlse64 = CtrlBundle(VLSE64_V)
    val vse8 = CtrlBundle(VSE8_V)
  
    val ldRespsTest0 = Seq(
        (0x00040003.U, 0.U),
        (0x00040003.U, 1.U),
        (0x00040003.U, 2.U),
        (0x00040003.U, 3.U),
        (0x00040003.U, 4.U),
        (0x00040003.U, 5.U),
        (0x00040003.U, 6.U),
        (0x00040003.U, 7.U),
    )

    val ldRespsTest1 = Seq(
        (BigInt("0123456789abcdef", 16).U, 0.U),
        (BigInt("0123456789abcdef", 16).U, 1.U),
        (BigInt("0123456789abcdef", 16).U, 2.U),
        (BigInt("0123456789abcdef", 16).U, 3.U),
        (BigInt("0123456789abcdef", 16).U, 4.U),
        (BigInt("0123456789abcdef", 16).U, 5.U),
        (BigInt("0123456789abcdef", 16).U, 6.U),
        (BigInt("0123456789abcdef", 16).U, 7.U),
        (BigInt("ffffffffffffffff", 16).U, 0.U),
        (BigInt("ffffffffffffffff", 16).U, 1.U),
        (BigInt("ffffffffffffffff", 16).U, 2.U),
        (BigInt("ffffffffffffffff", 16).U, 3.U),
        (BigInt("ffffffffffffffff", 16).U, 4.U),
        (BigInt("ffffffffffffffff", 16).U, 5.U),
        (BigInt("ffffffffffffffff", 16).U, 6.U),
        (BigInt("ffffffffffffffff", 16).U, 7.U),
        (BigInt("0f0f0f0f0f0f0f0f", 16).U, 0.U),
        (BigInt("0f0f0f0f0f0f0f0f", 16).U, 1.U),
        (BigInt("0f0f0f0f0f0f0f0f", 16).U, 2.U),
        
    )

  def vLsuTest0(): Unit = {
    it should "pass: unit-stride load (uop=1, eew=8, vl=8, vstart=0)" in {
      test(new SmartVectorLsuTestWrapper(ldRespsTest0)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle8.copy(vl=8, uopIdx=0, uopEnd=true), ldReqSrc_default, "h201f1e1d1c1b1a190000000000040003".U),
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
        test(new SmartVectorLsuTestWrapper(ldRespsTest1)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            test_init(dut)
            dut.clock.step(1)
            val ldReqs = Seq(
                (vle8.copy(vl=19, uopIdx=0, ta=false), ldReqSrc_default, "hffffffffffffffff0123456789abcdef".U), 
                (vle8.copy(vl=19, uopIdx=1, uopEnd=true, ta=false), ldReqSrc_default, "h201f1e1d1c1b1a1918171615140f0f0f".U),
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
}

class VLsuSpec_ld extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_ld {
  behavior of "LSU test"
  it should behave like vLsuTest0()  // unit-stride load
  it should behave like vLsuTest1()  // unit-stride load
}
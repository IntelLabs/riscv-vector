package darecreek.lsutest

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import darecreek.lsu._
import darecreek.ctrl.decode.VInstructions._
import darecreek._

class VExpdUOpTest extends Bundle {
  val ctrl_funct6 = UInt(6.W)
  val ctrl_funct3 = UInt(3.W)
  val ctrl_lsrc_1 = UInt(5.W)
  val ctrl_lsrcVal_2 = Bool()
  val ctrl_load = Bool()
  val ctrl_store = Bool()
  val pdestVal = Bool()
  val ctrl_vm = Bool()
  val info_ma = Bool()
  val info_vsew = UInt(3.W)
  val info_vlmul = UInt(3.W)
  val info_vl = UInt(bVL.W)
  val info_vstart = UInt(bVstart.W)
  val expdIdx = UInt(3.W)
  val expdEnd = Bool()
}

class VLdInputTest extends Bundle {
  val uop = new VExpdUOpTest
  val rs2 = UInt(xLen.W)
  val vs2 = UInt(VLEN.W)
  val oldVd = UInt(VLEN.W)
  val vmask = UInt(VLEN.W)
  val nextVRobIdx = new VRobPtr
  val iqEmpty = Bool()
}

class VStInputTest extends Bundle {
  val uop = new VExpdUOpTest
  val rs2 = UInt(xLen.W)
  val vs2 = UInt(VLEN.W)
  val vs3 = UInt(VLEN.W)
  val vmask = UInt(VLEN.W)
  val nextVRobIdx = new VRobPtr
  val iqEmpty = Bool()
}

class VLsuTestWrapper extends Module {
  val io = IO(new Bundle {
    val fromIQ = new Bundle {
      val ld = Flipped(Decoupled(new VLdInputTest))
      val st = Flipped(Decoupled(new VStInputTest))
    }
    val wb = new Bundle {
      val ld = ValidIO(new VLdOutput)
      val st = ValidIO(new VStOutput)
    }
    val stateIsStore = Output(Bool())
    // OVI interfaces
    val ovi_memop = new OVImemop
    val ovi_load = new OVIload
    val ovi_store = new OVIstore
    val ovi_maskIdx = new OVImaskIdx
  })

  val vLsu = Module(new VLsu)
  io.wb := vLsu.io.wb
  vLsu.io.ovi_memop <> io.ovi_memop
  vLsu.io.ovi_load <> io.ovi_load
  vLsu.io.ovi_store <> io.ovi_store
  vLsu.io.ovi_maskIdx <> io.ovi_maskIdx
  io.stateIsStore := vLsu.io.stateIsStore

  vLsu.io.fromIQ.ld.valid := io.fromIQ.ld.valid
  vLsu.io.fromIQ.st.valid := io.fromIQ.st.valid
  io.fromIQ.ld.ready := vLsu.io.fromIQ.ld.ready
  io.fromIQ.st.ready := vLsu.io.fromIQ.st.ready

  Seq((vLsu.io.fromIQ.ld.bits, io.fromIQ.ld.bits), 
      (vLsu.io.fromIQ.st.bits, io.fromIQ.st.bits)).foreach {
    case (lsu_bits, io_bits) => {
      lsu_bits.elements.foreach {
        case (name, data) => {
          if (name != "uop") {
            data := io_bits.elements(name)
          }
        }
      }
    }
  }

  vLsu.io.fromIQ.ld.bits.uop := 0.U.asTypeOf(new VExpdUOp)
  vLsu.io.fromIQ.st.bits.uop := 0.U.asTypeOf(new VExpdUOp)

  Seq((vLsu.io.fromIQ.ld.bits.uop, io.fromIQ.ld.bits.uop), 
      (vLsu.io.fromIQ.st.bits.uop, io.fromIQ.st.bits.uop)).foreach {
    case (lsu_uop, io_uop) => {
      lsu_uop.ctrl.funct6 := io_uop.ctrl_funct6
      lsu_uop.ctrl.funct3 := io_uop.ctrl_funct3
      lsu_uop.ctrl.lsrc(1) := io_uop.ctrl_lsrc_1
      lsu_uop.ctrl.lsrcVal(2) := io_uop.ctrl_lsrcVal_2
      lsu_uop.ctrl.load := io_uop.ctrl_load
      lsu_uop.ctrl.store := io_uop.ctrl_store
      lsu_uop.pdestVal := io_uop.pdestVal
      lsu_uop.ctrl.vm := io_uop.ctrl_vm
      lsu_uop.info.ma := io_uop.info_ma
      lsu_uop.info.vsew := io_uop.info_vsew
      lsu_uop.info.vlmul := io_uop.info_vlmul
      lsu_uop.info.vl := io_uop.info_vl
      lsu_uop.info.vstart := io_uop.info_vstart
      lsu_uop.expdIdx := io_uop.expdIdx
      lsu_uop.expdEnd := io_uop.expdEnd
    }
  }
}

trait VLsuBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val ldReqSrc_default = SrcBundleLd()
  val vle8 = CtrlBundle(VLE8_V)
  val vse8 = CtrlBundle(VSE8_V)
  
  def vLsuTest0(): Unit = {
    it should "pass: unit-stride load (sew=8, el_count=64, el_off=0, el_id=0)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle8.copy(vl=64, uopIdx=0), ldReqSrc_default),
          (vle8.copy(vl=64, uopIdx=1, uopEnd=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
           "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=64)),
        )
        next_is_load_and_step(dut)
        for ((c, s) <- ldReqs) {
          while (!dut.io.fromIQ.ld.ready.peekBoolean()) {
            dut.clock.step(1)
          }
          dut.io.fromIQ.ld.valid.poke(true.B)
          dut.io.fromIQ.ld.bits.poke(genLdInput(c, s))
          dut.clock.step(1)
        }
        dut.io.fromIQ.ld.valid.poke(false.B)
        dut.clock.step(4)

        fork {
          for ((ldData, seqId)  <- ldResps) {
            one_512b_load_resp(dut, ldData, seqId.asUInt)
          }
          dut.clock.step(1)
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("h0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef".U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d".U)
              }
            }
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest1(): Unit = {
    it should "pass: unit-stride load (sew=8, el_count=32, el_off=32, el_id=0)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle8.copy(vl=32, uopIdx=0), ldReqSrc_default),
          (vle8.copy(vl=32, uopIdx=1, uopEnd=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=32, el_off=32)),
        )
        next_is_load_and_step(dut)
        for ((c, s) <- ldReqs) {
          while (!dut.io.fromIQ.ld.ready.peekBoolean()) {
            dut.clock.step(1)
          }
          dut.io.fromIQ.ld.valid.poke(true.B)
          dut.io.fromIQ.ld.bits.poke(genLdInput(c, s))
          dut.clock.step(1)
        }
        dut.io.fromIQ.ld.valid.poke(false.B)
        dut.clock.step(4)

        fork {
          for ((ldData, seqId)  <- ldResps) {
            one_512b_load_resp(dut, ldData, seqId.asUInt)
          }
          dut.clock.step(1)
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffffffff_ffffffffffffffff_ffffffffffffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
              }
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }


}

class VLsuSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior {
  behavior of "LSU test"
  // it should behave like vLsuTest0()  // unit-stride load
  it should behave like vLsuTest1()  // unit-stride load
}
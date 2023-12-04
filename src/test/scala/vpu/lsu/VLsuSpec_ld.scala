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
  val info_ta = Bool()
  val info_vsew = UInt(3.W)
  val info_vlmul = UInt(3.W)
  val info_vl = UInt(bVL.W)
  val info_vstart = UInt(bVstart.W)
  val info_destEew = UInt(3.W)
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
      lsu_uop.info.ta := io_uop.info_ta
      lsu_uop.info.vsew := io_uop.info_vsew
      lsu_uop.info.vlmul := io_uop.info_vlmul
      lsu_uop.info.vl := io_uop.info_vl
      lsu_uop.info.vstart := io_uop.info_vstart
      lsu_uop.info.destEew := io_uop.info_destEew
      lsu_uop.expdIdx := io_uop.expdIdx
      lsu_uop.expdEnd := io_uop.expdEnd
    }
  }
}

trait VLsuBehavior_ld {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val ldReqSrc_default = SrcBundleLd()
  val vle8 = CtrlBundle(VLE8_V)
  val vle16 = CtrlBundle(VLE16_V, destEew=1)
  val vle32 = CtrlBundle(VLE32_V, destEew=2)
  val vle64 = CtrlBundle(VLE64_V, destEew=3)
  val vlm = CtrlBundle(VLM_V)
  val vlse8 = CtrlBundle(VLSE8_V)
  val vlse16 = CtrlBundle(VLSE16_V, destEew=1)
  val vlse32 = CtrlBundle(VLSE32_V, destEew=2)
  val vlse64 = CtrlBundle(VLSE64_V, destEew=3)
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
    it should "pass: unit-stride load (sew=8, el_count=19, el_off=40, el_id=0)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle8.copy(vl=19, uopIdx=0, ta=false), ldReqSrc_default),
          (vle8.copy(vl=19, uopIdx=1, uopEnd=true, ta=false), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=19, el_off=40)),
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
                dut.io.wb.ld.bits.vd.expect("h201f1e1d1c1b1a19_1817161514020001_0008000700060005_000c000b000a0009".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("h201f1e1d1c1b1a19_1817161514131211_100f0e0d0c0b0a09_0807060504030201".U)
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

  def vLsuTest2(): Unit = {
    it should "pass: unit-stride load (sew=8, el_count=60, el_off=0, el_id=67)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle8.copy(vl=60, uopIdx=0), ldReqSrc_default),
          (vle8.copy(vl=60, uopIdx=1, uopEnd=false), ldReqSrc_default),
          (vle8.copy(vl=60, uopIdx=2, uopEnd=false), ldReqSrc_default),
          (vle8.copy(vl=60, uopIdx=3, uopEnd=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=60, el_off=0, el_id=67)),
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
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffffffff_ffffffffffffffff_ffffffffffffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else if (wb_cnt == 1) {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffffffff_ffffffffffffffff_ffffffffffffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              } else if (wb_cnt == 2) {
                dut.io.wb.ld.bits.vd.expect("h6701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef_ffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(2.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("hff00020001_0008000700060005_000c000b000a0009_0001000f000e000d_012345".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(3.U)
              }
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest3(): Unit = {
    it should "pass: unit-stride load (sew=8, el_count=40, el_off=5, el_id=10)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle8.copy(vl=40, uopIdx=0), ldReqSrc_default),
          (vle8.copy(vl=40, uopIdx=1, uopEnd=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=40, el_off=5, el_id=10)),
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
                dut.io.wb.ld.bits.vd.expect("h234567_89abcdef89abcdef_0123456701234567_89abcd_ffff_ffffffffffffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("hffffff_ffffffffffffffff_ffffff0b000a0009_0001000f000e000d_0123456701".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              } 
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest4(): Unit = {
    it should "pass: unit-stride load (sew=16, el_count=27, el_off=0, el_id=10)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle16.copy(vl=27, uopIdx=0), ldReqSrc_default),
          (vle16.copy(vl=27, uopIdx=1, uopEnd=false), ldReqSrc_default),
          (vle16.copy(vl=27, uopIdx=2, uopEnd=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=27, el_off=0, el_id=10)),
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
                dut.io.wb.ld.bits.vd.expect("h01234567_89abcdef89abcdef_ffffffffffffffff_ffffffffffffffff_ffffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else if (wb_cnt == 1) {
                dut.io.wb.ld.bits.vd.expect("h000a0009_0001000f000e000d_fedcba98fedcba98_7654321076543210_01234567".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("hffffffff_ffffffffffffffff_ffffffffffffffff_ffff000700060005_000c000b".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(2.U)
                wb_cnt += 1
              } 
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest5(): Unit = {
    it should "pass: unit-stride load (sew=32, el_count=15, el_off=0, el_id=5)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle32.copy(vl=18, uopIdx=0), ldReqSrc_default),
          (vle32.copy(vl=18, uopIdx=1, uopEnd=false), ldReqSrc_default),
          (vle32.copy(vl=18, uopIdx=2, uopEnd=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=15, el_off=0, el_id=5)),
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
                dut.io.wb.ld.bits.vd.expect("h01234567_89abcdef89abcdef_ffffffffffffffff_ffffffffffffffff_ffffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else if (wb_cnt == 1) {
                dut.io.wb.ld.bits.vd.expect("h000a0009_0001000f000e000d_fedcba98fedcba98_7654321076543210_01234567".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("hffffffff_ffffffffffffffff_ffffffff00020001_0008000700060005_000c000b".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(2.U)
                wb_cnt += 1
              } 
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest6(): Unit = {
    it should "pass: unit-stride load (sew=64, vl=15)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle64.copy(vl=15, uopIdx=0), ldReqSrc_default),
          (vle64.copy(vl=15, uopIdx=1, uopEnd=false), ldReqSrc_default),
          (vle64.copy(vl=15, uopIdx=2, uopEnd=false), ldReqSrc_default),
          (vle64.copy(vl=15, uopIdx=3, uopEnd=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=7, el_id=0)),
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=8, el_off=0, el_id=1)),
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=6, el_off=0, el_id=9)),
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
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 20) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("h7654321076543210_0123456701234567_89abcdef89abcdef_0004000300020001".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else if (wb_cnt == 1) {
                dut.io.wb.ld.bits.vd.expect("h0008000700060005_000c000b000a0009_0001000f000e000d_fedcba98fedcba98".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              } else if (wb_cnt == 2) {
                dut.io.wb.ld.bits.vd.expect("h7654321076543210_0123456701234567_89abcdef89abcdef_0004000300020001".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(2.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_000c000b000a0009_0001000f000e000d_fedcba98fedcba98".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(3.U)
                wb_cnt += 1
              } 
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest7(): Unit = {
    it should "pass: strided load (stride=-1)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vlse32.copy(vl=5, uopIdx=0, uopEnd=true), SrcBundleLd(rs2="hffffffff_ffffffff")),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=5, el_off=5, el_id=2)),
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
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 14) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("hfffffffffedcba98_fedcba98000e000d_0001000f000a0009_ffffffffffffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } /*else if (wb_cnt == 1) {
                dut.io.wb.ld.bits.vd.expect("h0008000700060005_000c000b000a0009_0001000f000e000d_fedcba98fedcba98".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              }*/
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest8(): Unit = {
    it should "pass: strided load (stride=2, sew=16)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vlse16.copy(vl=5, uopIdx=0, uopEnd=true), SrcBundleLd(rs2="h4")),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=5, el_off=17, el_id=5)),
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
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 14) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("hffffffffffff_ffffffffffff0006_000c000a0001000e_ffffffffffffffff_ffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } /*else if (wb_cnt == 1) {
                dut.io.wb.ld.bits.vd.expect("h0008000700060005_000c000b000a0009_0001000f000e000d_fedcba98fedcba98".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              }*/
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest9(): Unit = {
    it should "pass: strided load (stride=4, sew=32)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vlse32.copy(vl=7, uopIdx=0), SrcBundleLd(rs2="h10")),
          (vlse32.copy(vl=7, uopIdx=1, uopEnd=true), SrcBundleLd(rs2="h10")),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=4, el_off=1, el_id=7)),
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=3, el_off=1, el_id=11)),
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
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 14) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("h89abcdefffffffff_ffffffffffffffff_ffffffffffffffff_ffffffffffffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else if (wb_cnt == 1) {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_0001000f76543210_89abcdef00080007_0001000f76543210".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              }
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest10(): Unit = {
    it should "pass: strided load (stride=-4, sew=8)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vlse8.copy(vl=5, uopIdx=0, uopEnd=true), SrcBundleLd(rs2="hffffffff_fffffffc")),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=5, el_off=45, el_id=17)),
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
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 14) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffabab232354ff_ffffffffffffffff_ffffffffffffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } /*else if (wb_cnt == 1) {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_0001000f76543210_89abcdef00080007_0001000f76543210".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              }*/
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest11(): Unit = {
    it should "pass: strided load (stride=-5, sew=32)" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vlse32.copy(vl=1, uopIdx=0, uopEnd=true), SrcBundleLd(rs2="hffffffff_ffffffec")),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "fedcba98fedcba98_7654321076543210_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=5, el_id=7)),
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
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 14) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("h000a0009ffffffff_ffffffffffffffff_ffffffffffffffff_ffffffffffffffff".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } /*else if (wb_cnt == 1) {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_0001000f76543210_89abcdef00080007_0001000f76543210".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              }*/
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest12(): Unit = {
    it should "pass: unit-stride load with mask sew=8" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle8.copy(vl=40+10, uopIdx=0, vm=false), ldReqSrc_default.copy(mask="h4d")),
          (vle8.copy(vl=40+10, uopIdx=1, uopEnd=true, vm=false), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=40, el_off=5, el_id=10), ("h4d", true)),
        )
        next_is_load_and_step(dut)
        fork {
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
        }.fork {
          var maskIdx_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.ovi_maskIdx.valid.peekBoolean()) {
              if (maskIdx_cnt == 0) {
                dut.io.ovi_maskIdx.item.expect("h4d".U)
                maskIdx_cnt += 1
              }
              dut.io.ovi_maskIdx.credit.poke(true.B)
            } else {
              dut.io.ovi_maskIdx.credit.poke(false.B)
            }
            dut.clock.step(1)
          }
        }.join()

        fork {
          for ((ldData, seqId, mask)  <- ldResps) {
            one_512b_load_resp(dut, ldData, seqId.asUInt, mask)
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("h201f1e1d1c1b1a19_1817161514131201_100f67890ccd0a09_0807060504030201".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffff_1211_100f0e0d0c0b0a09_0807060504030201".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              } 
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest13(): Unit = {
    it should "pass: unit-stride load with mask sew=32" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle32.copy(vl=15, uopIdx=0, vm=false, ta=false), ldReqSrc_default.copy(mask="h12345")),
          (vle32.copy(vl=15, uopIdx=1, uopEnd=true, vm=false, ta=false), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=10, el_off=6, el_id=0), ("h12345", true)),
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=5, el_off=0, el_id=10), ("h48", true)),
        )
        next_is_load_and_step(dut)
        fork {
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
        }.fork {
          var maskIdx_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.ovi_maskIdx.valid.peekBoolean()) {
              if (maskIdx_cnt == 0) {
                dut.io.ovi_maskIdx.item.expect("h12345".U)
                maskIdx_cnt += 1
              }
              dut.io.ovi_maskIdx.credit.poke(true.B)
            } else {
              dut.io.ovi_maskIdx.credit.poke(false.B)
            }
            dut.clock.step(1)
          }
        }.join()

        fork {
          for ((ldData, seqId, mask)  <- ldResps) {
            one_512b_load_resp(dut, ldData, seqId.asUInt, mask)
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("h201f1e1d00060005_1817161514131211_100f0e0d000e000d_0807060501234567".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("h201f1e1d1c1b1a19_0123456714131211_100f0e0d0c0b0a09_0004000300020001".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              } 
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest14(): Unit = {
    it should "pass: unit-stride/strided load with mask sew=16" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vlse16.copy(vl=17, uopIdx=0, vm=false), ldReqSrc_default.copy(rs2="h4", mask="h12345")),
          (vlse16.copy(vl=17, uopIdx=1, uopEnd=true, vm=false), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=11, el_off=11, el_id=0), ("h12345", true)),
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=6, el_off=1, el_id=11), ("h24", true)),
        )
        next_is_load_and_step(dut)
        fork {
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
        }.fork {
          var maskIdx_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.ovi_maskIdx.valid.peekBoolean()) {
              if (maskIdx_cnt == 0) {
                dut.io.ovi_maskIdx.item.expect("h12345".U)
                maskIdx_cnt += 1
              }
              dut.io.ovi_maskIdx.credit.poke(true.B)
            } else {
              dut.io.ovi_maskIdx.credit.poke(false.B)
            }
            dut.clock.step(1)
          }
        }.join()

        fork {
          for ((ldData, seqId, mask)  <- ldResps) {
            one_512b_load_resp(dut, ldData, seqId.asUInt, mask)
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("h201f1e1d01231a19_1817161500020008_100f000c0c0b0a09_08070123040389ab".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffffffff_ffffffffffffffff_ffffffffffff89ab".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              } 
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest15(): Unit = {
    it should "pass: unit-stride mask load: vlm.v" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vlm.copy(vl=27, uopIdx=0, uopEnd=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=4, el_off=18, el_id=0)),
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
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 14) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffffffff_ffffffffffffffff_ffffffffcdef89ab".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } 
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest16(): Unit = {
    it should "pass: unit-stride/strided load with mask & vstart" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle16.copy(vl=22, vstart=5, uopIdx=0, vm=false, ma=true), ldReqSrc_default.copy(mask="h12345")),
          (vle16.copy(vl=22, vstart=5, uopIdx=1, uopEnd=true, vm=false, ma=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=11, el_off=11, el_id=5), ("h91a", true)),
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=6, el_off=1, el_id=16), ("h1", true)),
        )
        next_is_load_and_step(dut)
        fork {
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
        }.fork {
          var maskIdx_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.ovi_maskIdx.valid.peekBoolean()) {
              if (maskIdx_cnt == 0) {
                dut.io.ovi_maskIdx.item.expect("h91a".U)
                maskIdx_cnt += 1
              }
              dut.io.ovi_maskIdx.credit.poke(true.B)
            } else {
              dut.io.ovi_maskIdx.credit.poke(false.B)
            }
            dut.clock.step(1)
          }
        }.join()

        fork {
          for ((ldData, seqId, mask)  <- ldResps) {
            one_512b_load_resp(dut, ldData, seqId.asUInt, mask)
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("hffffffff_0001ffffffffffff_01234567ffff4567ffff_0a090807060504030201".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffffffff_ffffffffffffffff_ffffffffffff89ab".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
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

class VLsuSpec_ld extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_ld {
  behavior of "LSU test"
  it should behave like vLsuTest0()  // unit-stride load
  it should behave like vLsuTest1()  // unit-stride load
  it should behave like vLsuTest2()  // unit-stride load
  it should behave like vLsuTest3()  // unit-stride load
  it should behave like vLsuTest4()  // unit-stride load
  it should behave like vLsuTest5()  // unit-stride load
  it should behave like vLsuTest6()  // unit-stride load
  it should behave like vLsuTest7()  // strided load
  it should behave like vLsuTest8()  // strided load
  it should behave like vLsuTest9()  // strided load
  it should behave like vLsuTest10()  // strided load
  it should behave like vLsuTest11()
  it should behave like vLsuTest12()
  it should behave like vLsuTest13()
  it should behave like vLsuTest14()
  it should behave like vLsuTest15()
  it should behave like vLsuTest16()
}
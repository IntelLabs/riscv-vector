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
  
  vLsu.io.fromIQ.ld.bits.rs2 := io.fromIQ.ld.bits.rs2
  vLsu.io.fromIQ.ld.bits.vs2 := io.fromIQ.ld.bits.vs2
  vLsu.io.fromIQ.ld.bits.oldVd := io.fromIQ.ld.bits.oldVd
  vLsu.io.fromIQ.ld.bits.vmask := io.fromIQ.ld.bits.vmask
  vLsu.io.fromIQ.ld.bits.nextVRobIdx := io.fromIQ.ld.bits.nextVRobIdx
  vLsu.io.fromIQ.ld.bits.iqEmpty := io.fromIQ.ld.bits.iqEmpty

  vLsu.io.fromIQ.st.bits.rs2 := io.fromIQ.st.bits.rs2
  vLsu.io.fromIQ.st.bits.vs2 := io.fromIQ.st.bits.vs2
  vLsu.io.fromIQ.st.bits.vs3 := io.fromIQ.st.bits.vs3
  vLsu.io.fromIQ.st.bits.vmask := io.fromIQ.st.bits.vmask
  vLsu.io.fromIQ.st.bits.nextVRobIdx := io.fromIQ.st.bits.nextVRobIdx
  vLsu.io.fromIQ.st.bits.iqEmpty := io.fromIQ.st.bits.iqEmpty

  vLsu.io.fromIQ.ld.bits.uop := 0.U.asTypeOf(new VExpdUOp)
  vLsu.io.fromIQ.st.bits.uop := 0.U.asTypeOf(new VExpdUOp)

  vLsu.io.fromIQ.ld.bits.uop.ctrl.funct6 := io.fromIQ.ld.bits.uop.ctrl_funct6
  vLsu.io.fromIQ.ld.bits.uop.ctrl.funct3 := io.fromIQ.ld.bits.uop.ctrl_funct3
  vLsu.io.fromIQ.ld.bits.uop.ctrl.lsrc(1) := io.fromIQ.ld.bits.uop.ctrl_lsrc_1
  vLsu.io.fromIQ.ld.bits.uop.ctrl.lsrcVal(2) := io.fromIQ.ld.bits.uop.ctrl_lsrcVal_2
  vLsu.io.fromIQ.ld.bits.uop.ctrl.load := io.fromIQ.ld.bits.uop.ctrl_load
  vLsu.io.fromIQ.ld.bits.uop.ctrl.store := io.fromIQ.ld.bits.uop.ctrl_store
  vLsu.io.fromIQ.ld.bits.uop.pdestVal := io.fromIQ.ld.bits.uop.pdestVal
  vLsu.io.fromIQ.ld.bits.uop.ctrl.vm := io.fromIQ.ld.bits.uop.ctrl_vm
  vLsu.io.fromIQ.ld.bits.uop.info.ma := io.fromIQ.ld.bits.uop.info_ma
  vLsu.io.fromIQ.ld.bits.uop.info.vsew := io.fromIQ.ld.bits.uop.info_vsew
  vLsu.io.fromIQ.ld.bits.uop.info.vlmul := io.fromIQ.ld.bits.uop.info_vlmul
  vLsu.io.fromIQ.ld.bits.uop.info.vl := io.fromIQ.ld.bits.uop.info_vl
  vLsu.io.fromIQ.ld.bits.uop.info.vstart := io.fromIQ.ld.bits.uop.info_vstart
  vLsu.io.fromIQ.ld.bits.uop.expdIdx := io.fromIQ.ld.bits.uop.expdIdx
  vLsu.io.fromIQ.ld.bits.uop.expdEnd := io.fromIQ.ld.bits.uop.expdEnd

  vLsu.io.fromIQ.st.bits.uop.ctrl.funct6 := io.fromIQ.st.bits.uop.ctrl_funct6
  vLsu.io.fromIQ.st.bits.uop.ctrl.funct3 := io.fromIQ.st.bits.uop.ctrl_funct3
  vLsu.io.fromIQ.st.bits.uop.ctrl.lsrc(1) := io.fromIQ.st.bits.uop.ctrl_lsrc_1
  vLsu.io.fromIQ.st.bits.uop.ctrl.lsrcVal(2) := io.fromIQ.st.bits.uop.ctrl_lsrcVal_2
  vLsu.io.fromIQ.st.bits.uop.ctrl.load := io.fromIQ.st.bits.uop.ctrl_load
  vLsu.io.fromIQ.st.bits.uop.ctrl.store := io.fromIQ.st.bits.uop.ctrl_store
  vLsu.io.fromIQ.st.bits.uop.pdestVal := io.fromIQ.st.bits.uop.pdestVal
  vLsu.io.fromIQ.st.bits.uop.ctrl.vm := io.fromIQ.st.bits.uop.ctrl_vm
  vLsu.io.fromIQ.st.bits.uop.info.ma := io.fromIQ.st.bits.uop.info_ma
  vLsu.io.fromIQ.st.bits.uop.info.vsew := io.fromIQ.st.bits.uop.info_vsew
  vLsu.io.fromIQ.st.bits.uop.info.vlmul := io.fromIQ.st.bits.uop.info_vlmul
  vLsu.io.fromIQ.st.bits.uop.info.vl := io.fromIQ.st.bits.uop.info_vl
  vLsu.io.fromIQ.st.bits.uop.info.vstart := io.fromIQ.st.bits.uop.info_vstart
  vLsu.io.fromIQ.st.bits.uop.expdIdx := io.fromIQ.st.bits.uop.expdIdx
  vLsu.io.fromIQ.st.bits.uop.expdEnd := io.fromIQ.st.bits.uop.expdEnd
}

trait VLsuBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val ldReqSrc_default = SrcBundleLd()
  val vle8 = CtrlBundle(VLE8_V)
  val vse8 = CtrlBundle(VSE8_V)
  
 
  def vLsuTest0(): Unit = {
    it should "pass the test: unit-stride load" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val ldReqs = Seq(
          (vle8.copy(uopEnd=true), ldReqSrc_default),
        )

        for ((c, s) <- ldReqs) {
          next_is_load_and_step(dut)
          while (!dut.io.fromIQ.ld.ready.peekBoolean()) {
            dut.clock.step(1)
          }
          dut.io.fromIQ.ld.valid.poke(true.B)
          dut.io.fromIQ.ld.bits.poke(genLdInput(c, s))
        }
        dut.clock.step(1)
        dut.io.fromIQ.ld.valid.poke(false.B)

        dut.clock.step(4)

        dut.io.ovi_memop.sync_end.poke(false.B)
        dut.io.ovi_load.valid.poke(true.B)
        dut.io.ovi_load.seq_id.poke(SeqId().asUInt)
        dut.io.ovi_load.data.poke(("h1234567812345678_1234567812345678_1234567812345678_1234567812345678" + 
                                    "1234567812345678_1234567812345678_1234567812345678_1234567812345678").U)
        dut.io.ovi_load.mask_valid.poke(false.B)
        dut.io.ovi_store.credit.poke(false.B)
        dut.io.ovi_maskIdx.credit.poke(false.B)
        dut.clock.step(1)
        dut.io.ovi_load.valid.poke(false.B)

        for (i <- 0 until 5) {
          if (dut.io.wb.ld.valid.peekBoolean()) {
            dut.io.wb.ld.bits.vd.expect("h1234567812345678_1234567812345678_1234567812345678_1234567812345678".U)
          }
          if (i == 1) {dut.io.ovi_memop.sync_end.poke(true.B)}
          if (i == 2) {dut.io.ovi_memop.sync_end.poke(false.B)}
          dut.clock.step(1)
        }



        // dut.io.wb.ld.valid.expect(true.B)
        // dut.io.wb.ld.bits.vd.expect("h1234567812345678_1234567812345678_1234567812345678_1234567812345678".U)

      }
    }
  }

 

}

class VLsuSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior {
  behavior of "LSU test"
  it should behave like vLsuTest0()  // unit-stride load

}
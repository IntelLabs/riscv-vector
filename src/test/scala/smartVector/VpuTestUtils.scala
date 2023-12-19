package smartVector

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import smartVector._
import SmartParam._

object test_init {
  def apply(dut: SmartVectorTestWrapper): Unit = {
    dut.clock.setTimeout(0)
    dut.io.rvuIssue.valid.poke(false.B)
  }
}
object next_is_load_and_step {
  def apply(dut: SmartVectorTestWrapper): Unit = {
    dut.io.rvuIssue.valid.poke(false.B)
    dut.clock.step(1)
  }
}

case class CtrlBundle(instrn: String = "h0",
                      ma: Boolean = false,
                      ta: Boolean = true,
                      vsew: Int = 0,
                      vlmul: Int = 1,
                      vl: Int = 16,
                      vstart: Int = 0,
                      vxrm: Int = 0,
                      frm: Int = 0,
)

case class SrcBundleLd(rs1: String = "h1000",
                       rs2: String = "h0",
)              

trait BundleGenHelper {
  def genCtrl(c: CtrlBundle) = {
    (new CtrlTest).Lit(
        _.inst -> c.instrn.U,
        _.info_ma -> c.ma.B,
        _.info_ta -> c.ta.B,
        _.info_vsew -> c.vsew.U,
        _.info_vlmul -> c.vlmul.U,
        _.info_vl -> c.vl.U,
        _.info_vstart -> c.vstart.U,
        _.info_vxrm -> c.vxrm.U,
        _.info_frm -> c.frm.U,
    )
  }

  def genLdInput(c: CtrlBundle, s: SrcBundleLd) = {
    (new VIssueTest).Lit(
        _.ctrl -> genCtrl(c),
        _.rs1 -> s.rs1.U,
        _.rs2 -> s.rs2.U,
    )
  }
}

class CtrlTest extends Bundle {
    val inst        = UInt(32.W)
    val info_ma     = Bool() // vector mask agnostic, data unknown or undisturbed
    val info_ta     = Bool() // vector tail agnostic, data unknown or undisturbed
    val info_vsew   = UInt(3.W)
    val info_vlmul  = UInt(3.W)
    val info_vl     = UInt(bVL.W)
    val info_vstart = UInt(bVstart.W)
    val info_vxrm   = UInt(2.W)
    val info_frm    = UInt(3.W)
}

class VIssueTest extends Bundle {
    val ctrl = new CtrlTest
    val rs1 = UInt(XLEN.W)
    val rs2 = UInt(XLEN.W)
}
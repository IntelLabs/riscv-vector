package smartVector

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.VFuParamsKey
import darecreek.exu.vfu.VFuParameters
import xiangshan.XSCoreParamsKey
import xiangshan.XSCoreParameters
import smartVector._
import SmartParam._
import smartVector.lsutest.LSUFakeDCache
import smartVector.lsutest.DataTable

case class CtrlBundle(instrn: String = "h0",
                      ma: Boolean = false,
                      ta: Boolean = false,
                      vsew: Int = 0,
                      vlmul: Int = 1,
                      vl: Int = 16,
                      vstart: Int = 0,
                      vxrm: Int = 0,
                      frm: Int = 0,
)

case class SrcBundleLdst(rs1: String = "h1000",
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

  def genLdstInput(c: CtrlBundle, s: SrcBundleLdst) = {
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

class SmartVectorTestWrapper extends Module {
    val io = IO(new Bundle{
        val rvuIssue = Flipped(Decoupled(new VIssueTest))
        val rvuCommit = Output(new RVUCommit)
        val rfData = Output(Vec(NVPhyRegs, UInt(VLEN.W)))
        val memInfo = Output(Vec(DataTable.dataTable.length, UInt(64.W)))
    })

    val p = Parameters.empty.alterPartial({
        case SmartParamsKey => SmartParameters(VLEN = 128)
        case VFuParamsKey   => VFuParameters(XLEN = 64, VLEN = 128)
        case XSCoreParamsKey => XSCoreParameters()
    })

    val smartVector = Module(new SmartVector())
  
    smartVector.io.in.valid := io.rvuIssue.valid
    smartVector.io.in.bits.inst := io.rvuIssue.bits.ctrl.inst
    smartVector.io.in.bits.rs1 := io.rvuIssue.bits.rs1
    smartVector.io.in.bits.rs2 := io.rvuIssue.bits.rs2
    smartVector.io.in.bits.vInfo.vl := io.rvuIssue.bits.ctrl.info_vl
    smartVector.io.in.bits.vInfo.vstart := io.rvuIssue.bits.ctrl.info_vstart
    smartVector.io.in.bits.vInfo.vma := io.rvuIssue.bits.ctrl.info_ma
    smartVector.io.in.bits.vInfo.vta := io.rvuIssue.bits.ctrl.info_ta
    smartVector.io.in.bits.vInfo.vsew := io.rvuIssue.bits.ctrl.info_vsew
    smartVector.io.in.bits.vInfo.vlmul := io.rvuIssue.bits.ctrl.info_vlmul
    smartVector.io.in.bits.vInfo.vxrm := 0.U
    smartVector.io.in.bits.vInfo.frm := 0.U
    smartVector.io.in.bits.frs1 := DontCare
    
    

    io.rvuIssue.ready := smartVector.io.in.ready
    io.rfData := smartVector.io.rfData
    io.rvuCommit <> smartVector.io.out.rvuCommit

    val dcache = Module(new LSUFakeDCache)
    smartVector.io.rvuMemory <> dcache.io.dataExchange

    io.memInfo := dcache.io.memInfo
}
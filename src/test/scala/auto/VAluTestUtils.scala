
package darecreek.vfuAutotest.alu

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import darecreek.exu.vfu._
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import xiangshan.MicroOp
import darecreek.exu.vfu.perm._
import chipsalliance.rocketchip.config._
import xiangshan._
import xiangshan.backend.rob.RobPtr


class VFpuInput(implicit p: Parameters) extends Bundle {
  // val in = new VFuInput
  val in = new VFuInput
  val redirect = ValidUndirectioned(new Redirect)
}

case class SrcBundle(var vs2: String = "h0",
                     var vs1: String = "h0",
                     var old_vd: String = "h0",
                     var mask: String = "hffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
                     var rs1: String = "h0"
)

abstract class TestCtrlBundleBase

case class CtrlBundle(instrn: BitPat,
                      vm: Boolean = true,
                      var vs1_imm: Int = 0,
                      widen: Boolean = false,
                      widen2: Boolean = false,
                      narrow: Boolean = false,
                      narrow_to_1: Boolean = false,
                      ma: Boolean = false,
                      ta: Boolean = true,
                      vsew: Int = 0,
                      vlmul: Int = 0,
                      vl: Int = 16,
                      vstart: Int = 0,
                      vxrm : Int = 0,
                      frm : Int = 0,
                      uopIdx: Int = 0,
                      uopEnd : Boolean = false,
                      robIdx: (Boolean, Int) = (false, 0),
) extends TestCtrlBundleBase


case class FSMSrcBundle(
  rs1: String = "h0",
  vs1_preg_idx: Seq[Int] = Seq(),
  vs2_preg_idx: Seq[Int] = Seq(),
  old_vd_preg_idx: Seq[Int] = Seq(),
  mask_preg_idx: Int = 0,
  uop_valid: Boolean = false,
  uop_rob_flag : Boolean = false,
  uop_rob_idx: Int = 0,
  rdata: String = "h0",
  rvalid: Boolean = false,
  flush_vld: Boolean = false,
  flush_rob_flag : Boolean = false,
  flush_rob_idx: Int = 0,
)

trait BundleGenHelper {
  implicit val p = Parameters.empty.alterPartial({case VFuParamsKey => VFuParameters()
                                                  case XSCoreParamsKey => XSCoreParameters()})
  def genVFuUop(c: CtrlBundle) = {
    (new VUop).Lit(
      _.ctrl -> (new VUopCtrl).Lit(
        _.funct6 -> BitPat.bitPatToUInt(c.instrn(31, 26)),
        _.funct3 -> BitPat.bitPatToUInt(c.instrn(14, 12)),
        _.vm -> c.vm.B,
        _.vs1_imm -> c.vs1_imm.U,
        _.widen -> c.widen.B,
        _.widen2 -> c.widen2.B,
        _.narrow -> c.narrow.B,
        _.narrow_to_1 -> c.narrow_to_1.B
      ),
      _.info -> (new VUopInfo).Lit(
        _.ma -> c.ma.B,
        _.ta -> c.ta.B,
        _.vsew -> c.vsew.U,
        _.vlmul -> c.vlmul.U,
        _.vl -> c.vl.U,
        _.vstart -> c.vstart.U,
        _.vxrm -> c.vxrm.U,
        _.frm -> c.frm.U
      ),
    _.uopIdx -> c.uopIdx.U,
    _.uopEnd -> c.uopEnd.B,
    _.sysUop -> (new MicroOp).Lit(
      _.robIdx -> (new RobPtr).Lit(_.flag -> c.robIdx._1.B, _.value -> c.robIdx._2.U)
    ),
    )
  }

  def genVFuInput(s: SrcBundle, c: CtrlBundle) = {
    (new VFuInput).Lit(
      _.uop -> genVFuUop(c),
      _.vs1 -> s.vs1.U,
      _.vs2 -> s.vs2.U,
      _.oldVd -> s.old_vd.U,
      _.mask -> s.mask.U,
      _.rs1 -> s.rs1.U
    )
  }

  def genVAluOutput(vd: String, vxsat: Boolean = false) = {
    (new VAluOutput).Lit(
      _.vd -> vd.U(128.W),
      _.vxsat -> vxsat.B
    )
  }

  def genVFpuInput(s: SrcBundle, c: CtrlBundle,
                   redirect: (Boolean, Boolean, Int) = (false, false, 0)) = {
    (new VFpuInput).Lit(
      _.in -> genVFuInput(s, c),
      _.redirect -> (ValidUndirectioned(new Redirect)).Lit(
        _.valid -> redirect._1.B,
        _.bits -> (new Redirect).Lit(
          _.robIdx -> (new RobPtr).Lit(
            _.flag -> redirect._2.B,
            _.value -> redirect._3.U
          )
        )
      )
    )
  }

  def genFSMRedirect(redirect: (Boolean, Boolean, Int) = (false, false, 0)) = {
    (ValidIO(new Redirect)).Lit(
      _.valid -> redirect._1.B,
      _.bits -> (new Redirect).Lit(
        _.robIdx -> (new RobPtr).Lit(
          _.flag -> redirect._2.B,
          _.value -> redirect._3.U
        )
      )
    )
  }

  def accessSeq(seq: Seq[Int], idx: Int) : Int = {
    if(idx < seq.length) {
      return seq(idx)
    }
    return 0
  }

  def genFSMInput(s: FSMSrcBundle, c: CtrlBundle) = {
    (new VPermInput).Lit(
      _.uop -> genVFuUop(c),
      _.rs1 -> s.rs1.U(64.W),

      _.vs1_preg_idx(0) -> accessSeq(s.vs1_preg_idx, 0).U(8.W),
      _.vs1_preg_idx(1) -> accessSeq(s.vs1_preg_idx, 1).U(8.W),
      _.vs1_preg_idx(2) -> accessSeq(s.vs1_preg_idx, 2).U(8.W),
      _.vs1_preg_idx(3) -> accessSeq(s.vs1_preg_idx, 3).U(8.W),
      _.vs1_preg_idx(4) -> accessSeq(s.vs1_preg_idx, 4).U(8.W),
      _.vs1_preg_idx(5) -> accessSeq(s.vs1_preg_idx, 5).U(8.W),
      _.vs1_preg_idx(6) -> accessSeq(s.vs1_preg_idx, 6).U(8.W),
      _.vs1_preg_idx(7) -> accessSeq(s.vs1_preg_idx, 7).U(8.W),

      _.vs2_preg_idx(0) -> accessSeq(s.vs2_preg_idx, 0).U(8.W),
      _.vs2_preg_idx(1) -> accessSeq(s.vs2_preg_idx, 1).U(8.W),
      _.vs2_preg_idx(2) -> accessSeq(s.vs2_preg_idx, 2).U(8.W),
      _.vs2_preg_idx(3) -> accessSeq(s.vs2_preg_idx, 3).U(8.W),
      _.vs2_preg_idx(4) -> accessSeq(s.vs2_preg_idx, 4).U(8.W),
      _.vs2_preg_idx(5) -> accessSeq(s.vs2_preg_idx, 5).U(8.W),
      _.vs2_preg_idx(6) -> accessSeq(s.vs2_preg_idx, 6).U(8.W),
      _.vs2_preg_idx(7) -> accessSeq(s.vs2_preg_idx, 7).U(8.W),

      _.old_vd_preg_idx(0) -> accessSeq(s.old_vd_preg_idx, 0).U(8.W),
      _.old_vd_preg_idx(1) -> accessSeq(s.old_vd_preg_idx, 1).U(8.W),
      _.old_vd_preg_idx(2) -> accessSeq(s.old_vd_preg_idx, 2).U(8.W),
      _.old_vd_preg_idx(3) -> accessSeq(s.old_vd_preg_idx, 3).U(8.W),
      _.old_vd_preg_idx(4) -> accessSeq(s.old_vd_preg_idx, 4).U(8.W),
      _.old_vd_preg_idx(5) -> accessSeq(s.old_vd_preg_idx, 5).U(8.W),
      _.old_vd_preg_idx(6) -> accessSeq(s.old_vd_preg_idx, 6).U(8.W),
      _.old_vd_preg_idx(7) -> accessSeq(s.old_vd_preg_idx, 7).U(8.W),

      _.mask_preg_idx -> s.mask_preg_idx.U(8.W),
      _.uop_valid -> s.uop_valid.B,
      // _.uop_rob_flag -> s.uop_rob_flag.B,
      // _.uop_rob_idx -> s.uop_rob_idx.U(8.W),
      _.rdata -> s.rdata.U(128.W),
      _.rvalid -> s.rvalid.B,
      // _.flush_vld -> s.flush_vld.B,
      // _.flush_rob_flag -> s.flush_rob_flag.B,
      // _.flush_rob_idx -> s.flush_rob_idx.U(8.W),
    )
  }
}


object TestHarnessAlu {
  def test_init(dut: VAluWrapper): Unit = {
    dut.clock.setTimeout(2000)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)
  }
}

object TestHarnessMac {
  def test_init(dut: VMacWrapper): Unit = {
    dut.clock.setTimeout(2000)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)
  }
}

object TestHarnessMask {
  def test_init(dut: VMask): Unit = {
    /*dut.clock.setTimeout(2000)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)*/
  }
}

object TestHarnessFPU {
  def test_init(dut: VFPUWrapper): Unit = {
    dut.clock.setTimeout(2000)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)
  }
}

object TestHarnessDiv {
  def test_init(dut: VDivWrapper): Unit = {
    dut.clock.setTimeout(1000)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)
  }
}




object TestHarnessFSM {
  def test_init(dut:Permutation): Unit = {
    
  }
}

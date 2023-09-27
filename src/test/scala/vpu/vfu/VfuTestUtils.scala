package darecreek.vfutest

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import darecreek.exu.vfu._
import darecreek.exu.vfu.div._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._
import xiangshan.backend.rob.RobPtr
import darecreek.vfutest.alu._
import darecreek.vfutest.mac._
import darecreek.vfutest.div._

case class SrcBundle(vs2: String = "h0",
                     vs1: String = "h0",
                     old_vd: String = "h0",
                     mask: String = "hffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
                     rs1: String = "h0"
)

case class CtrlBundle(instrn: BitPat,
                      vm: Boolean = true,
                      vs1_imm: Int = 0,
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
                      uopEnd: Boolean = false,
                      robIdx: (Boolean, Int) = (false, 0)
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

  def genVAluOutput(vd: String, vxsat: Boolean = false) = {
    (new VAluOutput).Lit(
      _.vd -> vd.U(128.W),
      _.vxsat -> vxsat.B
    )
  }

  def genVFpuOutput(c: CtrlBundle, vd: String, fflags: Int = 0) = {
    (new VFpuOutput).Lit(
      _.uop -> genVFuUop(c),
      _.vd -> vd.U(128.W),
      _.fflags -> fflags.U
    )
  }
}


// object TestHarnessAlu {
//   def test_init(dut: VAluWrapper): Unit = {
//     dut.clock.setTimeout(2000)
//     dut.io.in.initSource()
//     dut.io.in.setSourceClock(dut.clock)
//     dut.io.out.initSink()
//     dut.io.out.setSinkClock(dut.clock)
//     dut.io.out.ready.poke(true.B)
//   }
// }

// object TestHarnessMac {
//   def test_init(dut: VMacWrapper): Unit = {
//     dut.clock.setTimeout(2000)
//     dut.io.in.initSource()
//     dut.io.in.setSourceClock(dut.clock)
//     dut.io.out.initSink()
//     dut.io.out.setSinkClock(dut.clock)
//     dut.io.out.ready.poke(true.B)
//   }
// }

// object TestHarnessDiv {
//   def test_init(dut: VDivWrapper): Unit = {
//     dut.clock.setTimeout(1000)
//     dut.io.in.initSource()
//     dut.io.in.setSourceClock(dut.clock)
//     dut.io.out.initSink()
//     dut.io.out.setSinkClock(dut.clock)
//     dut.io.out.ready.poke(true.B)
//   }
// }
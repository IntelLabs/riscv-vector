package vfu

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import yunsuan.vector._
import vfu.alu.{VIAluWrapper}

case class SrcBundle(vs2: String = "h0",
                     vs1: String = "h0",
                     old_vd: String = "h0",
                     mask: String = "hffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
)

case class CtrlBundle(vdType: Int = 0,
                      srcTypeVs2: Int = 0,
                      srcTypeVs1: Int = 0,
                      opcode: Int = 0,
                      vm: Boolean = true,
                      ma: Boolean = true,
                      ta: Boolean = true,
                      vlmul: Int = 0,
                      vl: Int = 0,
                      uopIdx: Int = 0,
                      vxrm : Int = 0,
)

trait BundleGenHelper {

  def genVIFuInfo(c: CtrlBundle) = {
    (new VIFuInfo).Lit(
      _.vm -> c.vm.B,
      _.ma -> c.ma.B,
      _.ta -> c.ta.B,
      _.vlmul -> c.vlmul.U,
      _.vl -> c.vl.U,
      _.uopIdx -> c.uopIdx.U,
      _.vxrm -> c.vxrm.U,
    )
  }

  def genVAluInput(s: SrcBundle, c: CtrlBundle) = {
    (new VIFuInput).Lit(
      _.opcode -> c.opcode.U,
      _.info -> genVIFuInfo(c),
      _.srcType -> Vec.Lit(c.srcTypeVs2.U(4.W), c.srcTypeVs1.U(4.W)),
      _.vdType -> c.vdType.U,
      _.vs1 -> s.vs1.U(128.W),
      _.vs2 -> s.vs2.U(128.W),
      _.old_vd -> s.old_vd.U(128.W),
      _.mask -> s.mask.U(128.W),
    )
  }

  def genVAluOutput(vd: String, vxsat: Boolean = false) = {
    (new VIFuOutput).Lit(
      _.vd -> vd.U(128.W),
      _.vxsat -> vxsat.B
    )
  }

}

object dataType {
  val u8  = 0
  val u16 = 1
  val u32 = 2
  val u64 = 3
  val s8  = 4
  val s16 = 5
  val s32 = 6
  val s64 = 7
  val f16 = 9
  val f32 = 10
  val f64 = 11
  val mask = 15
}

object TestHarnessAlu {
  def test_init(dut: VIAluWrapper): Unit = {
    dut.clock.setTimeout(2000)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)
  }
}
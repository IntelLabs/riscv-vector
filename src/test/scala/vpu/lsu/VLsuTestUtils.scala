package smartVector.lsutest

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import smartVector._

object test_init {
  def apply(dut: SmartVectorLsuTestWrapper): Unit = {
    dut.clock.setTimeout(0)
    dut.io.mUop.valid.poke(false.B)
    dut.io.mUop.bits.oldVd.poke(0.U)
  }
}
object next_is_load_and_step {
  def apply(dut: SmartVectorLsuTestWrapper): Unit = {
    dut.io.mUop.valid.poke(false.B)
    dut.io.mUop.bits.oldVd.poke(0.U)
    dut.clock.step(1)
  }
}

case class CtrlBundle(instrn: BitPat,
                      isLoad: Boolean = true,
                      vm: Boolean = true,
                      ma: Boolean = false,
                      ta: Boolean = true,
                      vsew: Int = 0,
                      vlmul: Int = 0,
                      vl: Int = 32,
                      vstart: Int = 0,
                      uopIdx: Int = 0,
                      uopEnd: Boolean = false
)

case class SrcBundleLd(scalar_opnd_1: String = "h1000",
                       scalar_opnd_2: String = "h0",
                       vs1: String = "hc0bfbebdbcbbbab9_b8b7b6b5b4b3b2b1",
                       vs2: String = "h0000000000000000_0000000000000000",
                       oldVd: String = "h201f1e1d1c1b1a19_1817161514131211",
                    //    mask: String = "hffff_ffff_ffff_ffff",
)

case class SeqId(v_reg: Int = 0,
                 el_id: Int = 0,
                 el_off: Int = 0,
                 el_count: Int = 32,
                 sb_id: Int = 0) {
  // def lookAsUInt = Cat(sb_id.U(5.W), el_count.U(7.W), el_off.U(6.W), el_id.U(11.W), v_reg.U(5.W))
  def asUInt = ((((((((sb_id << 7) + el_count) << 6) + el_off) << 11) + el_id) << 5) + v_reg).U
}
                

trait BundleGenHelper {
  def genUop(c: CtrlBundle) = {
    (new VUopTest).Lit(
      _.ctrl_funct6 -> {if (c.instrn(31, 29).equals(BitPat("b???"))) {
                          BitPat.bitPatToUInt(c.instrn(28, 26))
                        } else {
                          BitPat.bitPatToUInt(c.instrn(31, 26))
                        }},
      _.ctrl_funct3 -> BitPat.bitPatToUInt(c.instrn(14, 12)),
      _.ctrl_load -> c.isLoad.B,
      _.ctrl_store -> (!c.isLoad).B,
      _.ctrl_vm -> c.vm.B,
      _.info_ma -> c.ma.B,
      _.info_ta -> c.ta.B,
      _.info_vsew -> c.vsew.U,
      _.info_vlmul -> c.vlmul.U,
      _.info_vl -> c.vl.U,
      _.info_vstart -> c.vstart.U,
      _.splitUopIdx -> c.uopIdx.U,
      _.splitUopEnd -> c.uopEnd.B,
    )
  }

  def genLdInput(c: CtrlBundle, s: SrcBundleLd) = {
    (new MuopTest).Lit(
        _.uop -> genUop(c),
        _.scalar_opnd_1 -> s.scalar_opnd_1.U,
        _.scalar_opnd_2 -> s.scalar_opnd_2.U,
        _.vs1 -> s.vs1.U,
        _.vs2 -> s.vs2.U,
        _.oldVd -> s.oldVd.U,
    )
  }
}
package darecreek.lsutest

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import darecreek._
import darecreek.lsu._

object test_init {
  def apply(dut: VLsuTestWrapper): Unit = {
    dut.clock.setTimeout(200)
    dut.io.fromIQ.ld.valid.poke(false.B)
    dut.io.fromIQ.st.valid.poke(false.B)
    dut.io.fromIQ.ld.bits.iqEmpty.poke(true.B)
    dut.io.fromIQ.st.bits.iqEmpty.poke(true.B) 
    dut.io.ovi_memop.sync_end.poke(false.B)
    dut.io.ovi_memop.vstart_vlfof.poke(0.U)
    dut.io.ovi_load.valid.poke(false.B)
    dut.io.ovi_load.mask_valid.poke(false.B)
    dut.io.ovi_store.credit.poke(false.B)
    dut.io.ovi_maskIdx.credit.poke(false.B)
  }
}
object next_is_load_and_step {
  def apply(dut: VLsuTestWrapper): Unit = {
    dut.io.fromIQ.ld.bits.iqEmpty.poke(false.B)
    dut.io.fromIQ.st.bits.iqEmpty.poke(true.B) 
    dut.io.fromIQ.ld.valid.poke(false.B)
    dut.io.fromIQ.st.valid.poke(false.B)
    dut.clock.step(1)
  }
}
object next_is_store_and_step {
  def apply(dut: VLsuTestWrapper): Unit = {
    dut.io.fromIQ.st.bits.iqEmpty.poke(false.B) 
    dut.io.fromIQ.ld.bits.iqEmpty.poke(true.B)
    dut.io.fromIQ.ld.valid.poke(false.B)
    dut.io.fromIQ.st.valid.poke(false.B)
    dut.clock.step(1)
  }
}
object one_512b_load_resp {
  def apply(dut: VLsuTestWrapper, data: String, seqId: UInt): Unit = {
    dut.io.ovi_memop.sync_end.poke(false.B)
    dut.io.ovi_load.valid.poke(true.B)
    dut.io.ovi_load.seq_id.poke(seqId)
    dut.io.ovi_load.data.poke(data.U)
    dut.io.ovi_load.mask_valid.poke(false.B)
    dut.io.ovi_store.credit.poke(false.B)
    dut.io.ovi_maskIdx.credit.poke(false.B)
  }
}

case class CtrlBundle(instrn: BitPat,
                      isLoad: Boolean = true,
                      needOldVd: Boolean = false,
                      vm: Boolean = true,
                      ma: Boolean = false,
                      ta: Boolean = true,
                      vsew: Int = 0,
                      vlmul: Int = 0,
                      vl: Int = 32,
                      vstart: Int = 0,
                      uopIdx: Int = 0,
                      uopEnd: Boolean = false,
                      robIdx: (Boolean, Int) = (false, 0)
)

case class SrcBundleLd(rs2: String = "h0",
                       vs2: String = "h0",
                       oldVd: String = "h0",
                       mask: String = "hffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
                       nextVRobIdx: (Boolean, Int) = (false, 0),
                       iqEmpty: Boolean = false
)

case class SrcBundleSt(rs2: String = "h0",
                       vs2: String = "h0",
                       vs3: String = "h0",
                       mask: String = "hffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
                       nextVRobIdx: (Boolean, Int) = (false, 0),
                       iqEmpty: Boolean = false
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
    (new VExpdUOpTest).Lit(
      _.ctrl_funct6 -> {if (c.instrn(31, 29).equals(BitPat("b???"))) {
                          BitPat.bitPatToUInt(c.instrn(28, 26))
                        } else {
                          BitPat.bitPatToUInt(c.instrn(31, 26))
                        }},
      _.ctrl_funct3 -> BitPat.bitPatToUInt(c.instrn(14, 12)),
      _.ctrl_lsrc_1 -> {if (c.instrn(24, 20).equals(BitPat("b?????"))) {
                          0.U,
                        } else {
                          BitPat.bitPatToUInt(c.instrn(24, 20))
                        }},
      _.ctrl_lsrcVal_2 -> c.needOldVd.B,
      _.ctrl_load -> c.isLoad.B,
      _.ctrl_store -> (!c.isLoad).B,
      _.pdestVal -> c.isLoad.B,
      _.ctrl_vm -> c.vm.B,
      _.info_ma -> c.ma.B,
      _.info_vsew -> c.vsew.U,
      _.info_vlmul -> c.vlmul.U,
      _.info_vl -> c.vl.U,
      _.info_vstart -> c.vstart.U,
      _.expdIdx -> c.uopIdx.U,
      _.expdEnd -> c.uopEnd.B,
    )
  }

  def genLdInput(c: CtrlBundle, s: SrcBundleLd) = {
    (new VLdInputTest).Lit(
      _.uop -> genUop(c),
      _.rs2 -> s.rs2.U,
      _.vs2 -> s.vs2.U,
      _.oldVd -> s.oldVd.U,
      _.vmask -> s.mask.U,
      _.nextVRobIdx -> (new VRobPtr).Lit(
          _.flag -> s.nextVRobIdx._1.B,
          _.value -> s.nextVRobIdx._2.U,
        ),
      _.iqEmpty -> s.iqEmpty.B
    )
  }
  def genStInput(c: CtrlBundle, s: SrcBundleSt) = {
    (new VStInputTest).Lit(
      _.uop -> genUop(c),
      _.rs2 -> s.rs2.U,
      _.vs2 -> s.vs2.U,
      _.vs3 -> s.vs3.U,
      _.vmask -> s.mask.U,
      _.nextVRobIdx -> (new VRobPtr).Lit(
          _.flag -> s.nextVRobIdx._1.B,
          _.value -> s.nextVRobIdx._2.U,
        ),
      _.iqEmpty -> s.iqEmpty.B
    )
  }


}
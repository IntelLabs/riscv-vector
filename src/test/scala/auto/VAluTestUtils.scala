
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
import scala.util._
import xiangshan.backend.rob.RobPtr


object UtilFuncs {
  //string format convert
  def stringconvert(str:String) = {
    val stringpart = str.trim.split("0x")
    "h"+stringpart(1)
  }

  def removespace(str:String) = {
    val stringtemp = str.split(" ")
    var stringout = ""
    for(i <- 0 until stringtemp.length){
      stringout = stringout + stringtemp(i)
    }
    stringout
  }

  def multilmuldatahandle(str:String) = {
    val stringtemp = str.split(" ")
    var stringout = stringtemp
    for(i <- 0 until stringtemp.length/2){
      stringout(i) = "h" + stringtemp(i*2) + stringtemp(i*2+1)
    }
    val temp = stringout(0).split("0x")
    stringout(0) = "h" + temp(1)
    stringout
  }

  def lmulconvert(lmul:String) = {
    var lmulreg = 0
    if(lmul=="0.125000")
       lmulreg = 5
    if(lmul=="0.250000")
       lmulreg = 6
    if(lmul=="0.500000")
       lmulreg = 7
    if(lmul=="1.000000")
       lmulreg = 0
    if(lmul=="2.000000")
       lmulreg = 1
    if(lmul=="4.000000")
       lmulreg = 2
    if(lmul=="8.000000")
       lmulreg = 3
    lmulreg
  }

  def vsewconvert(sew:String) : Int = {
    sew match {
      case "8" => return 0
      case "16" => return 1
      case "32" => return 2
      case "64" => return 3
    }
  }

  def vlremaincal(vl:String,num:Int,sew:Int) = {
    var vlremain = vl.toInt
    //println("vl",vlremain)
    vlremain = vlremain - (128/sew)*num
    if(vlremain < 0)
      vlremain = 0
    vlremain
  }
}

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
                      var robIdx: (Boolean, Int) = (false, 0),
) extends TestCtrlBundleBase


case class FSMSrcBundle(
  var rs1: String = "h0",
  var vs1_preg_idx: Seq[Int] = Seq(),
  var vs2_preg_idx: Seq[Int] = Seq(),
  var old_vd_preg_idx: Seq[Int] = Seq(),
  var mask_preg_idx: Int = 0,
  var uop_valid: Boolean = false,
  var uop_rob_flag : Boolean = false,
  var uop_rob_idx: Int = 0,
  var rdata: String = "h0",
  var rvalid: Boolean = false,
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

  /*def genDontCareVFuUop(c: CtrlBundle) = {
    var vuop = Wire(new VUop)
    var vuopCtrl = Wire(new VUopCtrl)
    var vuopInfo = Wire(new VUopInfo)


    
    vuopCtrl.funct6 := DontCare
    vuopCtrl.funct3 := DontCare
    vuopCtrl.vm := DontCare
    vuopCtrl.vs1_imm := DontCare
    vuopCtrl.widen := DontCare
    vuopCtrl.widen2 := DontCare
    vuopCtrl.narrow := DontCare
    vuopCtrl.narrow_to_1 := DontCare


      
    vuopInfo.ma := DontCare
    vuopInfo.ta := DontCare
    vuopInfo.vsew := DontCare
    vuopInfo.vlmul := DontCare
    vuopInfo.vl := DontCare
    vuopInfo.vstart := DontCare
    vuopInfo.vxrm := DontCare
    vuopInfo.frm := DontCare
      
    vuop.uopIdx := DontCare
    vuop.uopEnd := DontCare


    var microOp = Wire(new MicroOp)

    var robPtr = Wire(new RobPtr)
    robPtr.flag := c.robIdx._1.B
    robPtr.value := c.robIdx._2.U

    microOp.robIdx := robPtr
    vuop.sysUop := microOp
    vuop.ctrl := vuopCtrl
    vuop.info := vuopInfo

    vuop
    
    // (new RobPtr).Lit(_.flag -> c.robIdx._1.B, _.value -> c.robIdx._2.U)
  }*/

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

  /*def genDontCareVFuInput(s: SrcBundle, c: CtrlBundle) = {
    var res = Wire(new VFuInput)
    res.uop := genDontCareVFuUop(c)
    res.vs1 := DontCare
    res.vs2 := DontCare
    res.oldVd := DontCare
    res.mask := DontCare
    res.rs1 := DontCare

    res
  }*/

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

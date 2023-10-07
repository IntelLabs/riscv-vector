package darecreek

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import darecreek.exu.fp._
import chisel3.util._
//import vpu.fp.{TestHarness}
import darecreek.exu.fp.fudian.{FMULToFADD, FMULToFADD_fflags, FloatPoint}

case class VSrcBundle(vs1: String = "h0",
                     vs2: String = "h0",
                     old_vd: String = "h0",
                     rs1: String = "h0",
                     mask: String = "h0"
                     )

case class CtrlBundle(
                    tail: Int = 0,
                    lsrc0: Int = 5,
                    lsrc1: Int = 0,
                    ldest: Int = 0,
                    vm: Boolean = true,
                    funct6: Int = 0,
                    funct3: Int = 0,
                    illegal: Boolean = false,
                    lsrcVal0: Boolean = true,
                    lsrcVal1: Boolean = true,
                    lsrcVal2: Boolean = false,
                    ldestVal: Boolean = true,
                    rdVal: Boolean = false,
                    //ldst: Int = 0,
                    arith: Boolean = true,
                    crosslane: Boolean = false,
                    alu: Boolean = true,
                    mul: Boolean = false,
                    fp: Boolean = false,
                    div: Boolean = false,
                    fixp: Boolean = false,
                    redu: Boolean = false,
                    mask: Boolean = false,
                    perm: Boolean = false,
                    widen: Boolean = false,
                    widen2: Boolean = false,
                    narrow: Boolean = false,
                    narrow_to_1: Boolean = false,
                    vstart: Int = 0,
                    vl: Int = 0,
                    vxrm: Int = 0,
                    frm: Int = 0,
                    vlmul: Int = 0,
                    vsew: Int = 0,
                    vill:Boolean = false,
                    ma:Boolean = false,
                    ta:Boolean = false,
                    destEew: Int = 0,
                    sb_id: Int = 0,
                    vRobIdxflag: Boolean = false,
                    vRobIdxvalue: Int = 0,
                    expdlen: Int = 0,
                    expdIdx: Int = 0,
                    expdEnd: Boolean = true,
                    //lsrcExpd: Int = 0,
                    ldestExpd: Int = 0,
                    //psrc: Int = 0,
                    pdest: Int = 0,
                    pdestVal: Boolean = true
                    //psrcVal
)

trait BundleChiselverifyGen {
  implicit val p: Parameters = (new WithVFPUConfig).toInstance
    def genInput(s: VSrcBundle, c: CtrlBundle): LaneFUInput = {
    (new LaneFUInput).Lit(
      _.uop -> genVExpdUOp(c),
      _.vs1 -> s.vs1.U(64.W),
      _.vs2 -> s.vs2.U(64.W),
      _.old_vd -> s.old_vd.U(64.W),
      _.rs1 -> s.rs1.U(64.W),
      _.prestart -> 0.U,
      _.mask -> s.mask.U,
      _.tail -> c.tail.U,
    )
  }
 
  def emptyVRobPtr(c: CtrlBundle): VRobPtr = {
    (new VRobPtr).Lit(
      _.flag -> c.vRobIdxflag.B,
      _.value -> c.vRobIdxvalue.U,
    )
  }
  def genVExpdUOp(c: CtrlBundle): VExpdUOp = {
    (new VExpdUOp).Lit(
      // VexpOp
      _.expdLen -> c.expdlen.U,
      _.expdIdx -> c.expdIdx.U,
      _.expdEnd -> c.expdEnd.B,
      _.lsrcExpd -> Vec.Lit(0.U(5.W), 0.U(5.W)),
      _.ldestExpd -> c.ldestExpd.U,
      _.psrc -> Vec.Lit(0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W)),
      _.pdest -> c.pdest.U,
      _.psrcVal -> Vec.Lit(false.B, false.B, false.B, false.B),
      _.pdestVal -> c.pdestVal.B,
      _.sb_id -> c.sb_id.U,
      _.vRobIdx -> emptyVRobPtr(c),
      _.ctrl -> VCtrlGen(c),
      _.info -> VInfoGen(c),
    )
  }
  def VInfoGen(c: CtrlBundle): VInfo = {
    (new VInfo).Lit(
      _.vstart -> 0.U,
      _.vl -> (log2Up(VLEN)+1).U,
      _.vxrm -> 0.U,
      _.frm -> 0.U(3.W),
      _.vlmul -> c.vlmul.U(3.W),
      _.vsew -> c.vsew.U,
      _.vill -> c.vill.B,
      _.ma -> c.ma.B,
      _.ta -> c.ta.B,
      _.destEew -> gendestEew(c)//c.vsew.U//0.U, why gendestEew not work for FP
    )
  }

  def gendestEew(c: CtrlBundle): UInt = {
     if((c.widen | c.widen2) == true){
        return (c.vsew + 1).U
     }
     else{ if(c.narrow == true){
        return (c.vsew - 1).U
     }
     else {
      if(c.narrow_to_1 == true){
        return c.vsew.U
     }else return c.vsew.U}}
  }

  def VCtrlGen(c: CtrlBundle): VCtrl = {
    (new VCtrl).Lit(
      _.lsrc -> Vec.Lit(c.lsrc0.U(5.W), c.lsrc1.U(5.W)),
      _.ldest -> c.ldest.U(5.W),
      _.vm -> c.vm.B,
      _.funct6 -> c.funct6.U(6.W),
      _.funct3 -> c.funct3.U(3.W),

      _.illegal -> c.illegal.B,
      _.lsrcVal -> Vec.Lit(c.lsrcVal0.B, c.lsrcVal1.B, c.lsrcVal2.B),
      _.ldestVal -> c.ldestVal.B,
      _.rdVal -> c.rdVal.B,
      _.load -> false.B,
      _.store -> false.B,
      _.arith -> c.arith.B,
      _.crossLane -> c.crosslane.B,
      _.alu -> c.alu.B,
      _.mul -> c.mul.B,
      _.fp -> c.fp.B,
      _.div -> c.div.B,
      _.fixP -> c.fixp.B,
      _.mask -> c.mask.B,
      _.redu -> c.redu.B,
      _.perm -> c.perm.B,
      _.widen -> c.widen.B,
      _.widen2 -> c.widen2.B,
      _.narrow -> c.narrow.B,
      _.narrow_to_1 -> c.narrow_to_1.B,
    )
  }
}
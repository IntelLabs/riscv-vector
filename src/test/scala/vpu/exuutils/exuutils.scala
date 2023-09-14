package darecreek

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import darecreek.exu.fp._
//import vpu.fp.{TestHarness}
import darecreek.exu.fp.fudian.{FMULToFADD, FMULToFADD_fflags, FloatPoint}

case class SrcBundle(vs1: String = "h0",
                     vs2: String = "h0",
                     rs1: String = "h0",
                     vd: String = "h0",
                     mask: String = "h0",
                     cvs1: String = "h0",
                     cvs2: String = "h0",
                     cvs11: String = "h0",
                     cvs12: String = "h0",
                     cvs13: String = "h0",
                     cvs21: String = "h0",
                     cvs22: String = "h0",
                     cvs23: String = "h0",
                     crs1: String = "h0",
                     cvd: String = "h0",
                     cmask: String = "h0",
                     imm: String = "h0")

case class VCtrlBundle(vsew: Int = 3, // default 64b
                       // inst
                       funct6: Int = 0,
                       vm: Boolean = true, // default  not mask
                       lsrc1: Int = 0, // vs2 addr
                       lsrc0: Int = 0, // vs1 addr, 5bits
                       funct3: Int = 1, // default VV,  5 for VX
                       ldest: Int = 0,
                       // decoded signal
                       isFp: Boolean = false,
                       wideningDst: Boolean = false,
                       wideningSrc2: Boolean = false,
                       narrowDst: Boolean = false,
                       // other
                       narrow_to_1: Boolean = false,
                       expdIdx: Int = 0,
                       expdLen: Int = 1,
                       //old_vd: Int = 0,
                       vlRemain: Int = 0,
                       //rdVal: Boolean = false,
                       crosslane: Boolean = false,
                       expdEnd: Boolean = false,
                       perm: Boolean = false,
                       vl: Int = 0,
                       vlmul: Int = 1,

                       // fp specific control, do not change
                       frm: Int = 0,
                       typeTag: Int = 1, // default 64b
                       fmaWidenEnd: Boolean = false,
                       fmaCmd: Int = 0, // fma specific
                       negVs1: Boolean = false,
                       negVs2: Boolean = false,
                       negVd: Boolean = false,
                       switchVdVs2: Boolean = false,
                       isCvt: Boolean = false, // cvt specific
                       cvtSigned: Boolean = false,
                       cvtCmd: Int = 0, // not cvt
                       cvtRm: Int = 0,
                       isMisc: Boolean = false,
                       miscCmd: Int = 0,
                       miscSubCmd: Int = 0,
                       isRec7: Boolean = false,
                       isRecSqrt7: Boolean = false,
                       isDivSqrt: Boolean = false,
                       isSqrt: Boolean = false,
                       divReverse: Boolean = false,
                      )

trait BundleGenHelper {
  implicit val p: Parameters = (new WithVFPUConfig).toInstance

  def emptyFMULToFADD(expWidth: Int, precision: Int): FMULToFADD = {
    new FMULToFADD(expWidth, precision).Lit(
      _.fp_prod -> new FloatPoint(expWidth, 2 * precision).Lit(
        _.sign -> false.B,
        _.exp -> 0.U,
        _.sig -> 0.U
      ),
      _.inter_flags -> (new FMULToFADD_fflags).Lit(
        _.isNaN -> false.B,
        _.isInf -> false.B,
        _.isInv -> false.B,
        _.overflow -> false.B
      )
    )
  }

  def emptyMulToAdd: MulToAddIOVec = {
    val ret = new MulToAddIOVec(VFPU.ftypes)
    ret.Lit(
      _.uop -> emptyVFPUOp,
      _.addend -> 0.U,
      _.mulOutElmt0Vec -> ret.mulOutElmt0Vec.Lit(
        _.getElements(0) -> emptyFMULToFADD(VFPU.ftypes(0).expWidth, VFPU.ftypes(0).precision),
        _.getElements(1) -> emptyFMULToFADD(VFPU.ftypes(1).expWidth, VFPU.ftypes(1).precision)
      ),
      _.mulOutElmt1 -> emptyFMULToFADD(VFPU.ftypes(0).expWidth, VFPU.ftypes(0).precision)
    )
  }

  def emptyVExcptInfo: VExcptInfo = {
    (new VExcptInfo).Lit(
      _.excpt -> false.B,
      _.excptVec -> Vec.Lit(Seq.fill(16){false.B}:_*),
      _.excptPosition -> 0.U
    )
  }

  def emptyVFPUOp: VFPUOp = { genVFPUOp(VCtrlBundle())}

  def emptyVRobPtr: VRobPtr = {
    (new VRobPtr).Lit(
      _.flag -> false.B,
      _.value -> 0.U,
    )
  }

  def emptyVCtrl: VCtrl = { VCtrlGen(VCtrlBundle())}

  def VCtrlGen(c: VCtrlBundle): VCtrl = {
    (new VCtrl).Lit(
      _.lsrc -> Vec.Lit(c.lsrc0.U(5.W), c.lsrc1.U(5.W)),
      _.ldest -> c.ldest.U(5.W),
      _.vm -> c.vm.B,
      _.funct6 -> c.funct6.U(6.W),
      _.funct3 -> c.funct3.U(3.W),

      _.illegal -> false.B,
      _.lsrcVal -> Vec.Lit(false.B, false.B, false.B),
      _.ldestVal -> false.B,
      _.rdVal -> 0.B,//c.rdVal.B,
      // _.ldst -> (new LdstOp).Lit(
      //   _.ld -> false.B,
      //   _.st -> false.B,
      //   _.idx -> false.B,
      //   _.seg -> false.B,
      //   _.wholeReg -> false.B,
      // ),
      _.load -> false.B,
      _.store -> false.B,
      _.arith -> true.B,
      _.crossLane -> c.crosslane.B,
      _.alu -> false.B,
      _.mul -> false.B,
      _.fp -> c.isFp.B,
      _.div -> c.isDivSqrt.B,
      _.fixP -> false.B,
      _.mask -> false.B,
      _.redu -> false.B,
      _.perm -> c.perm.B,
      _.widen -> c.wideningDst.B,
      _.widen2 -> c.wideningSrc2.B,
      _.narrow -> c.narrowDst.B,
      _.narrow_to_1 -> c.narrow_to_1.B,
    )
  }

  def emptyVInfo: VInfo = {VInfoGen(VCtrlBundle())}

  def VInfoGen(c: VCtrlBundle): VInfo = {
    (new VInfo).Lit(
      _.vstart -> 0.U,
      _.vl -> 0.U,//c.vl.U,
      _.vxrm -> 0.U,
      _.frm -> c.frm.U(3.W),
      _.vlmul -> c.vlmul.U(3.W),
      _.vsew -> c.vsew.U,
      _.vill -> false.B,
      _.ma -> false.B,
      _.ta -> false.B,
     // _.lmul -> 0.U,
      _.destEew -> gendestEew(c)//c.vsew.U//0.U, why gendestEew not work for FP
//      _.wenRF -> false.B
    )
  }

  def gendestEew(c: VCtrlBundle): UInt = {
     if(c.wideningDst || c.wideningSrc2){
        return (c.vsew + 1).U
     }
     else{ if(c.narrowDst){
        return (c.vsew - 1).U
     }
     else {
      if(c.narrow_to_1){
        return c.vsew.U
     }else return c.vsew.U}}
  }

  def genVFPCtrl(p: VCtrlBundle) = {
    (new VFPUCtrlSigs).Lit(
      _.fmaCmd -> p.fmaCmd.U(2.W),
      _.negVs1 -> p.negVs1.B,
      _.negVs2 -> p.negVs2.B,
      _.negVd -> p.negVd.B,
      _.switchVdVs2 -> p.switchVdVs2.B,
      _.isCvt -> p.isCvt.B,
      _.cvtSigned -> p.cvtSigned.B,
      _.cvtCmd -> p.cvtCmd.U(3.W),
      _.cvtRm -> p.cvtRm.U(2.W),
      _.isMisc -> p.isMisc.B,
      _.miscCmd -> p.miscCmd.U(5.W),
      _.miscSubCmd -> 0.U,//p.miscSubCmd.U(3.W),
      _.isRec7 -> p.isRec7.B,
      _.isRecSqrt7 -> p.isRecSqrt7.B,
      _.isDivSqrt -> p.isDivSqrt.B,
      _.isSqrt -> p.isSqrt.B,
      _.divReverse -> p.divReverse.B
    )
  }
  // typeTag: 0 -> fp32, 1 -> fp64
  def genVFPUOp(c: VCtrlBundle) = {
    (new VFPUOp).Lit(
      // mask sigs
      _.maskKeep -> "hffffffff".U(64.W),  //  all 1: all pass
      _.maskOff -> 0.U(64.W),  // all 0: all pass
      // fp sigs
      _.typeTag -> c.typeTag.U,
      _.vfpCtrl -> genVFPCtrl(c),
      _.fWidenEnd -> c.fmaWidenEnd.B,
      // VexpOp
      _.expdLen -> c.expdLen.U,
      _.expdIdx -> c.expdIdx.U,
      _.expdEnd -> c.expdEnd.B,
      _.lsrcExpd -> Vec.Lit(0.U(5.W), 0.U(5.W)),
      _.ldestExpd -> 0.U(5.W),
      _.psrc -> Vec.Lit(0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W)),
      _.pdest -> 0.U,
      _.psrcVal -> Vec.Lit(false.B, false.B, false.B, false.B),
      _.pdestVal -> true.B,
      // VMicroOp
      _.sb_id -> 0.U,
      _.vRobIdx -> emptyVRobPtr,
      // VCtrlInfo
      _.ctrl -> VCtrlGen(c),
      _.info -> VInfoGen(c),
//      _.excptInfo -> emptyVExcptInfo
    )
  }

  // typeTag: 0 -> fp32, 1 -> fp64
  def genVExpdWithMaskUOp(c: VCtrlBundle) = {
    (new VExpdWithMaskUOp).Lit(
      // mask sigs
      _.maskKeep -> "hffffffff".U(64.W),  //  all 1: all pass
      _.maskOff -> 0.U(64.W),  // all 0: all pass
      // VexpOp
      _.expdLen -> c.expdLen.U,
      _.expdIdx -> c.expdIdx.U,
      _.expdEnd -> c.expdEnd.B,
      _.lsrcExpd -> Vec.Lit(0.U(5.W), 0.U(5.W)),
      _.ldestExpd -> 0.U(5.W),
      _.psrc -> Vec.Lit(0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W)),
      _.pdest -> 0.U,
      _.psrcVal -> Vec.Lit(false.B, false.B, false.B, false.B),
      _.pdestVal -> true.B,
      // VMicroOp
      _.sb_id -> 0.U,
      _.vRobIdx -> emptyVRobPtr,
      // VCtrlInfo
      _.ctrl -> VCtrlGen(c),
      _.info -> VInfoGen(c),
      //      _.excptInfo -> emptyVExcptInfo
    )
  }


  def genVExpdUOp(c: VCtrlBundle): VExpdUOp = {
    (new VExpdUOp).Lit(
      // VexpOp
      _.expdLen -> c.expdLen.U,
      _.expdIdx -> c.expdIdx.U,
      _.expdEnd -> c.expdEnd.B,
      _.lsrcExpd -> Vec.Lit(0.U(5.W), 0.U(5.W)),
      _.ldestExpd -> 0.U(5.W),
      _.psrc -> Vec.Lit(0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W), 0.U(VPRegIdxWidth.W)),
      _.pdest -> 0.U,
      _.psrcVal -> Vec.Lit(false.B, false.B, false.B, false.B),
      _.pdestVal -> true.B,
      // VMicroOp
      _.sb_id -> 0.U,
      _.vRobIdx -> emptyVRobPtr,
      // VCtrlInfo
      _.ctrl -> VCtrlGen(c),
      _.info -> VInfoGen(c),
//      _.excptInfo -> emptyVExcptInfo
    )
  }

  def genFUWithMaskInput(s: SrcBundle, c: VCtrlBundle): LaneFUWithMaskIn = {
    (new LaneFUWithMaskIn).Lit(
      _.uop -> genVExpdWithMaskUOp(c),
      _.vs1 -> s.vs1.U(64.W),
      _.vs2 -> s.vs2.U(64.W),
      _.old_vd -> s.vd.U(64.W),
      _.rs1 -> 0.U,
      _.prestart -> 0.U,
      _.mask -> s.mask.U,
      _.tail -> 0.U,
    )
  }

  def genFloatInput(s: SrcBundle, c: VCtrlBundle): LaneFloatFUIn = {
    (new LaneFloatFUIn).Lit(
      _.uop -> genVFPUOp(c),
      _.vs1 -> s.vs1.U(64.W),
      _.vs2 -> s.vs2.U(64.W),
      _.old_vd -> s.vd.U(64.W),
      _.rs1 -> 0.U,
      _.prestart -> 0.U,
      _.mask -> s.mask.U,
      _.tail -> 0.U,
    )
  }

  def genFUInput(s: SrcBundle, c: VCtrlBundle): LaneFUInput = {
    (new LaneFUInput).Lit(
      _.uop -> genVExpdUOp(c),
      _.vs1 -> s.vs1.U(64.W),
      _.vs2 -> s.vs2.U(64.W),
      _.old_vd -> s.vd.U(64.W),
      _.rs1 -> 0.U,
      _.prestart -> 0.U,
      _.mask -> s.mask.U,
      _.tail -> 0.U,
    )
  }

  def genVExuInput(s: SrcBundle, c: VCtrlBundle): VExuInput = {
    (new VExuInput).Lit(
      _.uop -> genVExpdUOp(c),
      _.rs1 -> s.rs1.U(256.W),
      //_.vSrc -> Vec.Lit(0.U(64.W), 0.U(64.W), 0.U(64.W), 0.U(64.W)),
      //_.vSrc -> Vec.Lit(Vec.Lit(s.cvs1.U(64.W), s.cvs2.U(64.W), s.cvd.U(64.W), s.cmask.U(64.W)),Vec.Lit(s.cvs1.U(64.W), s.cvs2.U(64.W), s.cvd.U(64.W), s.cmask.U(64.W)),Vec.Lit(s.cvs1.U(64.W), s.cvs2.U(64.W), s.cvd.U(64.W), s.cmask.U(64.W)),Vec.Lit(s.cvs1.U(64.W), s.cvs2.U(64.W), s.cvd.U(64.W), s.cmask.U(64.W))),
      _.vSrc -> Vec.Lit(Vec.Lit(s.cvs1.U(64.W),s.cvs11.U(64.W),s.cvs12.U(64.W),s.cvs13.U(64.W)),Vec.Lit(s.cvs2.U(64.W),s.cvs21.U(64.W),s.cvs22.U(64.W),s.cvs23.U(64.W)),
      Vec.Lit(s.cvd.U(64.W),s.cvd.U(64.W),s.cvd.U(64.W),s.cvd.U(64.W)),Vec.Lit(s.cmask.U(64.W),s.cmask.U(64.W),s.cmask.U(64.W),s.cmask.U(64.W))),
      _.vstartRemain -> 0.U,
      _.vlRemain -> c.vlRemain.U
    )
  }

  def genVExuOutput(rd: String,vd: String, s: SrcBundle, c: VCtrlBundle): VExuOutput = {
    (new VExuOutput).Lit(
      _.uop -> genVExpdUOp(c),
      //_.vd = Vec.Lit(),
      _.vd -> gencrosslanevdgroup(vd).vd,//Vec.Lit(0.U(LaneWidth.W),0.U(LaneWidth.W),0.U(LaneWidth.W),0.U(LaneWidth.W)),
      _.fflags -> 0.U,
      _.vxsat -> 0.U,
      _.rd ->  rd.U(xLen.W)
    )
  }

  class vdout extends Bundle {
    val vd = Vec(NLanes, UInt(LaneWidth.W))
  }

  def gencrosslanevdgroup(vd: String): vdout = {
    val laner = vd.split("")
    (new vdout).Lit(
    _.vd -> Vec.Lit(laner(0).U(LaneWidth.W),laner(1).U(LaneWidth.W),laner(2).U(LaneWidth.W),laner(0).U(LaneWidth.W))
    )
  }

  def genInput(s: SrcBundle, c: VCtrlBundle): LaneFUInput = {
    (new LaneFUInput).Lit(
      _.uop -> genVExpdUOp(c),
      _.vs1 -> s.vs1.U(64.W),
      _.vs2 -> s.vs2.U(64.W),
      _.old_vd -> s.vd.U(64.W),
      _.rs1 -> s.rs1.U(64.W),
      _.prestart -> 0.U,
      _.mask -> 0.U,
      _.tail -> 0.U,
    )
  }

  def genFUOutput(vd: String, c: VCtrlBundle, fflags: String = "b0"): LaneFUOutput = {
    (new LaneFUOutput).Lit(
      _.uop -> genVExpdUOp(c),
      _.vd -> vd.U(64.W),
      _.fflags -> fflags.U(5.W),
      _.vxsat -> false.B
    )
  }

  def genFUWithMaskOutput(vd: String, c: VCtrlBundle, fflags: String = "b0"): LaneFUWithMaskOut = {
    (new LaneFUWithMaskOut).Lit(
      _.uop -> genVExpdWithMaskUOp(c),
      _.vd -> vd.U(64.W),
      _.fflags -> fflags.U(5.W),
      _.vxsat -> false.B
    )
  }
  def genFloatOutput(vd: String, c: VCtrlBundle, fflags: String = "b0"): LaneFloatFUOut = {
    (new LaneFloatFUOut).Lit(
      _.uop -> genVFPUOp(c),
      _.vd -> vd.U(64.W),
      _.fflags -> fflags.U(5.W),
      _.vxsat -> false.B
    )
  }

  def genExuOutput(vd: String, c: VCtrlBundle, fflags: String = "b0"): LaneFUOutput = {
    (new LaneFUOutput).Lit(
      _.uop -> genVFPUOp(c),
      _.vd -> vd.U(64.W),
      _.fflags -> fflags.U(5.W),
      _.vxsat -> false.B
    )
  }

  def genFUWithMaskInputs(ops: Seq[VCtrlBundle], srcs: Seq[SrcBundle]) = {
    ops.zip(srcs).map(x => genFUWithMaskInput(x._2, x._1))
  }
  def genFUWithMaskOutputs(ops: Seq[VCtrlBundle], results: Seq[String], fflags: Seq[String]) = {
    //require(ops.length == srcs.length)
    ops.zipWithIndex.map(x => genFUWithMaskOutput(results(x._2), x._1,fflags(x._2)))
  }
  def genFUWithMaskOutputs(ops: Seq[VCtrlBundle], results: Seq[String]) = {
    //require(ops.length == srcs.length)
    ops.zipWithIndex.map(x => genFUWithMaskOutput(results(x._2), x._1, "h0"))
  }

  def genFUInputs(ops: Seq[VCtrlBundle], srcs: Seq[SrcBundle]) = {
    //require(ops.length == srcs.length)
    ops.zip(srcs).map(x => genFUInput(x._2, x._1))
  }
  def genFUOutputs(ops: Seq[VCtrlBundle], results: Seq[String], fflags: Seq[String]) = {
    //require(ops.length == srcs.length)
    ops.zipWithIndex.map(x => genFUOutput(results(x._2), x._1,fflags(x._2)))
  }
  def genFUOutputs(ops: Seq[VCtrlBundle], results: Seq[String]) = {
    //require(ops.length == srcs.length)
    ops.zipWithIndex.map(x => genFUOutput(results(x._2), x._1, "h0"))
  }

  def genFloatInputs(ops: Seq[VCtrlBundle], srcs: Seq[SrcBundle]) = {
    //require(ops.length == srcs.length)
    ops.zip(srcs).map(x => genFloatInput(x._2, x._1))
  }
  def genFloatOutputs(ops: Seq[VCtrlBundle], results: Seq[String], fflags: Seq[String]) = {
    //require(ops.length == results.length && ops.length == fflags.length)
    ops.zipWithIndex.map(x => genFloatOutput(results(x._2), x._1,fflags(x._2)))
  }

  def dupElement[A](l: Seq[A]) = l.flatMap(Seq.fill(2)(_))

}
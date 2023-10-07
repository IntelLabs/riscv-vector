package darecreek.exu.vfu.fp

import chisel3._
import chisel3.util._
// import darecreek.{BundleHelper, LaneFUInput, LaneFUOutput, VExpdUOp}
import darecreek.exu.vfu.{BundleHelper, LaneFUInput, LaneFUOutput, VExpdUOp}
import org.chipsalliance.cde.config.Parameters
import xiangshan.Redirect

class VFPUCtrlSigs extends Bundle {

  // for decode table fma
  val fmaCmd = UInt(2.W) // fmaCmd: 10->only add/sub, 01->only mul, 11->fma (both add/sub and mul)
  val negVs1 = Bool() // negate product/vs1  - xxx + xxx  // left one, a.k.a, negate vs1
  val negVs2 = Bool()
  val negVd = Bool()
  val switchVdVs2 = Bool()

  // for decode table cvt
  val isCvt = Bool()
  val cvtSigned = Bool()
  val cvtCmd = UInt(3.W) // 001(f2f), 010(f2i), 100(i2f)
  val cvtRm = UInt(2.W) // 01(rod), 10(rtz), 00(dynamic)

  // for decode table misc
  val isMisc = Bool()
  // One-hot, (classify, cmp, sgnj, minmax, merge/mv)
  val miscCmd = UInt(5.W)
  // eq 000, ne 100, le 001, lt 011, gt 101, ge 111;
  // 110 for max, 100 for min; sgnj 000, sgnjn 001, sgnjx 010;
  val miscSubCmd = UInt(3.W)

  // for rec module
  val isRec7 = Bool()
  val isRecSqrt7 = Bool()

  // for div module
  val isDivSqrt = Bool()
  val isSqrt = Bool()
  val divReverse = Bool()

}

trait HasMaskSigs {
  val maskKeep = UInt(64.W)
  val maskOff = UInt(64.W)
}

trait HasFPCtrl {
  val vfpCtrl = new VFPUCtrlSigs
  val typeTag = UInt(VFPU.typeWidth.W)
  val fWidenEnd = Bool() // is the second widen inst, for inactive element control
}

class VExpdWithMaskUOp(implicit p: Parameters) extends VExpdUOp with HasMaskSigs
class VFPUOp(implicit p: Parameters) extends VExpdWithMaskUOp with HasFPCtrl

class LaneFUWithMaskIn(implicit p: Parameters) extends LaneFUInput {
  override val uop = new VExpdWithMaskUOp
  def connectFromLaneFUInput(op: LaneFUInput): Unit = {
    op.elements.foreach{ case (name, data) =>
      data match {
        case x:Bundle  =>
          x.elements.foreach{ case (name2, data2) =>
            this.uop.elements(name2) := data2
          }
        case _ =>  this.elements(name) := data
      }
    }
  }
}

class LaneFUWithMaskOut(implicit p: Parameters) extends LaneFUOutput {
  override val uop = new VExpdWithMaskUOp
  def outputToLaneFU(op: LaneFUOutput): Unit = {
    //    println(op.elements)
    //////    println(op.elements)
    //    println(this.elements)
    //////    println(this.uop.elements)

    BundleHelper.partialConnectByName(op, this)

  }
}

class LaneFloatFUIn(implicit p: Parameters) extends LaneFUWithMaskIn {
  override val uop = new VFPUOp
}

class LaneFloatFUOut(implicit p: Parameters) extends LaneFUWithMaskOut {
  override val uop = new VFPUOp
//      data match {
//        case x:Bundle  =>
//        //          x.elements.foreach{ case (name2, data2) =>
//        //            op.uop.elements(name2) := this.uop.elements(name2)
//        //          }
//        case _ =>  op.elements(name) := this.elements(name)
//      }
//    }
//    op.elements.foreach{ case (name, data) =>
//      data match {
//        case x:Bundle  =>
////          x.elements.foreach{ case (name2, data2) =>
////            op.uop.elements(name2) := this.uop.elements(name2)
////          }
//        case _ =>  op.elements(name) := this.elements(name)
//      }
//    }

}

class LaneFloatFUIO(implicit p: Parameters) extends Bundle {
  val in = Flipped(DecoupledIO(new LaneFloatFUIn))
  val out = DecoupledIO(new LaneFloatFUOut)
  val redirect = Flipped(ValidIO(new Redirect))
}

class LaneFloatDivFUIO(implicit p: Parameters) extends Bundle {
  val in = Flipped(DecoupledIO(new LaneFloatFUIn))
  val out = DecoupledIO(new LaneFloatFUOut)
  // val redirect = Flipped(ValidIO(new Redirect))
}



//class Redirect(implicit p: Parameters) extends Bundle with HasVFPUParams {
//  val robIdx = new RobPtr
//  val ftqIdx = new FtqPtr
//  val ftqOffset = UInt(log2Up(PredictWidth).W)
//  val level = RedirectLevel()
//  val interrupt = Bool()
//  val cfiUpdate = new CfiUpdateInfo
//
//  val stFtqIdx = new FtqPtr // for load violation predict
//  val stFtqOffset = UInt(log2Up(PredictWidth).W)
//
//  val debug_runahead_checkpoint_id = UInt(64.W)
//
//  // def isUnconditional() = RedirectLevel.isUnconditional(level)
//  def flushItself() = RedirectLevel.flushItself(level)
//  // def isException() = RedirectLevel.isException(level)
//}

/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan

import chisel3._
import chisel3.util._
import xiangshan.backend.rob.RobPtr
// import xiangshan.backend.CtrlToFtqIO
// import xiangshan.backend.decode.{ImmUnion, XDecode}
// import xiangshan.vector.videcode._
// import xiangshan.mem.{LqPtr, SqPtr}
// import xiangshan.frontend.PreDecodeInfo
// import xiangshan.frontend.HasBPUParameter
// import xiangshan.frontend.AllFoldedHistories
// import xiangshan.frontend.RASEntry
// import xiangshan.frontend.BPUCtrl
// import xiangshan.frontend.FtqPtr
// import xiangshan.frontend.CGHPtr
// import xiangshan.frontend.FtqToCtrlIO
import chipsalliance.rocketchip.config.Parameters
// import chisel3.util.BitPat.bitPatToUInt
// import xiangshan.backend.execute.fu.alu.ALUOpType
// import xiangshan.backend.execute.fu.csr.CSROpType
// import xiangshan.backend.execute.fu.fpu.FPUCtrlSignals
// import xiangshan.frontend.Ftq_Redirect_SRAMEntry
// import xiangshan.frontend.AllAheadFoldedHistoryOldestBits
// import xs.utils.DataChanged

// import xiangshan.vector._
// import scala.math.max

class ValidUndirectioned[T <: Data](gen: T) extends Bundle {
  val valid = Bool()
  val bits = gen.cloneType.asInstanceOf[T]

}

object ValidUndirectioned {
  def apply[T <: Data](gen: T) = {
    new ValidUndirectioned[T](gen)
  }
}

class MicroOp(implicit p: Parameters) extends Bundle {
  val robIdx = new RobPtr
  val scalarRegWriteEn = Bool()
  val floatRegWriteEn = Bool()
  val rfWriteEn = Bool()
  val ldest = UInt(5.W)
  //For Permutation
  val permExpdLen = UInt(4.W)
  val regDstIdx = UInt(5.W)
  val regCount  = UInt(4.W)
}

class Redirect(implicit p: Parameters) extends Bundle {
  val robIdx = new RobPtr
  // val ftqIdx = new FtqPtr
  // val ftqOffset = UInt(log2Up(PredictWidth).W)
  // val level = RedirectLevel()
  // val interrupt = Bool()
  // val cfiUpdate = new CfiUpdateInfo
  // val isException = Bool()
  // val isLoadStore = Bool()
  // val isLoadLoad = Bool()
  // val isXRet = Bool()
  // val isFlushPipe = Bool()

  // val stFtqIdx = new FtqPtr // for load violation predict
  // val stFtqOffset = UInt(log2Up(PredictWidth).W)

  // // def isUnconditional() = RedirectLevel.isUnconditional(level)
  // def flushItself() = RedirectLevel.flushItself(level)
  // // def isException() = RedirectLevel.isException(level)
}
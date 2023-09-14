package darecreek.exu.fu.alu

import chisel3._
import darecreek.{VExpdUOp, SewOH}
import darecreek.DarecreekParam._

class VAluDecode extends Bundle {
  val sub = Bool()
  val misc = Bool()
  // val unsigned = Bool()
}

trait hasVAluCtrl {
  val ctrl = new VAluDecode
  val sew = new SewOH
}

class VAluInput extends Bundle with hasVAluCtrl{
  val uop = new VExpdUOp
  val vs1_rs1_imm = UInt(LaneWidth.W)
  val vs2 = UInt(LaneWidth.W)
  val vmask = UInt(8.W)
}
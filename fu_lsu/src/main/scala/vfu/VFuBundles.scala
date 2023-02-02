package yunsuan.vector

import chisel3._
import chisel3.util._

class VFuInfo extends Bundle {
  val vm = Bool()
  val ma = Bool()
  val ta = Bool()
  val lmul = UInt(3.W)
  val vl = UInt(8.W)
  val uopIdx = UInt(6.W)
}
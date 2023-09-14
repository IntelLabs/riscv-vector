// https://github.com/OpenXiangShan/fudian
package darecreek.exu.fp.fudian.utils

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

class CLZ(len: Int, zero: Boolean) extends Module {

  val inWidth = len
  val outWidth = (inWidth - 1).U.getWidth

  val io = IO(new Bundle() {
    val in = Input(UInt(inWidth.W))
    val out = Output(UInt(outWidth.W))
  })

  io.out := PriorityEncoder(io.in.asBools().reverse)
}

object CLZ {
  def apply(value: UInt): UInt = {
    val clz = Module(new CLZ(value.getWidth, true))
    clz.io.in := value
    clz.io.out
  }
  def apply(xs: Seq[Bool]): UInt = {
    apply(Cat(xs.reverse))
  }
}

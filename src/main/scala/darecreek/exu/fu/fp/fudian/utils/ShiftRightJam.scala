// https://github.com/OpenXiangShan/fudian
package darecreek.exu.fp.fudian.utils

import chisel3._
import chisel3.util._

/**
  * in => shift | collect sticky bit => {in_shifted, sticky}
  */
class ShiftRightJam(val len: Int) extends Module {
  val max_shift_width = log2Up(len + 1)
  val io = IO(new Bundle() {
    val in = Input(UInt(len.W))
    val shamt = Input(UInt())
    val out = Output(UInt(len.W))
    val sticky = Output(Bool())
  })
  val exceed_max_shift = io.shamt > len.U
  val shamt = io.shamt(max_shift_width - 1, 0)
  val sticky_mask =
    ((1.U << shamt).asUInt - 1.U)(len - 1, 0) | Fill(len, exceed_max_shift)
  io.out := Mux(exceed_max_shift, 0.U, io.in >> io.shamt)
  io.sticky := (io.in & sticky_mask).orR
}

object ShiftRightJam {
  def apply(x: UInt, shamt: UInt): (UInt, Bool) = {
    val shiftRightJam = Module(new ShiftRightJam(x.getWidth))
    shiftRightJam.io.in := x
    shiftRightJam.io.shamt := shamt
    (shiftRightJam.io.out, shiftRightJam.io.sticky)
  }
}

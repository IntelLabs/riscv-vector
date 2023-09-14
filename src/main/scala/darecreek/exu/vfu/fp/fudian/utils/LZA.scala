// https://github.com/OpenXiangShan/fudian
package darecreek.exu.vfu.fp.fudian.utils

import chisel3._
import chisel3.util._

class LzaIO(val len: Int) extends Bundle {
  val a, b = Input(UInt(len.W))
  val f = Output(UInt(len.W))
}

class LZA(len: Int) extends Module {
  val io = IO(new LzaIO(len))

  val (a, b) = (io.a, io.b)

  val p, k, f = Wire(Vec(len, Bool()))
  for (i <- 0 until len) {
    p(i) := a(i) ^ b(i)
    k(i) := (!a(i)) & (!b(i))
    if (i == 0) {
      f(i) := false.B
    } else {
      f(i) := p(i) ^ (!k(i - 1))
    }
  }
  io.f := Cat(f.reverse)
}

package darecreek.exu.vfu.fp

import chisel3.util.{Cat, Fill, log2Ceil}
import chisel3._

object VFPU {

  case class FType(expWidth: Int, precision: Int) {
    val sigWidth = precision - 1
    val len = expWidth + precision
    val bias = 1 << (expWidth - 1) - 1
  }

  val f32 = FType(8, 24) // IEEE754/2008 float(single)
  val f64 = FType(11, 53) // IEEE754/2008 double

  val ftypes = List(f32, f64)
  val typeWidth = log2Ceil(ftypes.length)

  val S = ftypes.indexOf(f32).U(typeWidth.W)
  val D = ftypes.indexOf(f64).U(typeWidth.W)

  // vsew->sew: 0->8, 1->16, 2->32, 3->64
  // sew->ftype: 32->S, 64->D
  def getTypeTagFromVSEW(vsew: UInt): UInt = {
    require(vsew.getWidth == 3)
    Mux(vsew === 2.U, S, D)
  }

  def dup32(x: UInt): UInt = {
    require(x.getWidth == 64)
    Cat(Fill(2, x.tail(32)))
  }

}

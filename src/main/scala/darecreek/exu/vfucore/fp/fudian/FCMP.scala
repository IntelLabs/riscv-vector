// https://github.com/OpenXiangShan/fudian
package darecreek.exu.vfucore.fp.fudian

import chisel3._
import chisel3.util._

class FCMP(val expWidth: Int, val precision: Int) extends Module {
  val io = IO(new Bundle() {
    val a, b = Input(UInt((expWidth + precision).W))
    val signaling = Input(Bool())
    val eq, le, lt = Output(Bool())
    val fflags = Output(UInt(5.W))
  })

  val (a, b) = (io.a, io.b)
  val fp_a = FloatPoint.fromUInt(a, expWidth, precision)
  val fp_b = FloatPoint.fromUInt(b, expWidth, precision)
  val decode_a = fp_a.decode
  val decode_b = fp_b.decode

  val hasNaN = decode_a.isNaN || decode_b.isNaN
  val hasSNaN = decode_a.isSNaN || decode_b.isSNaN
  val bothZero = decode_a.isZero && decode_b.isZero

  val same_sign = fp_a.sign === fp_b.sign
  val a_minus_b = Cat(0.U(1.W), a) - Cat(0.U(1.W), b)
  val uint_eq = a_minus_b.tail(1) === 0.U
  val uint_less = fp_a.sign ^ a_minus_b.head(1).asBool()

  val invalid = hasSNaN || (io.signaling && hasNaN)

  io.eq := !hasNaN && (uint_eq || bothZero)
  io.le := !hasNaN && Mux(
    same_sign,
    uint_less || uint_eq,
    fp_a.sign || bothZero
  )
  io.lt := !hasNaN && Mux(
    same_sign,
    uint_less && !uint_eq,
    fp_a.sign && !bothZero
  )
  io.fflags := Cat(invalid, 0.U(4.W))
}

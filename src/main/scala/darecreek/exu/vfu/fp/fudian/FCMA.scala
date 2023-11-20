// https://github.com/OpenXiangShan/fudian
package darecreek.exu.vfu.fp.fudian

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._

// Cascade FMA (a * b + c)
class FCMA(val expWidth: Int, val precision: Int) extends Module {

  val io = IO(new Bundle() {
    val a, b, c = Input(UInt((expWidth + precision).W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt((expWidth + precision).W))
    val fflags = Output(UInt(5.W))
  })

  val fmul = Module(new FMUL(expWidth, precision))
  val fadd = Module(new FCMA_ADD(expWidth, 2 * precision, precision))

  fmul.io.a := io.a
  fmul.io.b := io.b
  fmul.io.rm := io.rm

  val mul_to_fadd = fmul.io.to_fadd
  fadd.io.a := Cat(io.c, 0.U(precision.W))
  fadd.io.b := mul_to_fadd.fp_prod.asUInt
  fadd.io.b_inter_valid := true.B
  fadd.io.b_inter_flags := mul_to_fadd.inter_flags
  fadd.io.rm := io.rm

  io.result := fadd.io.result
  io.fflags := fadd.io.fflags
}

/*
object FCMA extends App {
  override def main(args: Array[String]): Unit = {
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new FCMA(11, 53))
    ))
  }
}
*/

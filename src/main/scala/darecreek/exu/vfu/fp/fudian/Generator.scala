// https://github.com/OpenXiangShan/fudian
package darecreek.exu.vfu.fp.fudian

import chisel3.RawModule
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

object Generator extends App {

  def getModuleGen(
    name:      String,
    expWidth:  Int,
    precision: Int
  ): () => RawModule = {
    val pkg = this.getClass.getPackageName
    name match {
      case "FPToFP" =>
        val (inE, inP, oE, oP) = expWidth match {
          case -1 => (8, 24, 11, 53)
          case -2 => (11, 53, 8, 24)
        }
        () => new FPToFP(inE, inP, oE, oP)
      case _ =>
        val c =
          Class
            .forName(pkg + "." + name)
            .getConstructor(Integer.TYPE, Integer.TYPE)
        () =>
          c.newInstance(
            expWidth.asInstanceOf[Object],
            precision.asInstanceOf[Object]
          ).asInstanceOf[RawModule]
    }
  }

  val (module, expWidth, precision, firrtlOpts) = ArgParser.parse(args)
  (new ChiselStage).execute(
      firrtlOpts,
      Seq(
        ChiselGeneratorAnnotation(getModuleGen(module, expWidth, precision))
      )
    )
}

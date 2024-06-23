import chisel3._
import chisel3.util._

package object coincreekDCache extends DCacheParams with Parameters_HY {

  def toInt(in: UInt): Int = {
    var out: Int = 0
    for (i <- 0 to 256)
      when(i.asUInt === in) {
        out = i
      }
    out
  }

}

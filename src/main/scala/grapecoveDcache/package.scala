import chisel3._
import chisel3.util._

package object grapecoveDCache extends DCacheParams {

  def toInt(in: UInt, range: Int = 256): Int = {
    var out: Int = 0
    for (i <- 0 to range)
      when(i.asUInt === in) {
        out = i
      }
    out
  }

}

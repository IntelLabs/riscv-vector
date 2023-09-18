package darecreek

import chisel3._
import chisel3.util._

/** Assert that when cond == true.B, statement must = true.B */
object assertWhen {
  def apply(cond: Bool, statement: Bool, str: String) = {
    assert((cond && statement) || !cond, str)
  }
}
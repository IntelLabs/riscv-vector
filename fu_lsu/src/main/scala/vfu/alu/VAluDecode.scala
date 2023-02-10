package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

object VAluMISC {
                      //         misc
                      //          |
  val default =          BitPat("b?")
  val table   = Seq(
    BitPat(0.U(6.W))  -> BitPat("b0"),
    BitPat(1.U(6.W))  -> BitPat("b1"),
    BitPat(2.U(6.W))  -> BitPat("b0"),
    BitPat(3.U(6.W))  -> BitPat("b0"),
    BitPat(4.U(6.W))  -> BitPat("b0"),
    BitPat(5.U(6.W))  -> BitPat("b0"),
    BitPat(6.U(6.W))  -> BitPat("b1"),
    BitPat(7.U(6.W))  -> BitPat("b1"),
    BitPat(8.U(6.W))  -> BitPat("b1"),
    BitPat(9.U(6.W))  -> BitPat("b1"),
    BitPat(10.U(6.W)) -> BitPat("b1"),
    BitPat(11.U(6.W)) -> BitPat("b1"),
    BitPat(12.U(6.W)) -> BitPat("b1"),
    BitPat(13.U(6.W)) -> BitPat("b1"),
    BitPat(14.U(6.W)) -> BitPat("b1"),
    BitPat(15.U(6.W)) -> BitPat("b1"),
    BitPat(16.U(6.W)) -> BitPat("b1"),
    BitPat(17.U(6.W)) -> BitPat("b0"),
    BitPat(18.U(6.W)) -> BitPat("b0"),
    BitPat(19.U(6.W)) -> BitPat("b0"),
    BitPat(20.U(6.W)) -> BitPat("b0"),
    BitPat(21.U(6.W)) -> BitPat("b0"),
    BitPat(22.U(6.W)) -> BitPat("b0"),
    BitPat(23.U(6.W)) -> BitPat("b0"),
    BitPat(24.U(6.W)) -> BitPat("b0"),
    BitPat(25.U(6.W)) -> BitPat("b0"),
    BitPat(26.U(6.W)) -> BitPat("b1"),
    BitPat(27.U(6.W)) -> BitPat("b1"),
  )
}

// object VAluDecode {
//                       //             mask
//                       //          redu|
//                       //     IntFixp| | perm
//                       //          | | | |
//   val default =          BitPat("b? ? ? ?")
//   val table   = Seq(
//     BitPat(0.U(6.W))  -> BitPat("b1 0 0 0"),
//     BitPat(1.U(6.W))  -> BitPat("b1 0 0 0"),
//   )
// }
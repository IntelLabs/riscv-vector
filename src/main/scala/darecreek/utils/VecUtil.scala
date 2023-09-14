package darecreek

import chisel3._
import chisel3.util._

object BundleHelper {
  def partialConnectByName(to: Bundle, from: Bundle):Unit = {
    to.elements.foreach{ case (name, data) =>
      data match {
        case x: Bundle => {
          from.elements(name) match {
            case y: Bundle => partialConnectByName(x, y)
          }
        }
        case _ =>
          to.elements(name) := from.elements(name)
      }
    }
  }
}

object Vlmul_to_lmul {
// vlmul --> LMUL --> max(LMUL, 1)
// Note: set result to 1 if LMUL < 1
  def apply(vlmul: UInt): UInt = {
    val y0 = !vlmul(1) && !vlmul(0) || vlmul(2)
    val y1 = !vlmul(2) && !vlmul(1) && vlmul(0)
    val y2 = !vlmul(2) && vlmul(1) && !vlmul(0)
    val y3 = !vlmul(2) && vlmul(1) && vlmul(0)
    Cat(y3, y2, y1, y0)
  }
}

object UIntSplit { // Split into elements
  // E.g., if sew=8, UInt(64.W) => Seq(UInt(8.W) * 8)
  def apply(data: UInt, sew: Int): Seq[UInt] = {
    val w = data.getWidth
    // require(w == 64 || w == 32 || w == 16 || w == 8)
    require(w >= sew && w % sew == 0)
    Seq.tabulate(w/sew)(i => data(sew*i+sew-1, sew*i))
  }
  def apply(dataVLEN: UInt): Seq[UInt] = {
    Seq.tabulate(NLanes)(i => dataVLEN(LaneWidth*i+LaneWidth-1, LaneWidth*i))
  }
}

object BitsExtend {
  def apply(data: UInt, extLen: Int, signed: Bool): UInt = {
    val width = data.getWidth
    require(width < extLen)
    Cat(Fill(extLen - width, data(width-1) && signed), data)
  }
  def vector(data: UInt, extLen: Int, signed: Bool, sew: Int): UInt = { // For extension instrn
    require(data.getWidth % sew == 0)
    val nVec = data.getWidth / sew
    require(extLen % nVec == 0)
    Cat(UIntSplit(data, sew).map(dataSplit => apply(dataSplit, extLen/nVec, signed)).reverse)
  }
}

object UIntToCont1s { // E.g., 0.U(3.W) => b"00000001"  7.U(3.W) => b"1111_1111"
  def apply(data: UInt, dw: Int): UInt = {  // dw is width of data
    if (dw == 1) {
      Mux(data === 0.U, 1.U(2.W), 3.U(2.W))
    } else {
      Mux(data(dw-1), Cat(apply(data(dw-2, 0), dw-1), ~0.U((1 << (dw-1)).W)),
                      Cat(0.U((1 << (dw-1)).W), apply(data(dw-2, 0), dw-1)))
    }
  }
}

// E.g., 0.U(3.W) => b"1111_11111"  1.U(3.W) => b"1111_1110"  7.U(3.W) => b"1000_0000"
object UIntToCont0s {
  def apply(data: UInt, dw: Int): UInt = {  // dw is width of data
    if (dw == 1) {
      Mux(data === 0.U, 3.U(2.W), 2.U(2.W))
    } else {
      Mux(data(dw-1), Cat(apply(data(dw-2, 0), dw-1), 0.U((1 << (dw-1)).W)),
                      Cat(~0.U((1 << (dw-1)).W), apply(data(dw-2, 0), dw-1)))
    }
  }
}

// // E.g., 0.U(3.W) => b"0000_0000"  1.U(3.W) => b"0000_0001"  7.U(3.W) => b"0111_1111"
// object UIntToCont1s {
//   def apply(data: UInt, dw: Int): UInt = {  // dw is width of data
//     if (dw == 1) {
//       Mux(data === 0.U, 0.U(2.W), 1.U(2.W))
//     } else {
//       Mux(data(dw-1), Cat(apply(data(dw-2, 0), dw-1), ~0.U((1 << (dw-1)).W)),
//                       Cat(0.U((1 << (dw-1)).W), apply(data(dw-2, 0), dw-1)))
//     }
//   }
// }
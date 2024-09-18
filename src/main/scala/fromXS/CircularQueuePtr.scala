// /***************************************************************************************
// * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
// * Copyright (c) 2020-2021 Peng Cheng Laboratory
// *
// * XiangShan is licensed under Mulan PSL v2.
// * You can use this software according to the terms and conditions of the Mulan PSL v2.
// * You may obtain a copy of Mulan PSL v2 at:
// *          http://license.coscl.org.cn/MulanPSL2
// *
// * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
// *
// * See the Mulan PSL v2 for more details.
// ***************************************************************************************/

// package utility

// import org.chipsalliance.cde.config.Parameters
// import chisel3._
// import chisel3.util._

// class CircularQueuePtr[T <: CircularQueuePtr[T]](val entries: Int) extends Bundle {

//   def this(f: Parameters => Int)(implicit p: Parameters) = this(f(p))

//   val PTR_WIDTH = log2Up(entries)
//   val flag = Bool()
//   val value = UInt(PTR_WIDTH.W)

//   override def toPrintable: Printable = {
//     p"$flag:$value"
//   }

//   final def +(v: UInt): T = {
//     val entries = this.entries
//     val new_ptr = Wire(this.asInstanceOf[T].cloneType)
//     if(isPow2(entries)){
//       new_ptr := (Cat(this.flag, this.value) + v).asTypeOf(new_ptr)
//     } else {
//       val new_value = this.value +& v
//       val diff = Cat(0.U(1.W), new_value).asSInt - Cat(0.U(1.W), entries.U.asTypeOf(new_value)).asSInt
//       val reverse_flag = diff >= 0.S
//       new_ptr.flag := Mux(reverse_flag, !this.flag, this.flag)
//       new_ptr.value := Mux(reverse_flag,
//         diff.asUInt,
//         new_value
//       )
//     }
//     new_ptr
//   }

//   final def -(v: UInt): T = {
//     val flipped_new_ptr = this + (this.entries.U - v)
//     val new_ptr = Wire(this.asInstanceOf[T].cloneType)
//     new_ptr.flag := !flipped_new_ptr.flag
//     new_ptr.value := flipped_new_ptr.value
//     new_ptr
//   }

//   final def === (that: T): Bool = this.asUInt === that.asUInt

//   final def =/= (that: T): Bool = this.asUInt =/= that.asUInt

//   final def > (that: T): Bool = {
//     val differentFlag = this.flag ^ that.flag
//     val compare = this.value > that.value
//     differentFlag ^ compare
//   }

//   final def < (that: T): Bool = {
//     val differentFlag = this.flag ^ that.flag
//     val compare = this.value < that.value
//     differentFlag ^ compare
//   }

//   final def >= (that: T): Bool = {
//     val differentFlag = this.flag ^ that.flag
//     val compare = this.value >= that.value
//     differentFlag ^ compare
//   }

//   final def <= (that: T): Bool = {
//     val differentFlag = this.flag ^ that.flag
//     val compare = this.value <= that.value
//     differentFlag ^ compare
//   }

//   def toOH: UInt = UIntToOH(value, entries)
// }

// trait HasCircularQueuePtrHelper {

//   def isEmpty[T <: CircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): Bool = {
//     enq_ptr === deq_ptr
//   }

//   def isFull[T <: CircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): Bool = {
//     (enq_ptr.flag =/= deq_ptr.flag) && (enq_ptr.value === deq_ptr.value)
//   }

//   def distanceBetween[T <: CircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): UInt = {
//     assert(enq_ptr.entries == deq_ptr.entries)
//     Mux(enq_ptr.flag === deq_ptr.flag,
//       enq_ptr.value - deq_ptr.value,
//       enq_ptr.entries.U + enq_ptr.value - deq_ptr.value)
//   }

//   def hasFreeEntries[T <: CircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): UInt = {
//     val free_deq_ptr = enq_ptr
//     val free_enq_ptr = WireInit(deq_ptr)
//     free_enq_ptr.flag := !deq_ptr.flag
//     distanceBetween(free_enq_ptr, free_deq_ptr)
//   }

//   def isAfter[T <: CircularQueuePtr[T]](left: T, right: T): Bool = left > right

//   def isBefore[T <: CircularQueuePtr[T]](left: T, right: T): Bool = left < right

//   def isNotAfter[T <: CircularQueuePtr[T]](left: T, right: T): Bool = left <= right

//   def isNotBefore[T <: CircularQueuePtr[T]](left: T, right: T): Bool = left >= right
// }

// // Should only be used when left and right are continuous pointers.
// class QPtrMatchMatrix[T <: CircularQueuePtr[T]](left: Seq[T], right: Seq[T]) {
//   val matrix = left.map(l => right.map(_ === l))

//   def apply(leftIndex: Int, rightIndex: Int): Bool = {
//     require(leftIndex < left.length && rightIndex < right.length)
//     if (leftIndex == 0 || rightIndex == 0) {
//       matrix(leftIndex)(rightIndex)
//     }
//     else {
//       apply(leftIndex - 1, rightIndex - 1)
//     }
//   }
//   def apply(leftIndex: Int): Seq[Bool] = right.indices.map(i => apply(leftIndex, i))
// }

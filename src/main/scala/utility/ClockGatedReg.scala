/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utility

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

object GatedValidRegNext {
  // 3 is the default minimal width of EDA inserted clock gating cells.
  // so using `GatedValidRegNext` to signals whoes width is less than 3 may not help.
  
  // It is useless to clockgate only one bit, so change to RegNext here
  def apply(next: Bool, init: Bool = false.B): Bool = {
    val last = WireInit(false.B)
    last := RegNext(next, init)
    last
  }

  def apply(last: Vec[Bool]): Vec[Bool] = {
    val next = VecInit(Seq.fill(last.size)(false.B))
    next := RegEnable(last, VecInit(Seq.fill(last.size)(false.B)), last.asUInt =/= next.asUInt)
    next
  }
}

object GatedValidRegNextN {
  def apply(in: Bool, n: Int, initOpt: Option[Bool] = None): Bool = {
    (0 until n).foldLeft(in){
      (prev, _) =>
        initOpt match {
          case Some(init) => GatedValidRegNext(prev, init)
          case None => GatedValidRegNext(prev)
        }
    }
  }
}

object GatedRegNext{
  // Vec can be judged and assigned one by one
  def regEnableVec[T <: Data](lastVec: Vec[T], initOptVec: Option[Vec[T]]): Vec[T] = {
    val nextVec = WireInit(0.U.asTypeOf(lastVec))
    for (i <- 0 until lastVec.length) {
      initOptVec match {
        case Some(initVec) => nextVec(i) := RegEnable(lastVec(i), initVec(i), lastVec(i).asUInt =/= nextVec(i).asUInt)
        case None => nextVec(i) := RegEnable(lastVec(i), 0.U.asTypeOf(lastVec(i)), lastVec(i).asUInt =/= nextVec(i).asUInt)
      }
    }
    nextVec
  }

  // NOTICE: The larger Data width , the longger time of =/= operations, which may lead to timing violations.
  // Callers need to consider timing requirements themselves.
  def apply[T <: Data](last: T, initOpt: Option[T] = None): T = {
    val next = WireInit(0.U.asTypeOf(last))
    last match {
      case v: Vec[_] =>
        next := regEnableVec(v.asInstanceOf[Vec[T]], initOpt.map(_.asInstanceOf[Vec[T]]))
      case _ =>
        initOpt match {
          case Some(init) => next := RegEnable(last, init, last.asUInt =/= next.asUInt)
          case None => next := RegEnable(last, 0.U.asTypeOf(last), last.asUInt =/= next.asUInt)
        }
    }
    next
  }
}

object GatedRegEnable{
  // Vec can be judged and assigned one by one
  def regEnableVec[T <: Data](lastVec: Vec[T], initOptVec: Option[Vec[T]], enable: Bool): Vec[T] = {
    val nextVec = WireInit(0.U.asTypeOf(lastVec))
    for (i <- 0 until lastVec.length) {
      initOptVec match {
        case Some(initVec) => nextVec(i) := RegEnable(lastVec(i), initVec(i), enable && lastVec(i).asUInt =/= nextVec(i).asUInt)
        case None => nextVec(i) := RegEnable(lastVec(i), 0.U.asTypeOf(lastVec(i)), enable && lastVec(i).asUInt =/= nextVec(i).asUInt)
      }
    }
    nextVec
  }

  // NOTICE: The larger Data width , the longger time of =/= operations, which may lead to timing violations.
  // Callers need to consider timing requirements themselves.
  def apply[T <: Data](last: T, initOpt: Option[T] = None, enable: Bool): T = {
    val next = WireInit(0.U.asTypeOf(last))
    last match {
      case v: Vec[_] =>
        next := regEnableVec(v.asInstanceOf[Vec[T]], initOpt.map(_.asInstanceOf[Vec[T]]), enable)
      case _ =>
        initOpt match {
          case Some(init) => next := RegEnable(last, init, enable && last.asUInt =/= next.asUInt)
          case None => next := RegEnable(last, 0.U.asTypeOf(last), enable && last.asUInt =/= next.asUInt)
        }
    }
    next
  }

  def dupRegs[T <: Data](last: Vec[T], initOpt: Option[Vec[T]] = None, enable: Bool): Vec[T] = {
    require(last.length > 0, "Input Vec must have at least one element")
    
    val numDup = last.length
    val next = WireInit(0.U.asTypeOf(last))

    val updateEnable = enable && last(0).asUInt =/= next(0).asUInt

    for (i <- 0 until numDup) {
      initOpt match {
        case Some(init) => next(i) := RegEnable(last(i), init(i), updateEnable)
        case None => next(i) := RegEnable(last(i), 0.U.asTypeOf(last(i)), updateEnable)
      }
    }
    next
  }
}

object GatedRegNextN {
  def apply[T <: Data](in: T, n: Int, initOpt: Option[T] = None): T = {
    (0 until n).foldLeft(in){
      (prev, _) => GatedRegNext(prev, initOpt)
    }
  }
}

class SegmentedAddr(_segments: Seq[Int]) {

  def this(f: Parameters => Seq[Int])(implicit p: Parameters) = this(f(p))

  val segments = _segments // (High, Lower ...)
  private var addr = UInt(segments.sum.W)

  // [High, Lower ...]
  private def segment(addrIn: UInt): Seq[UInt] = {
    segments.foldLeft((Seq[UInt](), addrIn.asBools)) { (acc, segment_length) =>
      ((acc._1 :+ VecInit(acc._2.takeRight(segment_length)).asUInt), acc._2.dropRight(segment_length))
    }._1
  }

  def getAddr(): UInt = {
    addr
  }
  def getAddrSegments(): Seq[UInt] = {
    segment(addr)
  }
  def fromSegments(seg: Seq[UInt]) = {
    this.addr = seg.reduce(Cat(_, _))
    this
  }
  def fromAddr(addrIn: UInt) = {
    this.addr = addrIn
    this
  }

  def compare(that: SegmentedAddr): Seq[Bool] = {
    this.getAddrSegments() zip that.getAddrSegments() map {
      case (l, r) => l =/= r
    }
  }

  def compare(that: Seq[UInt]): Seq[Bool] = {
    this.getAddrSegments() zip that map {
      case (l, r) => l =/= r
    }
  }
}

object SegmentedAddrNext {
  def apply(addr: SegmentedAddr): SegmentedAddr = {
    apply(addr, true.B, None)
  }

  def apply(addr: SegmentedAddr, fire: Bool, parentName: Option[String]): SegmentedAddr = {
    apply(addr.getAddr(), addr.segments, fire, parentName)
  }

  def apply(addr: UInt, segments: Seq[Int], fire: Bool, parentName: Option[String]): SegmentedAddr = {
    // Input wire, segmented
    val segmented = new SegmentedAddr(segments).fromAddr(addr).getAddrSegments()

    val modified = Wire(Vec(segmented.length, Bool()))

    val segmentedNext = segments zip segmented zip modified.zipWithIndex map {
      case ((segLength, seg), (modified, idx)) =>
      // Must init here to avoid X state
      RegEnable(seg, 0.U(segLength.W), modified && fire)
        .suggestName(s"${parentName.getOrElse("")}_seg_${idx}_value")
    }
    modified zip segmentedNext zip segmented map {
      case ((m, next), now) => m := next =/= now
    }
    modified.last := true.B // Assume lower part often changes

    val seg = new SegmentedAddr(segments).fromSegments(segmentedNext)
    if (parentName.isDefined) {
      val debug_addr = WireDefault(seg.getAddr()).suggestName(s"debug_${parentName.get}_addr")
      val debug_modified = modified.suggestName(s"debug_${parentName.get}_modified")
      dontTouch(debug_addr)
      dontTouch(debug_modified)
    }

    seg
  }
  
  def dupAddrs(addrs: Seq[UInt], segments: Seq[Int], fire: Bool, parentName: Option[String]): Seq[SegmentedAddr] = {
    // Input wire, segmented
    val dupSegmented = segments.zipWithIndex.map { case (segLength, idx) =>
      addrs.map(addr => new SegmentedAddr(segments).fromAddr(addr).getAddrSegments()(idx))
    }
    // dupSegmented(segIdx)(dupIdx), 
    // dupSegmented = Seq(
    //   Seq(seg0_addr0, seg0_addr1, seg0_addr2, seg0_addr3),  // first  segment results of each addr after segmentation
    //   Seq(seg1_addr0, seg1_addr1, seg1_addr2, seg1_addr3),  // second segment results of each addr after segmentation
    //   Seq(seg2_addr0, seg2_addr1, seg2_addr2, seg2_addr3)   // third  segment results of each addr after segmentation
    // )

    val modified = Wire(Vec(segments.length, Bool()))

    val dupSegmentedNext = segments zip dupSegmented zip modified.zipWithIndex map {
      case ((segLength, dupSegs), (modified, segIdx)) => {
        val nextDupSegs = dupSegs.zipWithIndex.map{ case (seg, dupIdx) =>
          // for each seg in duplicate segs ...
          RegEnable(seg, 0.U(segLength.W), modified && fire)
            .suggestName(s"${parentName.getOrElse("")}_seg_${segIdx}_dup_${dupIdx}_value")
        }
        nextDupSegs
      }
    }

    for (segIdx <- 0 until segments.length) {
      modified(segIdx) := dupSegmented(segIdx)(0) =/= dupSegmentedNext(segIdx)(0)
    }
    modified.last := true.B // Assume lower part often changes

    // dupSegmentedNext.transpose:
    // Seq(
    //   Seq(addr0_seg0, addr0_seg1, addr0_seg2),  // segmented result of addr(0)
    //   Seq(addr1_seg0, addr1_seg1, addr1_seg2),  // segmented result of addr(1)
    //   Seq(addr2_seg0, addr2_seg1, addr2_seg2),  // segmented result of addr(2)
    //   Seq(addr3_seg0, addr3_seg1, addr3_seg2)   // segmented result of addr(3)
    // )
    val result = dupSegmentedNext.transpose.map { segs =>
      new SegmentedAddr(segments).fromSegments(segs)
    }
    result
  }
}

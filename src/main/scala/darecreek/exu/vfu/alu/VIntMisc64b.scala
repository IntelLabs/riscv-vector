/**
  * Perform below instructions:
  *     11.3  vzext, ...
  *     11.5  vand, ...
  *     11.6  vsll, ...
  *     11.7  vnsrl, ...
  *     11.15 vmerge
  *     11.16 vmv.v.
  *     Part of 12.4
  *     Part of 12.5
  *     16.1
  *     16.2
  *     16.6
  */
package darecreek.exu.vfu.alu

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
// import darecreek.exu.vfu.VFUParam._
import org.chipsalliance.cde.config.Parameters

class VIntMisc64b(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val funct6 = Input(UInt(6.W))
    val funct3 = Input(UInt(3.W))
    val vi = Input(Bool()) // OPIVI: vs2 op imm
    val vm = Input(Bool())
    val vs1_imm = Input(UInt(5.W))
    val narrow = Input(Bool())
    val sew = Input(new SewOH)
    val uopIdx = Input(UInt(3.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val vmask = Input(UInt(8.W))

    val vd = Output(UInt(64.W))
    val narrowVd = Output(UInt(32.W))
    val toFixP = Output(new MiscToFixP)
    val rd = ValidIO(UInt(XLEN.W))
  })

  val funct6 = io.funct6
  val funct3 = io.funct3
  val sew = io.sew
  val uopIdx = io.uopIdx
  val vs1 = io.vs1
  val vs2 = io.vs2
  val vmask = io.vmask

  //---- Extension instructions ----
  val extSign = io.vs1_imm(0)  // 0: z   1: s
  val vf2 = io.vs1_imm(2, 1) === 3.U
  val vf4 = io.vs1_imm(2, 1) === 2.U
  val expdIdxOH_vf2 = Seq.tabulate(2)(i => uopIdx(0) === i.U)
  val expdIdxOH_vf4 = Seq.tabulate(4)(i => uopIdx(1,0) === i.U)
  val expdIdxOH_vf8 = Seq.tabulate(8)(i => uopIdx === i.U)
  val extResult = Wire(UInt(64.W))
  when (vf2) {  // sew = 16/32/64
    extResult := Mux(sew.is16, BitsExtend.vector(Mux1H(expdIdxOH_vf2.take(2), UIntSplit(vs2, 32)), 64, extSign, 8),
                 Mux(sew.is32, BitsExtend.vector(Mux1H(expdIdxOH_vf2.take(2), UIntSplit(vs2, 32)), 64, extSign, 16),
                               BitsExtend.vector(Mux1H(expdIdxOH_vf2.take(2), UIntSplit(vs2, 32)), 64, extSign, 32)))
  }.elsewhen (vf4) {  // sew = 32/64
    extResult := Mux(sew.is32, BitsExtend.vector(Mux1H(expdIdxOH_vf4.take(4), UIntSplit(vs2, 16)), 64, extSign, 8),
                               BitsExtend.vector(Mux1H(expdIdxOH_vf4.take(4), UIntSplit(vs2, 16)), 64, extSign, 16))
  }.otherwise {  //vf8  sew = 64
    extResult := BitsExtend(Mux1H(expdIdxOH_vf8, UIntSplit(vs2, 8)), 64, extSign)
  }

  //---- Bitwise Logical instructions ----
  val bitLogical = Mux1H(Seq(
    (funct6(1, 0) === 1.U) -> (vs2 & vs1),
    (funct6(1, 0) === 2.U) -> (vs2 | vs1),
    (funct6(1, 0) === 3.U) -> (vs2 ^ vs1),
  ))

  /**
    * Shift: vsll, vsrl, vsra, vnsrl, vnsra (vssrl, vssra and vnclipu/vnclip)
    */
  val signed = funct6(3) && funct6(0)
  def shiftOnce(n: Int, data: UInt): (UInt, Bool, Bool) = { // n: number of shift bits
    val len = data.getWidth
    require(len > n)
    val rnd_high = data(n-1)
    val rnd_tail = if (n == 1) true.B else {data(n-2, 0) === 0.U}
    //             result of bit-shift                     v[d-1]   v[d-2:0] == 0
    (Cat(Fill(n, data(len-1) && signed), data(len-1, n)), rnd_high, rnd_tail)
  }
  // Shift amount is dynamic
  def dynamicShift(shiftAmount: UInt, data: UInt): (UInt, Bool, Bool) = {
    val width = shiftAmount.getWidth
    val shiftSeq = Seq(1, 2, 4, 8, 16, 32)
    // (shiftAmount.asBools).zip(shiftSeq.take(width)).foldLeft(data) {
    //   case (data, (bit, n)) => Mux(bit, shiftOnce(n, data), data)
    // }
    val dataInit = (data, false.B, true.B)
    (shiftAmount.asBools).zip(shiftSeq.take(width)).foldLeft(dataInit) {
      case ((data, rnd_high, rnd_tail), (bit, n)) => {
        val shiftOnceResult = shiftOnce(n, data)
        val data_update = Mux(bit, shiftOnceResult._1, data)
        val rnd_high_update = Mux(bit, shiftOnceResult._2, rnd_high)
        val rnd_tail_update = Mux(bit, shiftOnceResult._3 && rnd_tail && !rnd_high, rnd_tail)
        (data_update, rnd_high_update, rnd_tail_update)
      }
    }
  }
  // Shift SEW bits data (one element)
  def shiftOneElement(shiftAmount: UInt, data: UInt, sew: Int): (UInt, Bool, Bool) = {
    sew match {
      case 8  => dynamicShift(shiftAmount(2, 0), data)
      case 16 => dynamicShift(shiftAmount(3, 0), data)
      case 32 => dynamicShift(shiftAmount(4, 0), data)
      case 64 => {  // For sew=64, the uimm should perform zero-extending
        val shiftAmount_uimm = Cat(Mux(io.vi, false.B, shiftAmount(5)), shiftAmount(4, 0))
        dynamicShift(shiftAmount_uimm, data)
      }
    }
  }

  // Handle shift left
  val leftShift = funct6(3, 0) === "b0101".U
  val vs2_reverse = Cat(vs2.asBools) // bit reverse
  val vs2_adjust = Mux(leftShift, vs2_reverse, vs2)
  val vs1_revsByElem = MuxCase(vs1, Seq(  // reverse vs1 by element when left-shift
    (leftShift && sew.is32) -> Cat(UIntSplit(vs1, 32)),
    (leftShift && sew.is16) -> Cat(UIntSplit(vs1, 16)),
    (leftShift && sew.is8)  -> Cat(UIntSplit(vs1, 8))
  ))
  // Handle narrow instruction
  val vs1Split = UIntSplit(vs1_revsByElem, 8)
  val vs1_adjust = Wire(UInt(64.W))
  when (io.narrow && !uopIdx(0)) {
    vs1_adjust := Cat(0.U(8.W), vs1Split(3), 0.U(8.W), vs1Split(2), 0.U(8.W), vs1Split(1), 0.U(8.W), vs1Split(0))
  }.elsewhen (io.narrow && uopIdx(0)) {
    vs1_adjust := Cat(0.U(8.W), vs1Split(7), 0.U(8.W), vs1Split(6), 0.U(8.W), vs1Split(5), 0.U(8.W), vs1Split(4))
  }.otherwise {
    vs1_adjust := vs1_revsByElem
  }
  
  def shift(sew: Int): Seq[(UInt, Bool, Bool)] = {
    UIntSplit(vs2_adjust, sew) zip UIntSplit(vs1_adjust, sew) map {case (vs2, vs1) => shiftOneElement(vs1, vs2, sew)}
  }
  // val shiftOut = MuxCase(shift(64), Seq(
  //   Mux(uop.ctrl.narrow, sew.is16, sew.is32) -> shift(32),
  //   Mux(uop.ctrl.narrow, sew.is8,  sew.is16) -> shift(16),
  //   Mux(uop.ctrl.narrow, false.B,  sew.is8)  -> shift(8)
  // ))

  val (shift64, shift32, shift16, shift8) = (shift(64), shift(32),shift(16), shift(8))
  val shiftOut = Wire(UInt(64.W))
  // Different SEW cases
  when (Mux(io.narrow, sew.is16, sew.is32)) {
    shiftOut := Cat(shift32.map(_._1).reverse)
    io.toFixP.rnd_high := Cat(Fill(4, shift32(1)._2), Fill(4, shift32(0)._2))
    io.toFixP.rnd_tail := Cat(Fill(4, shift32(1)._3), Fill(4, shift32(0)._3))
  }.elsewhen (Mux(io.narrow, sew.is8,  sew.is16)) {
    shiftOut := Cat(shift16.map(_._1).reverse)
    io.toFixP.rnd_high := Cat(Fill(2, shift16(3)._2), Fill(2, shift16(2)._2), Fill(2, shift16(1)._2), Fill(2, shift16(0)._2))
    io.toFixP.rnd_tail := Cat(Fill(2, shift16(3)._3), Fill(2, shift16(2)._3), Fill(2, shift16(1)._3), Fill(2, shift16(0)._3))
  }.elsewhen (Mux(io.narrow, false.B,  sew.is8)) {
    shiftOut := Cat(shift8.map(_._1).reverse)
    io.toFixP.rnd_high := Cat(shift8.map(_._2).reverse)
    io.toFixP.rnd_tail := Cat(shift8.map(_._3).reverse)
  }.otherwise {
    shiftOut := shift64(0)._1
    io.toFixP.rnd_high := Fill(8, shift64(0)._2)
    io.toFixP.rnd_tail := Fill(8, shift64(0)._3)
  }
  io.toFixP.shiftOut := shiftOut
  
  val shiftResult = Mux(leftShift, Cat(shiftOut.asBools), shiftOut)
  io.narrowVd := Mux1H(Seq(
    sew.is32 -> shiftOut(31, 0),
    sew.is16 -> Cat(shiftOut(47, 32), shiftOut(15, 0)),
    sew.is8  -> Cat(shiftOut(55, 48), shiftOut(39, 32), shiftOut(23, 16), shiftOut(7, 0))
  ))
  
  /**
    * Integer Merge/Move
    */
  // Adjust vmask. E.g., if sew==32: 000000ab -> aaaabbbb   
  val vmask_adjust = Mux1H(sew.oneHot, Seq(1, 2, 4, 8).map(k => 
    Cat(Seq.tabulate(8/k)(i => Fill(k, vmask(i))).reverse)
  ))
  val mergeResult = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    mergeResult(i) := Mux(vmask_adjust(i), vs1(8*i+7, 8*i), vs2(8*i+7, 8*i))
  }
  val mergeMove = Mux(io.vm, vs1, Cat(mergeResult.reverse))

  /** 
   * 16.1 16.2 (funct6 = 010000    funct3 = 010 x.s  110 s.x  001 f.s  101 s.f)
   *  @note vs should be 0 for vmv.x.s, we didn't do the judgement since other vs1 values
   *  are for mask instrns (vcpop, vfirst)
   */
  val perm_vmv = Wire(UInt(64.W))
  perm_vmv := Mux1H(Seq(
    (funct3 === "b110".U) -> Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(k => vs1(k-1, 0))),
    (funct3 === "b101".U) -> Mux1H(sew.oneHot(3,2), Seq(32, 64).map(k => vs1(k-1, 0)))
  ))

  // Output arbiter                      // vmv<nr>r
  io.vd := Mux(funct6(5), Mux(funct6 === "b100111".U, vs2, shiftResult),
           Mux(funct6(5, 2) === "b0100".U, Mux(funct6(1), extResult, perm_vmv),
           Mux(funct6(5, 2) === "b0010".U, bitLogical, mergeMove)))
  io.rd.bits := Mux1H(Seq(
    (funct3 === "b010".U) -> Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(k => vs2(k-1, 0).asSInt.pad(XLEN).asUInt)),
    (funct3 === "b001".U) -> Mux1H(sew.oneHot(3,2), Seq(32, 64).map(k => vs2(k-1, 0).asUInt.pad(XLEN))),
  ))
  io.rd.valid := funct6 === "b010000".U && (funct3 === "b010".U || funct3 === "b001".U)
}
/***************************************************************************************
*Copyright (c) 2023-2024 Intel Corporation
*Vector Acceleration IP core for RISC-V* is licensed under Mulan PSL v2.
*You can use this software according to the terms and conditions of the Mulan PSL v2.
*You may obtain a copy of Mulan PSL v2 at:
*        http://license.coscl.org.cn/MulanPSL2
*THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
*EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
*MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*See the Mulan PSL v2 for more details.
***************************************************************************************/

package darecreek.exu.vfu.mac

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._

/** 64-bit vector multiply and accumlation unit
 *  
 *  Support these instructions: 11.10, 11.12, 11.13, 11.14, 12.3
 *  io.fireIn, io.fireS1 are fire signals of valid/ready mechanisam.
 *  fireIn is input fire of MAC, fireS1 is the next stage fire.
 *  If ready is not needed, set fireIn = valid, fireS1 = RegNext(valid)
 */
class VMac64b extends Module {
  val io = IO(new Bundle {
    val fireIn = Input(Bool())
    val fireS1 = Input(Bool())
    val sew = Input(new SewOH)
    val uopIdx = Input(UInt(3.W))
    val vxrm = Input(UInt(2.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val oldVd = Input(UInt(64.W))
    val vs1_is_signed = Input(Bool())
    val vs2_is_signed = Input(Bool())
    val highHalf = Input(Bool())
    val isMacc = Input(Bool()) // (w)macc(nmsac)/madd(nmsub)
    val isSub = Input(Bool())
    val widen = Input(Bool())
    val isFixP = Input(Bool())

    val vd = Output(UInt(64.W))
    val vxsat = Output(UInt(8.W))
  })

  val vs2 = io.vs2
  val vs1 = io.vs1
  val oldVd = io.oldVd
  val vs2_is_signed = io.vs2_is_signed // vs2 & vd should be signed numbers for vmadd/vnmsub
  val vs1_is_signed = io.vs1_is_signed
  val sew = io.sew
  val uopIdx = io.uopIdx
  val fireIn = io.fireIn
  val fireS1 = io.fireS1

  /**
   *  First pipeline stage:
   *    (1) Booth-recoding partial products generation (2) Wallace tree
   */
  // Booth recoding
  class boothRecode(d: UInt) {
    val positive = !d(2) && d(1,0) =/= 0.U
    val negative = d(2) && d(1,0) =/= 3.U
    val one = d(1) =/= d(0)
    val double = d(2) =/= d(1) && d(1) === d(0)
  }
  val vs1Booth3b = Wire(Vec(32, UInt(3.W)))
  vs1Booth3b(0) := Cat(vs1(1, 0), false.B)
  for (i <- 1 until 32) {
    if (i % 4 != 0) {
      vs1Booth3b(i) := vs1(2*i+1, 2*i-1)
    } else if (i == 16) {
      vs1Booth3b(i) := Mux(sew.is64, vs1(2*i+1, 2*i-1), Cat(vs1(2*i+1, 2*i), false.B))
    } else if (i % 8 == 0) {
      vs1Booth3b(i) := Mux(sew.is64 || sew.is32, vs1(2*i+1, 2*i-1), Cat(vs1(2*i+1, 2*i), false.B))
    } else {
      vs1Booth3b(i) := Mux(!sew.is8, vs1(2*i+1, 2*i-1), Cat(vs1(2*i+1, 2*i), false.B))
    }
  }
  val vs1Booth = Seq.tabulate(32)(i => new boothRecode(vs1Booth3b(i)))
  // Partial products
  val partProd = Wire(Vec(32+2, UInt(128.W)))
  def calcPartProd(i: Int, sew: Int) = {
      val blockIdx = (2*i)/sew
      val elementVs2 = UIntSplit(vs2, sew)(blockIdx)
      val elementVs2L = BitsExtend(elementVs2, 2*sew, vs2_is_signed) // width: 2*sew
      val boothDouble = Wire(UInt((2*sew).W))
      boothDouble := Mux1H(Seq(vs1Booth(i).one    -> elementVs2L,
                               vs1Booth(i).double -> (elementVs2L << 1)))
      val boothResult = Mux1H(Seq(vs1Booth(i).positive -> boothDouble,
                                  vs1Booth(i).negative -> (~boothDouble)))
      val shiftBits = 2 * i - sew * blockIdx
      val shifted = (boothResult << shiftBits)(2*sew-1, 0)
      if (sew == 64) {shifted}
      else if (blockIdx == 0) {Cat(0.U((128-(blockIdx+1)*sew*2).W), shifted)}
      else if (blockIdx == 64/sew - 1) {Cat(shifted, 0.U((blockIdx*sew*2).W))}
      else {Cat(0.U((128-(blockIdx+1)*sew*2).W), shifted, 0.U((blockIdx*sew*2).W))}
  }
  for (i <- 0 until 32) {
    partProd(i) := Mux1H(sew.oneHot, Seq(8,16,32,64).map(sew => calcPartProd(i, sew)))
  }

  // Generate an extra addend (partProd(32)) to:
  // (1) Handle +1 for all negative numbers
  // (2) Handle unsinged vs1
  // (3) Handle +1 for negative old_vd (io.isSub)
  def blockPlus1(sew: Int, booth_neg: UInt, highVs1: Bool, vs1_is_unsign: Bool, data: UInt, subOldVd: Bool): UInt = {
    val hi = Wire(UInt(sew.W)) //Handle unsinged vs1
    hi := Mux(highVs1 && vs1_is_unsign, data, 0.U)
    val lo = Wire(UInt(sew.W)) //Handle +1 for all negative numbers
    lo := VecInit(Seq.tabulate(sew/2)(i => Cat(false.B, booth_neg(i)))).asUInt
    Cat(hi, Cat(lo(sew-1, 2), lo(1, 0) + subOldVd.asUInt))
  }
  val plus1sew64 = Wire(UInt(128.W))
  val plus1sew32 = Wire(UInt(128.W))
  val plus1sew16 = Wire(UInt(128.W))
  val plus1sew8 = Wire(UInt(128.W))
  val booth_neg = VecInit(vs1Booth.map(_.negative)).asUInt
  plus1sew64 := blockPlus1(64, booth_neg, vs1(63), !vs1_is_signed, vs2, io.isSub)
  plus1sew32 := VecInit(Seq.tabulate(2)(i => blockPlus1(32, UIntSplit(booth_neg, 16)(i),
                            UIntSplit(vs1, 32)(i)(31), !vs1_is_signed, UIntSplit(vs2, 32)(i), io.isSub))).asUInt
  plus1sew16 := VecInit(Seq.tabulate(4)(i => blockPlus1(16, UIntSplit(booth_neg, 8)(i),
                            UIntSplit(vs1, 16)(i)(15), !vs1_is_signed, UIntSplit(vs2, 16)(i), io.isSub))).asUInt
  plus1sew8 := VecInit(Seq.tabulate(8)(i => blockPlus1(8, UIntSplit(booth_neg, 4)(i),
                            UIntSplit(vs1, 8)(i)(7), !vs1_is_signed, UIntSplit(vs2, 8)(i), io.isSub))).asUInt
  partProd(32) := Mux1H(sew.oneHot, Seq(plus1sew8, plus1sew16, plus1sew32, plus1sew64))
  // Old vd
  val oldVdReorg = Mux1H(sew.oneHot, Seq(8,16,32,64).map(sew => 
                   VecInit(UIntSplit(oldVd, sew).map(x => BitsExtend(x, 2*sew, false.B))).asUInt))
  partProd(33) := Mux(io.isMacc, Mux(io.widen, Cat(oldVd, oldVd), Mux(io.isSub, ~oldVdReorg, oldVdReorg)), 0.U)

  /**
   *  Wallace tree
   */
  // 3-bit full-adder
  def add3_UInt(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    val cout = (a & b) | (b & c) | (a & c)
    val sum = a ^ b ^ c
    (cout, sum)
  }
  // ---- Perform wallace reduction once (3 -> 2) ----
  // Input data: n x 128    Output: ([n/3]*2 + n%3) x 128
  // def reduce3to2(data: Seq[UInt]): Seq[UInt] = {
  def reduce3to2(data: Seq[UInt], sew: SewOH): Seq[UInt] = {
    val nAddends = data.size
    val n3Group = nAddends / 3
    val cout = Seq.fill(n3Group)(Wire(UInt(128.W)))
    val sum = Seq.fill(n3Group)(Wire(UInt(128.W)))
    for (i <- 0 until n3Group) {
      cout(i) := add3_UInt(data(3*i), data(3*i+1), data(3*i+2))._1 &
                 Mux1H(Seq(sew.is8  -> "h7fff7fff7fff7fff7fff7fff7fff7fff".U(128.W),
                           sew.is16 -> "h7fffffff7fffffff7fffffff7fffffff".U(128.W),
                           sew.is32 -> "h7fffffffffffffff7fffffffffffffff".U(128.W),
                           sew.is64 -> ~0.U(128.W) ))
      sum(i) := add3_UInt(data(3*i), data(3*i+1), data(3*i+2))._2
    }
    val cin = cout.map(x => Cat(x(126, 0), 0.U(1.W)))
    sum ++ cin ++ data.drop(3 * n3Group)
  }

  // Generate an array to store number of addends of each wallace stage (e.g., [33, 22, 15, ..., 3])
  def nAddendsSeqGen(num_of_addends: Int): Seq[Int] = {
    num_of_addends match {
      case 3 => Seq(3)
      case n => n +: nAddendsSeqGen((n / 3) * 2 + n % 3)
    }
  }
  val nAddendsSeq = nAddendsSeqGen(34)
  // println(nAddendsSeq) =  List(34, 23, 16, 11, 8, 6, 4, 3)

  // Perform all wallace stages.
  def wallaceStage(stageIdx: Int): Seq[UInt] = {
    stageIdx match {
      case 0 => reduce3to2(partProd, sew)
      case k => reduce3to2(wallaceStage(k-1), sew)
    }
  }
  // val wallaceOut = wallaceStage(nAddendsSeq.size - 1)  // Seq(2)(UInt(128.W))
  val wallaceOut_mid = wallaceStage(nAddendsSeq.size - 1 - 3)  // Seq(6)(UInt(128.W))
  
  //------ To alleviate timing, move last 3 steps of wallace into the second pipeline stage
  val wallaceOut_mid_reg = RegEnable(VecInit(wallaceOut_mid), fireIn)
  //-------------------------------------------------------------------------------------

  /**
   *  Second pipeline stage: 128 + 128
   */
  val sewS1 = RegEnable(sew, fireIn)
  val highHalfS1 = RegEnable(io.highHalf, fireIn)
  val widenS1 = RegEnable(io.widen, fireIn)
  val uopIdxS1 = RegEnable(uopIdx, fireIn)
  val vxrmS1 = RegEnable(io.vxrm, fireIn)
  val isSubS1 = RegEnable(io.isSub, fireIn)
  val isFixPS1 = RegEnable(io.isFixP, fireIn)
  // val wallaceOutReg = wallaceOut map {x => RegNext(x)}
  val wallaceOutReg = reduce3to2(reduce3to2(reduce3to2(wallaceOut_mid_reg, sewS1), sewS1), sewS1) // Seq(2)(UInt(128.W))

  // Sum of final two 128b numbers
  class Adder_16b(in1: UInt, in2: UInt, cin: UInt) {
    private val bits = Cat(0.U(1.W), in1, cin) +  Cat(0.U(1.W), in2, cin)
    val (cout, out) = (bits(17), bits(16, 1))
  }
  val cin_wo = Wire(Vec(8, Bool()))
  val cout_wo = Wire(Vec(8, Bool()))
  val sum_wo = Wire(Vec(8, UInt(16.W)))
  for (i <- 0 until 8) {
    val adder_16b = new Adder_16b(wallaceOutReg(0)(16*i+15, 16*i), wallaceOutReg(1)(16*i+15, 16*i), cin_wo(i))
    cout_wo(i) := adder_16b.cout
    if (i == 0) {
      cin_wo(i) := false.B
    } else if (i == 4) {
      cin_wo(i) := Mux(sewS1.is64, cout_wo(i-1), false.B)
    } else if (i % 2 == 0) {
      cin_wo(i) := Mux(sewS1.is64 || sewS1.is32, cout_wo(i-1), false.B)
    } else {
      cin_wo(i) := Mux(sewS1.is8, false.B, cout_wo(i-1))
    }
    // cin_wo(i) := Mux1H(sewS1.oneHot, Seq(1,2,4,8).map(n => if (i % n == 0) false.B else cout_wo(i-1)))
    sum_wo(i) := adder_16b.out
  }

  /**
   *  Third pipeline stage
   *    (1) get vd  (2) vsmul  (3) vs1*vs2-vd --> -vs1*vs2+vd (vnmsac/vnmsub)
   */
  val walOut = Wire(UInt(128.W))
  walOut := RegEnable(sum_wo.asUInt, fireS1)
  val sewS2 = RegEnable(sewS1, fireS1)
  val highHalfS2 = RegEnable(highHalfS1, fireS1)
  val widenS2 = RegEnable(widenS1, fireS1)
  val uopIdxS2 = RegEnable(uopIdxS1, fireS1)
  val vxrmS2 = RegEnable(vxrmS1, fireS1)
  val isSubS2 = RegEnable(isSubS1, fireS1)
  val isFixPS2 = RegEnable(isFixPS1, fireS1)
  val vdS2 = PriorityMux(Seq(
           (sewS2.is64 || widenS2) -> Mux(sewS2.is64 && highHalfS2 || widenS2 && uopIdxS2(0), 
                                          walOut(127, 64), walOut(63, 0)),
           sewS2.is32 -> VecInit(UIntSplit(walOut, 64).map(x => Mux(highHalfS2, x(63, 32), x(31, 0)))).asUInt,
           sewS2.is16 -> VecInit(UIntSplit(walOut, 32).map(x => Mux(highHalfS2, x(31, 16), x(15, 0)))).asUInt,
           sewS2.is8  -> VecInit(UIntSplit(walOut, 16).map(x => Mux(highHalfS2, x(15, 8), x(7, 0)))).asUInt
         ))
  // 12.3 vsmul
  val sat = UIntSplit(walOut, 16).map(x => x(15, 14) === 1.U) //Saturate
  val vxsat = Mux1H(Seq(
    sewS2.is8  -> Cat(sat.reverse),
    sewS2.is16 -> Cat(Fill(2, sat(7)), Fill(2, sat(5)), Fill(2, sat(3)), Fill(2, sat(1))),
    sewS2.is32 -> Cat(Fill(4, sat(7)), Fill(4, sat(3))),
    sewS2.is64 -> Fill(8, sat(7))
  ))
  val walOutRnd8 = VecInit(UIntSplit(walOut, 16).map(x => x(14, 7))).asUInt
  val walOutRnd16 = VecInit(UIntSplit(walOut, 32).map(x => x(30, 15))).asUInt
  val walOutRnd32 = VecInit(UIntSplit(walOut, 64).map(x => x(62, 31))).asUInt
  val walOutRnd64 = walOut(126, 63)
  val walOutRnd = Mux1H(sewS2.oneHot, Seq(walOutRnd8, walOutRnd16, walOutRnd32, walOutRnd64))
  def rndIncGen(v_d: Bool, v_d_1: Bool, tail: UInt): Bool = {
    Mux1H(Seq((vxrmS2 === 0.U) -> v_d_1,
              (vxrmS2 === 1.U) -> (v_d_1 && (tail =/= 0.U || v_d)),
              (vxrmS2 === 2.U) -> false.B,
              (vxrmS2 === 3.U) -> (!v_d && Cat(v_d_1, tail) =/= 0.U) ))
  }
  val rndInc8 = UIntSplit(walOut, 16).map(x => rndIncGen(x(7), x(6), x(5, 0)))
  val rndInc16 = UIntSplit(walOut, 32).map(x => rndIncGen(x(15), x(14), x(13, 0)))
  val rndInc32 = UIntSplit(walOut, 64).map(x => rndIncGen(x(31), x(30), x(29, 0)))
  val rndInc64 = rndIncGen(walOut(63), walOut(62), walOut(61, 0))
  val rndInc = Mux1H(Seq(
    sewS2.is8  -> Cat(rndInc8.reverse),
    sewS2.is16 -> Cat(false.B, rndInc16(3), false.B, rndInc16(2), false.B, rndInc16(1), false.B, rndInc16(0)),
    sewS2.is32 -> Cat(0.U(3.W), rndInc32(1), 0.U(3.W), rndInc32(0)),
    sewS2.is64 -> Cat(0.U(7.W), rndInc64)
  ))
  // cin is 1 bit carry-in
  class Adder_8b_rnd(in1: UInt, cin: Bool) {
    private val bits = in1 + cin.asUInt
    val (cout, out) = (in1 === "b1111_1111".U && cin, bits)
  }
  // Chain up eight 8-bit adders
  def Adder_chain_rnd(data: Seq[UInt], rndInc: UInt): Seq[UInt] = {
    val cin = Wire(Vec(8, Bool()))
    val cout = Wire(Vec(8, Bool()))
    val out = Wire(Vec(8, UInt(8.W)))
    for (i <- 0 until 8) {
      val adder_8b_rnd = new Adder_8b_rnd(data(i), cin(i))
      if (i == 0) {
        cin(i) := rndInc(i)
      } else if (i == 4) {
        cin(i) := Mux(sewS2.is64, cout(i-1), rndInc(i))
      } else if (i % 2 == 0) {
        cin(i) := Mux(sewS2.is64 || sewS2.is32, cout(i-1), rndInc(i))
      } else {
        cin(i) := Mux(sewS2.is8, rndInc(i), cout(i-1))
      }
      // cin(i) := Mux1H(sewS2.oneHot, Seq(1, 2, 4, 8).map(n => if ((i % n) == 0) rndInc(i) else cout(i-1)))
      cout(i) := adder_8b_rnd.cout
      out(i) := adder_8b_rnd.out
    }
    out
  }
  // vs1*vs2-vd --> -vs1*vs2+vd (vnmsac/vnmsub)
  val adderChainData = Mux(isSubS2, ~vdS2, walOutRnd)
  val adderChainCin = Mux(isSubS2, ~0.U(8.W), rndInc)
  val adderChainOut = Adder_chain_rnd(UIntSplit(adderChainData, 8), adderChainCin)
  val vdFixP = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    // If this 8-bit portion is the highest bits of a SEW-bit element
    val highestBits = Mux1H(Seq(
      sewS2.is8  -> true.B,
      sewS2.is16 -> {if ((i % 2) == 1) true.B else false.B},
      sewS2.is32 -> {if ((i % 4) == 3) true.B else false.B},
      sewS2.is64 -> {if (i == 7) true.B else false.B},
    ))
    vdFixP(i) := Mux(vxsat(i), Cat(!highestBits, ~0.U(7.W)), adderChainOut(i))
  }

  io.vd := Mux(isFixPS2, vdFixP.asUInt, Mux(isSubS2, VecInit(adderChainOut).asUInt, vdS2))
  io.vxsat := Mux(isFixPS2, vxsat, 0.U)
}
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

package darecreek.exu.lanevfu.mac

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import darecreek.{SewOH, UIntSplit, MaskReorg}
import darecreek.DarecreekParam._
import darecreek.exu.vfucore.mac.VMac64b
import darecreek.exu.vfucore.{LaneFUInput, LaneFUOutput}
import darecreek.exu.vfucoreconfig.{VUop, Redirect}

// finalResult = result & maskKeep | maskOff
class MaskTailDataVMac extends Module {
  val io = IO(new Bundle {
    val mask = Input(UInt(8.W))
    val tail = Input(UInt(8.W))
    val vstart_gte_vl = Input(Bool())
    val oldVd = Input(UInt(64.W))
    val uop = Input(new VUop)
    val maskKeep = Output(UInt(64.W))  // keep: 11..1  off: 00..0
    val maskOff = Output(UInt(64.W))   // keep: 00..0  off: old_vd or 1.U
  })

  val maskTail = Wire(Vec(8, UInt(2.W))) // 00: keep result   10: old_vd(undisturbed)  11: write 1s(agnostic)
  val (mask, tail, oldVd, uop) = (io.mask, io.tail, io.oldVd, io.uop)
  for (i <- 0 until 8) {
    when (io.vstart_gte_vl) {
      maskTail(i) := 2.U
    }.elsewhen (tail(i)) {
      maskTail(i) := Mux(uop.info.ta, 3.U, 2.U)
    }.elsewhen (!mask(i) && !uop.ctrl.vm) {
      maskTail(i) := Mux(uop.info.ma, 3.U, 2.U)
    }.otherwise {
      maskTail(i) := 0.U
    }
  }
  io.maskKeep := Cat(maskTail.map(x => Mux(x(1), 0.U(8.W), ~(0.U(8.W)))).reverse)
  io.maskOff := Cat(maskTail.zipWithIndex.map({case (x, i) => 
                        Mux(!x(1), 0.U(8.W), Mux(x(0), ~0.U(8.W), UIntSplit(oldVd, 8)(i)))}).reverse)
}

class LaneVMac(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LaneFUInput))
    val redirect = Input(new Redirect)
    val out = Decoupled(new LaneFUOutput)
  })

  val uop = io.in.bits.uop
  val sew = SewOH(uop.info.vsew)  // 0:8, 1:16, 2:32, 3:64
  val eewVd = SewOH(uop.info.destEew)
  val funct6 = uop.ctrl.funct6

  // broadcast rs1 to all elements, assume xLen = 64
  val rs1_repeat = Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => Fill(64/n, io.in.bits.rs1(n-1, 0))))
  val vs1_rs1 = Mux(uop.ctrl.vv, io.in.bits.vs1, rs1_repeat)

  val vMac64b = Module(new VMac64b)
  vMac64b.io.sew := sew
  vMac64b.io.uopIdx := uop.expdIdx
  vMac64b.io.vxrm := uop.info.vxrm

  vMac64b.io.vs1 := vs1_rs1
  vMac64b.io.vs2 := Mux(funct6(4,2) === "b010".U, io.in.bits.old_vd, io.in.bits.vs2)
  vMac64b.io.oldVd := Mux(funct6(4,2) === "b010".U, io.in.bits.vs2, io.in.bits.old_vd)

  vMac64b.io.vs2_is_signed := !(funct6(4,0) === "b11111".U || funct6(1,0) === 0.U)
  vMac64b.io.vs1_is_signed := funct6(4,2) === "b001".U && funct6(0) || funct6(4,3) === "b01".U ||
                               funct6(4,3) === "b11".U && funct6(0)
  vMac64b.io.highHalf := funct6(4,2) === "b001".U && funct6(1,0) =/= 1.U
  vMac64b.io.isMacc := funct6(4,3) === "b01".U || funct6(4,2) === "b111".U
  vMac64b.io.isSub := funct6(4,3) === "b01".U && funct6(1,0) === "b11".U
  vMac64b.io.widen := funct6(4,3) === "b11".U
  vMac64b.io.isFixP := uop.ctrl.fixP
  
  val validVec = io.in.valid +: Seq.fill(3)(RegInit(false.B))
  val readyVec = Seq.fill(3)(Wire(Bool())) :+ io.out.ready
  val uopVec = io.in.bits.uop +: Seq.fill(3)(Reg(new VUop))
  val flushVec = validVec.zip(uopVec).map(x => x._1 && io.redirect.needFlush(x._2.robIdx))
  def regEnable(i: Int) = validVec(i - 1) && readyVec(i - 1) && !flushVec(i - 1)
  for (i <- 0 until 3) {
    readyVec(i) := readyVec(i + 1) || !validVec(i + 1)
  }
  for (i <- 1 to 3) {
    when (regEnable(i)) {
      validVec(i) := true.B
      uopVec(i) := uopVec(i - 1)
    }.elsewhen (readyVec(i) || flushVec(i)) {
      validVec(i) := false.B
    }
  }
  io.in.ready := readyVec(0)
  io.out.valid := validVec(3)
  io.out.bits.uop := uopVec(3)

  // //---- ready-valid S1
  // val validS1 = RegInit(false.B)
  // val readyS1 = Wire(Bool())
  // io.in.ready := !validS1 || readyS1
  // when (io.in.ready) { validS1 := io.in.valid } 
  // //---- ready-valid S2
  // val validS2 = RegInit(false.B)
  // val readyS2 = Wire(Bool())
  // val fireS1 = validS1 && readyS1
  // readyS1 := !validS2 || readyS2
  // when (readyS1) { validS2 := validS1 }
  // //---- ready-valid S3
  // val validS3 = RegInit(false.B)
  // val fireS2 = validS2 && readyS2
  // readyS2 := !validS3 || io.out.ready
  // when (readyS2) { validS3 := validS2 }
  // io.out.valid := validS3

  // val uopS1 = RegEnable(io.in.bits.uop, io.in.fire)
  // val uopS2 = RegEnable(uopS1, fireS1)
  // val uopS3 = RegEnable(uopS2, fireS2)
  // io.out.bits.uop := uopS3

  //------------------------------------
  //-------- Mask/Tail data gen --------
  //------------------------------------
  val maskS1 = RegEnable(io.in.bits.mask, regEnable(1)) // io.in.fire)
  val tailS1 = RegEnable(io.in.bits.tail, regEnable(1)) // io.in.fire)
  val oldVdS1 = RegEnable(io.in.bits.old_vd, regEnable(1)) // io.in.fire)
  val vstart_gte_vl_S1 = RegEnable(uop.info.vstart_gte_vl, regEnable(1))
  val eewVdS1 = RegEnable(eewVd, regEnable(1)) // io.in.fire)
  val maskS2 = RegEnable(maskS1, regEnable(2)) // fireS1)
  val tailS2 = RegEnable(tailS1, regEnable(2)) // fireS1)
  val oldVdS2 = RegEnable(oldVdS1, regEnable(2)) // fireS1)
  val vstart_gte_vl_S2 = RegEnable(vstart_gte_vl_S1, regEnable(2)) // fireS1)
  val eewVdS2 = RegEnable(eewVdS1, regEnable(2)) // fireS1)

  val maskSplash = MaskReorg.splash(maskS2, eewVdS2)
  val tailSplash = MaskReorg.splash(tailS2, eewVdS2)
  val maskTailData = Module(new MaskTailDataVMac)
  maskTailData.io.mask := maskSplash
  maskTailData.io.tail := tailSplash
  maskTailData.io.vstart_gte_vl := vstart_gte_vl_S2
  maskTailData.io.oldVd := oldVdS2
  maskTailData.io.uop := uopVec(2) // uopS2

  io.out.bits.vd := RegEnable(vMac64b.io.vd & maskTailData.io.maskKeep | maskTailData.io.maskOff, regEnable(3)) // fireS2)
  io.out.bits.vxsat := RegEnable(vMac64b.io.vxsat, regEnable(3)) // fireS2)
  io.out.bits.fflags := 0.U

  vMac64b.io.fireIn := regEnable(1) // io.in.fire
  vMac64b.io.fireS1 := regEnable(2) // fireS1
}
package darecreek.exu.lanevfu.mac

import chisel3._
import chisel3.util._
import darecreek.{LaneFUInput, LaneFUOutput, SewOH, VExpdUOp, UIntSplit, MaskReorg}
import darecreek.DarecreekParam._
import darecreek.exu.vfu.mac.VMac64b

// finalResult = result & maskKeep | maskOff
class MaskTailDataVMac extends Module {
  val io = IO(new Bundle {
    val mask = Input(UInt(8.W))
    val tail = Input(UInt(8.W))
    val oldVd = Input(UInt(64.W))
    val uop = Input(new VExpdUOp)
    val maskKeep = Output(UInt(64.W))  // keep: 11..1  off: 00..0
    val maskOff = Output(UInt(64.W))   // keep: 00..0  off: old_vd or 1.U
  })

  val maskTail = Wire(Vec(8, UInt(2.W))) // 00: keep result   10: old_vd(undisturbed)  11: write 1s(agnostic)
  val (mask, tail, oldVd, uop) = (io.mask, io.tail, io.oldVd, io.uop)
  for (i <- 0 until 8) {
    when (tail(i)) {
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

class LaneVMac extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LaneFUInput))
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
  
  //---- ready-valid S1
  val validS1 = RegInit(false.B)
  val readyS1 = Wire(Bool())
  io.in.ready := !io.in.valid || readyS1
  validS1 := Mux(io.in.fire, true.B, Mux(readyS1, false.B, validS1))
  //---- ready-valid S2
  val validS2 = RegInit(false.B)
  val readyS2 = Wire(Bool())
  readyS1 := !validS1 || readyS2
  val fireS1 = validS1 && readyS1
  validS2 := Mux(fireS1, true.B, Mux(readyS2, false.B, validS2))
  // io.out.valid := validS2
  //---- ready-valid S3
  val validS3 = RegInit(false.B)
  readyS2 := !validS2 || io.out.ready
  val fireS2 = validS2 && readyS2
  validS3 := Mux(fireS2, true.B, Mux(io.out.ready, false.B, validS3))
  io.out.valid := validS3

  val uopS1 = RegEnable(io.in.bits.uop, io.in.fire)
  val uopS2 = RegEnable(uopS1, fireS1)
  val uopS3 = RegEnable(uopS2, fireS2)
  io.out.bits.uop := uopS3

  //------------------------------------
  //-------- Mask/Tail data gen --------
  //------------------------------------
  val maskS1 = RegEnable(io.in.bits.mask, io.in.fire)
  val tailS1 = RegEnable(io.in.bits.tail, io.in.fire)
  val oldVdS1 = RegEnable(io.in.bits.old_vd, io.in.fire)
  val eewVdS1 = RegEnable(eewVd, io.in.fire)
  val maskS2 = RegEnable(maskS1, fireS1)
  val tailS2 = RegEnable(tailS1, fireS1)
  val oldVdS2 = RegEnable(oldVdS1, fireS1)
  val eewVdS2 = RegEnable(eewVdS1, fireS1)

  val maskSplash = MaskReorg.splash(maskS2, eewVdS2)
  val tailSplash = MaskReorg.splash(tailS2, eewVdS2)
  val maskTailData = Module(new MaskTailDataVMac)
  maskTailData.io.mask := maskSplash
  maskTailData.io.tail := tailSplash
  maskTailData.io.oldVd := oldVdS2
  maskTailData.io.uop := uopS2

  io.out.bits.vd := RegEnable(vMac64b.io.vd & maskTailData.io.maskKeep | maskTailData.io.maskOff, fireS2)
  io.out.bits.vxsat := RegEnable(vMac64b.io.vxsat, fireS2)
  io.out.bits.fflags := 0.U

  vMac64b.io.fireIn := io.in.fire
  vMac64b.io.fireS1 := fireS1
}
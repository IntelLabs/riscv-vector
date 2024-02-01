package darecreek.exu.vfucore.div

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.{VFuModule, VFuParamsKey, VFuParameters}
import darecreek.exu.vfucore.{LaneFUInput, LaneFUOutput, VFuInput, VFpuOutput}
import darecreek._
import darecreek.exu.vfucoreconfig.{VUop, Redirect}

class VDiv(implicit p: Parameters) extends VFuModule { //with RequireAsyncReset {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VFuInput))
    val redirect = Input(new Redirect)
    val out = Decoupled(new VFpuOutput)
  })

  val divTops = Seq.fill(NLanes)(Module(new DivTop))

  val uopIdx = io.in.bits.uop.uopIdx
  val veewVd = io.in.bits.uop.info.vsew
  val eewVd = SewOH(veewVd)
  //---- Tail gen ----
  val tail = TailGen(io.in.bits.uop.info.vl, uopIdx, eewVd)
  val laneTail = Wire(Vec(NLanes, UInt(NByteLane.W)))
  for (i <- 0 until NLanes) {
    laneTail(i) :=  Mux1H(eewVd.oneHot, Seq(1,2,4,8).map(bytes => UIntSplit(tail, NByteLane/bytes)(i)))
  }
  //---- Prestart gen ----
  val prestart = PrestartGen(io.in.bits.uop.info.vstart, uopIdx, eewVd)
  val lanePrestart = Wire(Vec(NLanes, UInt(NByteLane.W)))
  for (i <- 0 until NLanes) {
    lanePrestart(i) :=  Mux1H(eewVd.oneHot, Seq(1,2,4,8).map(bytes => UIntSplit(prestart, NByteLane/bytes)(i)))
  }
  //---- Mask gen ----
  val uopIdxOH = Seq.tabulate(8)(i => uopIdx === i.U)
  val mask = Wire(UInt(vlenb.W))
  mask := Mux1H(uopIdxOH, Seq.tabulate(8)(i => 
                Mux1H(eewVd.oneHot, Seq(1,2,4,8).map(k => UIntSplit(io.in.bits.mask, vlenb/k)(i)))))
  val laneMask = Wire(Vec(NLanes, UInt(NByteLane.W)))
  for (i <- 0 until NLanes) {
    laneMask(i) := Mux1H(eewVd.oneHot, Seq(1,2,4,8).map(k => UIntSplit(mask, NByteLane/k)(i)))
  }


  /** VFuInput -> LaneFuInput */
  for (i <- 0 until NLanes) {
    divTops(i).io.in.bits.uop := io.in.bits.uop
    divTops(i).io.in.bits.vs1 := UIntSplit(io.in.bits.vs1, 64)(i)
    divTops(i).io.in.bits.vs2 := UIntSplit(io.in.bits.vs2, 64)(i)
    divTops(i).io.in.bits.old_vd := UIntSplit(io.in.bits.oldVd, 64)(i)
    divTops(i).io.in.bits.rs1 := io.in.bits.rs1
    val prestartSplash = MaskReorg.splash(lanePrestart(i), eewVd)
    divTops(i).io.in.bits.prestart := UIntSplit(prestartSplash, 8)(i)
    val maskSplash = MaskReorg.splash(laneMask(i), eewVd)
    divTops(i).io.in.bits.mask := UIntSplit(maskSplash, 8)(i)
    val tailSplash = MaskReorg.splash(laneTail(i), eewVd)
    divTops(i).io.in.bits.tail := UIntSplit(tailSplash, 8)(i)
  }
  
  /** Flush gen */
  val currentNeedFlush = RegInit(false.B)
  val in_robIdx = io.in.bits.uop.robIdx
  val currentRobIdx = RegEnable(in_robIdx, io.in.fire)
  when (io.in.fire) {
    currentNeedFlush := io.redirect.needFlush(in_robIdx)
  }.otherwise {
    currentNeedFlush := currentNeedFlush || io.redirect.needFlush(currentRobIdx)
  }
  
  /** Valid out */
  val outValidReg = RegInit(VecInit.fill(NLanes)(false.B))
  val outValidFlush = RegInit(false.B)
  val performOutFlush = outValidReg.reduce(_ && _) && outValidFlush
  for (i <- 0 until NLanes) {
    divTops(i).io.out.ready := true.B
    when (divTops(i).io.out.valid) {
      outValidReg(i) := true.B
    }.elsewhen (io.out.fire || performOutFlush) {
      outValidReg(i) := false.B
    }
  }

  when (outValidReg.reduce(_ && _)) {
    outValidFlush := false.B
  }
  for (i <- 0 until NLanes) {
    when (divTops(i).io.out.valid) {
      outValidFlush := currentNeedFlush || io.redirect.needFlush(divTops(i).io.out.bits.uop.robIdx)
    }
  }
  io.out.valid := outValidReg.reduce(_ && _) && !outValidFlush

  /** io.in.ready */
  val isBusy = RegInit(false.B)
  when (io.in.fire) {
    isBusy := true.B
  }.elsewhen (io.out.fire || performOutFlush) {
    isBusy := false.B
  }
  for (i <- 0 until NLanes) {
    divTops(i).io.in.valid := io.in.valid && !isBusy
  }
  io.in.ready := divTops.map(_.io.in.ready).reduce(_ && _) && !isBusy

  /** vd,uop out */
  val outVdReg = Reg(Vec(NLanes, UInt(64.W)))
  val outfflagsReg = Reg(Vec(NLanes, UInt(5.W)))
  val outUopReg = Reg(new VUop)
  for (i <- 0 until NLanes) {
    when (divTops(i).io.out.valid) {
      outVdReg(i) := divTops(i).io.out.bits.vd
      outfflagsReg(i) := divTops(i).io.out.bits.fflags
    }
  }
  io.out.bits.vd := outVdReg.asUInt
  io.out.bits.fflags := outfflagsReg.reduce(_ | _)
  val outVUop = Wire(new VUop)
  outVUop := divTops(0).io.out.bits.uop
  when (divTops(0).io.out.valid) {
    outUopReg := outVUop
  }
  io.out.bits.uop := outUopReg
}
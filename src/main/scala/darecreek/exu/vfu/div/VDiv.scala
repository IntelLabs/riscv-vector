package darecreek.exu.vfu.div

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
// import darecreek.exu.vfu.VFUParam._
import org.chipsalliance.cde.config._
import xiangshan.Redirect

class VDiv(implicit p: Parameters) extends VFuModule { //with RequireAsyncReset {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VFuInput))
    val redirect = Input(ValidIO(new Redirect))
    val out = Decoupled(new VFpuOutput)
  })

  val divTops = Seq.fill(2)(Module(new DivTop))

  val uopIdx = io.in.bits.uop.uopIdx
  val veewVd = io.in.bits.uop.info.vsew
  val eewVd = SewOH(veewVd)
  //---- Tail gen ----
  val tail = TailGen(io.in.bits.uop.info.vl, uopIdx, eewVd)
  //---- Prestart gen ----
  val prestart = PrestartGen(io.in.bits.uop.info.vstart, uopIdx, eewVd)
  //---- Mask gen ----
  val mask16b = MaskExtract(io.in.bits.mask, uopIdx, eewVd)

  val tailReorg = MaskReorg.splash(tail, eewVd)
  val prestartReorg = MaskReorg.splash(prestart, eewVd)
  val mask16bReorg = MaskReorg.splash(mask16b, eewVd)

  /** VFuInput -> LaneFuInput */
  for (i <- 0 until 2) {
    // divTops(i).io.in.valid := io.in.valid
    divTops(i).io.in.bits.uop.connectFromVUop(io.in.bits.uop, isDiv = true)
    divTops(i).io.in.bits.vs1 := UIntSplit(io.in.bits.vs1, 64)(i)
    divTops(i).io.in.bits.vs2 := UIntSplit(io.in.bits.vs2, 64)(i)
    divTops(i).io.in.bits.old_vd := UIntSplit(io.in.bits.oldVd, 64)(i)
    divTops(i).io.in.bits.rs1 := io.in.bits.rs1
    divTops(i).io.in.bits.prestart := UIntSplit(prestartReorg, 8)(i)
    divTops(i).io.in.bits.mask := UIntSplit(mask16bReorg, 8)(i)
    divTops(i).io.in.bits.tail := UIntSplit(tailReorg, 8)(i)
  }
  
  /** LaneFUOutput -> VFpOutput */
  val outVdReg = Reg(Vec(2, UInt(64.W)))
  val outfflagsReg = Reg(Vec(2, UInt(5.W)))
  val outUopReg = Reg(new VUop)
  val outValidReg = RegInit(VecInit.fill(2)(false.B))
  val allOutValidRegClear = !outValidReg(0) && !outValidReg(1)
  
  io.out.valid := outValidReg(0) && outValidReg(1)

  val currentNeedFlush = RegInit(false.B)
  
  for (i <- 0 until 2) {
    divTops(i).io.out.ready := true.B
    when (divTops(i).io.out.valid) {
      // outValidReg(i) := true.B
      outValidReg(i) := !(currentNeedFlush || divTops(i).io.out.bits.uop.sysUop.robIdx.needFlush(io.redirect))
    }.elsewhen (io.out.fire) {
      outValidReg(i) := false.B
    }

    when (divTops(i).io.out.valid) {
      outVdReg(i) := divTops(i).io.out.bits.vd
      outfflagsReg(i) := divTops(i).io.out.bits.fflags
    }
  }
  io.out.bits.vd := Cat(outVdReg(1), outVdReg(0))
  io.out.bits.fflags := outfflagsReg(1) | outfflagsReg(0)
  val outVUop = Wire(new VUop)
  outVUop.connectFromLaneUop(divTops(0).io.out.bits.uop)
  when (divTops(0).io.out.valid) {
    outUopReg := outVUop
  }
  io.out.bits.uop := outUopReg

  val isBusy = RegInit(false.B)
  when (io.in.fire) {
    isBusy := true.B
  }.elsewhen (io.out.fire) {
    isBusy := false.B
  }
  // val outIsReady = io.out.fire || allOutValidRegClear
  for (i <- 0 until 2) {
    divTops(i).io.in.valid := io.in.valid && !isBusy
  }
  io.in.ready := divTops(0).io.in.ready && divTops(1).io.in.ready && !isBusy

  /**
   * Redirect handling
   */
  val in_robIdx = io.in.bits.uop.sysUop.robIdx
  val currentRobIdx = RegEnable(in_robIdx, io.in.fire)
  when (io.in.fire) {
    currentNeedFlush := in_robIdx.needFlush(io.redirect)
  }.otherwise {
    currentNeedFlush := currentNeedFlush || currentRobIdx.needFlush(io.redirect)
  }
}

import xiangshan._
object Main extends App {
  println("Generating hardware")
  val p = Parameters.empty.alterPartial({case XSCoreParamsKey => XSCoreParameters()})
  emitVerilog(new VDiv()(p.alterPartial({case VFuParamsKey => VFuParameters()})), Array("--target-dir", "generated",
              "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}
package darecreek.exu.vfu.div

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
// import darecreek.{LaneFUInput, LaneFUOutput}
// import darecreek.exu.fp.{LaneFUWithMaskIn, LaneFUWithMaskOut}
import darecreek.exu.vfu.{LaneFUInput, LaneFUOutput}
import darecreek.exu.vfu.fp.{LaneFUWithMaskIn, LaneFUWithMaskOut}

// time multiplex vector divider
// vs2 / vs1
class VIntSRT16TimeplexDivider(implicit p: Parameters) extends Module {

  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUWithMaskIn))
    val out = DecoupledIO(new LaneFUWithMaskOut)
  })
  // signal gen
  val uop = io.in.bits.uop
  val in_fire = io.in.valid && io.in.ready
  val dividend = io.in.bits.vs2
  val divisor = io.in.bits.vs1
  val vsew = uop.info.vsew

  val uopReg = RegEnable(uop, in_fire)
  val aReg = RegEnable(dividend, in_fire)
  val dReg = RegEnable(divisor, in_fire)
  val sewOH = UIntToOH(uopReg.info.vsew) // 0<->sew=8
  val isRem = uopReg.ctrl.funct6(1)
  val isSigned = uopReg.ctrl.funct6(0)

  val m = Module(new SRT16DividerDataModule(len=64))
  val uopsIdx, curUop = Wire(UInt(4.W))
  val resultIdx, curResult = Wire(UInt(4.W))
  val resultComplete = Wire(Bool())
  /* gen uops */
  // counter, starting from 0
  // State
  val busy = RegInit(false.B)
  when(!busy && in_fire) {
    busy := true.B
  } .elsewhen(busy && resultComplete) {
    busy := false.B
  }
  io.in.ready := !busy
  val uopsTotal = MuxLookup(vsew, 0.U, Seq(
    0.U -> 8.U,
    1.U -> 4.U,
    2.U -> 2.U,
    3.U -> 1.U
  ))

  val uopsCntReg = RegEnable(uopsIdx, in_fire || m.io.in_ready)
  uopsIdx := Mux(in_fire, uopsTotal, curUop )
  curUop := uopsCntReg -% 1.U

  /* gen srcs */
  // sew support 8/16/32/64
  def genSrcs(src: UInt, sew: Int): Vec[UInt] = {
    require(src.getWidth == 64)
    val ret = VecInit(Seq.tabulate(64 / sew){ i => src((i + 1) * sew - 1, i * sew) }.reverse)
    require(ret(0).getWidth == sew)
    ret
  }

  val a8Pre = genSrcs(aReg, 8)(curUop(2,0))
  val a16Pre = genSrcs(aReg, 16)(curUop(1,0))
  val a32Pre = genSrcs(aReg, 32)(curUop(0))
  val a64Pre = aReg
  val aPres = Seq(a8Pre, a16Pre, a32Pre, a64Pre)

  val d8Pre = genSrcs(dReg, 8)(curUop(2,0))
  val d16Pre = genSrcs(dReg, 16)(curUop(1,0))
  val d32Pre = genSrcs(dReg, 32)(curUop(0))
  val d64Pre = dReg
  val dPres = Seq(d8Pre, d16Pre, d32Pre, d64Pre)

  def extSrc(srcPre: UInt) = Mux(isSigned, SignExt(srcPre, 64), ZeroExt(srcPre, 64))
  val aSrcs = aPres.map(extSrc(_))
  val dSrcs = dPres.map(extSrc(_))

  /* compose results and output */
  /* Note that XS divider has a 1 cycle delay between valid and data signals*/
  val mValidReg = RegNext(m.io.out_valid)
  val resultCntReg = RegEnable(resultIdx, in_fire || mValidReg)
  resultIdx := Mux(in_fire, uopsTotal, curResult )
  curResult := resultCntReg -% 1.U
  resultComplete := curResult === 0.U && mValidReg

  val resultRegs = Reg(Vec(4, UInt(64.W)))  // 0 for sew=8
  val partialResult = Wire(Vec(4, UInt(64.W)))
  partialResult(0) := Cat(m.io.out_data(7, 0), resultRegs(0) >> 8)
  partialResult(1) := Cat(m.io.out_data(15, 0), resultRegs(1) >> 16)
  partialResult(2) := Cat(m.io.out_data(31, 0), resultRegs(2) >> 32)
  partialResult(3) := m.io.out_data

  when(mValidReg) {
    resultRegs.zip(partialResult).map(x => x._1 := x._2)
  }

  /* DATAMODULE and connection */

  m.io.src(0) := Mux1H(sewOH, aSrcs)
  m.io.src(1) := Mux1H(sewOH, dSrcs)
  m.io.isHi := isRem
  m.io.isW := false.B
  m.io.sign := isSigned
  m.io.kill_r := false.B
  m.io.kill_w := false.B
  m.io.out_ready := true.B //curResult =/= 0.U || curResult === 0.U && io.out.ready
  m.io.valid := busy
  io.out.bits.uop <> uopReg
  io.out.valid := resultComplete
  io.out.bits.vd := Mux1H(sewOH, partialResult)
  io.out.bits.vxsat := false.B
  io.out.bits.fflags := 0.U

}

// width=64
class VIntSRT16SpacePlexDivider(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUWithMaskIn))
    val out = DecoupledIO(new LaneFUWithMaskOut)
  })
  // signal gen
  val uop = io.in.bits.uop
  val in_fire = io.in.valid && io.in.ready
  val isRem = uop.ctrl.funct6(1)
  val isSigned = uop.ctrl.funct6(0)
  val sewSelOneHot = VecInit(Seq.tabulate(4)(i => i.U === uop.info.vsew)) // 0<->sew=8

  val dividers = Seq.tabulate(4)(i => Seq.fill(8>>i)(Module(new SRT16DividerDataModule(8<<i))))
  dividers.zipWithIndex.foreach(x => x._1.foreach{ m =>
    m.io.src(0) := io.in.bits.vs1 // a
    m.io.src(1) := io.in.bits.vs2 // d
    m.io.sign := isSigned
    m.io.isHi := isRem
    m.io.valid := x._2.U === sewSelOneHot(x._2)
    m.io.out_ready := io.out.ready

    m.io.kill_r := !m.io.in_ready  // FIXME
    m.io.kill_w := false.B // FIXME
    m.io.isW := false.B  // FIXME
  })

  // output
  io.in.ready := dividers.flatMap(_.map(_.io.in_ready)).reduce(_&_)
  io.out.bits.uop := RegEnable(uop, in_fire)
  // mux
  io.out.valid := Mux1H(
    sewSelOneHot,
    dividers.map(ms => ms.map(_.io.out_valid).reduce(_&_))
  )
  io.out.bits.vd := Mux1H(
    sewSelOneHot,
    dividers.map(ms => Cat(ms.map(_.io.out_data))) // FIXME: if reverse?
  )

}


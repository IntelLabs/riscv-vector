package darecreek.exu.vfucore.fp

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import darecreek.exu.vfucore.fp.fudian.FloatPoint


// cmp & minmax
class VFCMP(val expWidth: Int, val precision: Int) extends Module {
  val io = IO(new Bundle() {
    val a, b = Input(UInt((expWidth + precision).W))
    val cmd = Input(UInt(3.W))   // eq 000, ne 100, le 001, lt 011, gt 101, ge 111
                                // 110 for max, 100 for min
    val minmaxResult = Output(UInt((expWidth + precision).W))
    val cmpResult = Output(Bool())
    val cmpInvalid, minmaxInvalid = Output(Bool())
  })
  def signaling: Bool = io.cmd(0) | io.cmd(1)  // not ne or eq
  def EQ: UInt = 0.U(3.W)
  def NE: UInt = 4.U(3.W)
  def LE: UInt = 1.U(3.W)
  def LT: UInt = 3.U(3.W)
  def GT: UInt = 5.U(3.W)
  def GE: UInt = 7.U(3.W)
  def MAXOp = io.cmd(1)

  val (a, b) = (io.a, io.b)
  val fp_a = FloatPoint.fromUInt(a, expWidth, precision)
  val fp_b = FloatPoint.fromUInt(b, expWidth, precision)
  val decode_a = fp_a.decode
  val decode_b = fp_b.decode

  val hasNaN = decode_a.isNaN || decode_b.isNaN
  val bothNaN = decode_a.isNaN && decode_b.isNaN
  val hasSNaN = decode_a.isSNaN || decode_b.isSNaN
  val bothZero = decode_a.isZero && decode_b.isZero

  val same_sign = fp_a.sign === fp_b.sign
  val a_minus_b = Cat(0.U(1.W), a) - Cat(0.U(1.W), b)
  val uint_eq = a_minus_b.tail(1) === 0.U
  val uint_less = fp_a.sign ^ a_minus_b.head(1).asBool
  val invalid = hasSNaN || ((io.cmd(0) | io.cmd(1)) && hasNaN )  // not ne or eq

  val le = Mux(same_sign, uint_less || uint_eq, fp_a.sign || bothZero)
  val lt = Mux(same_sign, uint_less && !uint_eq, fp_a.sign && !bothZero)
  val gt = Mux(same_sign, !uint_less && !uint_eq, !fp_a.sign && !bothZero)
  val ge = Mux(same_sign, !uint_less || uint_eq, !fp_a.sign || bothZero)
  val eq = uint_eq || bothZero
  val ne = !uint_eq && !bothZero
  io.cmpResult := false.B // init
  switch(io.cmd) {
    is(EQ) {io.cmpResult := !hasNaN && eq}
    is(NE) {io.cmpResult :=  hasNaN || ne}
    is(LT) {io.cmpResult := !hasNaN && lt}
    is(LE) {io.cmpResult := !hasNaN && le}
    is(GT) {io.cmpResult := !hasNaN && gt}
    is(GE) {io.cmpResult := !hasNaN && ge}
  }
  io.cmpInvalid := invalid

  // difference comparing to cmp insts:
  // +0 == -0, but max insts should output +0
//  val minmaxSelA = decode_b.isNaN ||
//                   (!decode_a.isNaN && (lt ^ MAXOp)) ||
//                   (bothZero && (fp_a.sign ^ MAXOp))

  val minmaxSelA = decode_b.isNaN ||
                   Mux(bothZero, fp_a.sign ^ MAXOp,  (!decode_a.isNaN && (lt ^ MAXOp)))

  io.minmaxResult := Mux(
    bothNaN,
    FloatPoint.defaultNaNUInt(expWidth, precision),
    Mux(minmaxSelA, io.a, io.b)
  )
  io.minmaxInvalid := hasSNaN
}

class VFMiscDataModule(implicit val p: Parameters) extends VFPUPipelineModule {
  def latency = 2  // 2 stages
  class DataOut extends Bundle {
    val result = UInt(laneWidth.W)
    val fflags = UInt(5.W)
    def set(result: UInt, fflags: UInt) = {
      val b = Wire(new DataOut)
      b.result := result
      b.fflags := fflags
      b
    }
  }
  val vs1 = S1Reg(io.in.bits.vs1)
  val vs2 = S1Reg(io.in.bits.vs2)
  val mask = S1Reg(io.in.bits.mask)
  val uop = uopVec(1)
  val fpCtrl = uop.vfpCtrl
  val typeTagIn = uop.typeTag
  val eleActives = S1Reg(VecInit(Seq(0,4).map(isActive)))
  val narrow_eleActives = S1Reg(VecInit(Seq(0,1).map(isActive)))

  // sign injection & min/max (f2f)
  val signs = Seq(63,31).map( i => Mux(fpCtrl.miscSubCmd(1), vs1(i) ^ vs2(i), Mux(fpCtrl.miscSubCmd(0), ~vs1(i), vs1(i))))
  val sgnjResult = Cat(
    signs.head, vs2(62,32),
    Mux(typeTagIn === VFPU.S, signs(1), vs2(31)), vs2(30,0)
  )
  val sgnjOut = (new DataOut).set(sgnjResult, empty_fflags)

  // cmp & min/max. Note: dest of cmp is mask reg, thus should be tail agnositic
  val scmp1 = Module(new VFCMP(VFPU.f32.expWidth, VFPU.f32.precision))
  val scmp2 = Module(new VFCMP(VFPU.f32.expWidth, VFPU.f32.precision))
  val dcmp = Module(new VFCMP(VFPU.f64.expWidth, VFPU.f64.precision))
  Seq(scmp1, scmp2, dcmp).foreach { fcmp =>
    fcmp.io.a := vs2
    fcmp.io.b := vs1
    fcmp.io.cmd := fpCtrl.miscSubCmd
  }
  scmp1.io.a := vs2.head(32)
  scmp1.io.b := vs1.head(32)  // override
  val minmaxResult = WireInit(dcmp.io.minmaxResult)
  val cmpResult = WireInit(Cat(~0.U(63.W), dcmp.io.cmpResult)) //  extend to 64 bit
  val minmaxFlags = WireInit(Cat(dcmp.io.minmaxInvalid & eleActives(0), 0.U(4.W)))
  val cmpFlags = WireInit(Cat(dcmp.io.cmpInvalid & narrow_eleActives(0), 0.U(4.W)))
  // override when fp32
  when(typeTagIn === VFPU.S) {
    minmaxResult := Cat(scmp1.io.minmaxResult, scmp2.io.minmaxResult)
    cmpResult := Cat(~0.U(62.W), scmp1.io.cmpResult, scmp2.io.cmpResult)
    minmaxFlags := Cat(Seq(scmp1, scmp2).zip(Seq(1,0)).
      map(x => x._1.io.minmaxInvalid & eleActives(x._2)).reduce(_|_), 0.U(4.W))
    cmpFlags := Cat(Seq(scmp1, scmp2).zip(Seq(1,0)).
      map(x => x._1.io.cmpInvalid & narrow_eleActives(x._2)).reduce(_|_), 0.U(4.W))
  }


  val cmpOut = (new DataOut).set(cmpResult, cmpFlags)
  val minmaxOut = (new DataOut).set(minmaxResult, minmaxFlags)

  // classify does not output fflags
  val clsResult = Mux(typeTagIn === VFPU.S,
    Cat(Seq(vs2.head(32), vs2.tail(32)).map(x => Cat(0.U(22.W),classify(x, VFPU.f32)))),
    Cat(0.U(54.W), classify(vs2, VFPU.f64))
  )
  require(clsResult.getWidth == 64)
  val clsOut = (new DataOut).set(clsResult, empty_fflags)

  // classify
  def classify(x: UInt, ftype: VFPU.FType): UInt = {
    val float = fudian.FloatPoint.fromUInt(x, ftype.expWidth, ftype.precision)
    val decode = float.decode
    val isNormal = !decode.expIsOnes && !decode.expIsZero
    Cat(
      decode.isQNaN,
      decode.isSNaN,
      decode.isInf && !float.sign,
      isNormal && !float.sign,
      decode.isSubnormal && !float.sign,
      decode.isZero && !float.sign,
      decode.isZero && float.sign,
      decode.isSubnormal && float.sign,
      isNormal && float.sign,
      decode.isInf && float.sign
    )
  }

  // merge/mv  (f2f)
  // rs1 is pre-expanded to fill vs1 slot before entering data modules
  // merge has different mask policy, but same tail/prestart policy
  val mergeResult = Seq.fill(2)(Wire(UInt(32.W)))  // default vfmv.v.f
  mergeResult(1) := Mux(!uop.ctrl.vm && !mask(0), vs2.tail(32), vs1.tail(32))  // tail32b
  mergeResult(0) := Mux(
    !uop.ctrl.vm && ((typeTagIn === VFPU.D && !mask(0)) || (typeTagIn === VFPU.S && !mask(4))),
    vs2.head(32),
    vs1.head(32)
  )
  val mergeOut = (new DataOut).set(Cat(mergeResult), empty_fflags)

  // output muxes
  val muxedOut = Mux1H(fpCtrl.miscCmd.asBools, Seq(mergeOut, minmaxOut, sgnjOut, cmpOut, clsOut)) // Seq: LSB to MSB
  io.out.bits.vd := S2Reg(muxedOut.result)
  io.out.bits.fflags := S2Reg(muxedOut.fflags)
  // delay 1 cycle to match the timing of output
  io.out.bits.uop := uopVec.last
  io.out.valid := validVec.last

}

class VFMisc (implicit val p: Parameters) extends VFPUSubModule {
  val misc = Module(new VFMiscDataModule)
  misc.io.in <> io.in
  misc.io.redirect := io.redirect
  // block if is not misc,
  misc.io.in.valid := io.in.valid && io.in.bits.uop.vfpCtrl.isMisc  // idle when inst is not misc

  io.out <> misc.io.out
//  // IMPORTANT: compose results of compare uop when LMUL > 1. This is required from VPU Control/Issue design
//  val outUop = module.io.out.bits.uop
//  val compareReg = RegInit(~0.U(64.W))  // init all 1
//  val compareResult = Wire(UInt(64.W))
//  compareResult := (compareReg << 8)(63,0) | Cat(0.U(56.W), module.io.out.bits.vd(7,0))
//  when(outUop.vfpCtrl.miscCmd(3) & module.io.out.valid) {
//    when(!outUop.expdEnd) {
//      io.out.valid := false.B
//    }
//    compareReg := compareResult
//    io.out.bits.vd := compareResult
//  }

}


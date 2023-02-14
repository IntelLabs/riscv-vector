package vfu.alu

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import yunsuan._
import vfu._

class SewOH extends Bundle {  // 0   1   2   3
  val oneHot = Vec(4, Bool()) // 8, 16, 32, 64
  def is8 = oneHot(0)
  def is16 = oneHot(1)
  def is32 = oneHot(2)
  def is64 = oneHot(3)
}

class Reduction extends Module {
  val VLEN = 128
  val xLen = 64
  val LaneWidth = 64
  val NLanes = VLEN/64
  val vlenb = VLEN/8
  val io = IO(new Bundle() {
    val vs1           = Input (UInt(VLEN.W))
    val vs2           = Input (UInt(VLEN.W))
    val vmask         = Input (UInt(VLEN.W))
    val old_vd        = Input (UInt(VLEN.W))
    val srcType       = Input (Vec(2, VectorElementFormat()))  
    val vdType        = Input (VectorElementFormat())  
    val op_code       = Input (OpType())        
    val info          = Input(new VIFuInfo)
    val valid         = Input(Bool())
    val vd            = Output(UInt(VLEN.W))
  })


val (vs1, vs2, vmask, old_vd) = (io.vs1, io.vs2, io.vmask, io.old_vd)

val vm     = io.info.vm     
val ma     = io.info.ma     
val ta     = io.info.ta     
val lmul   = io.info.vlmul   
val vl     = io.info.vl     
val uopIdx = io.info.uopIdx 
val fire   = io.valid
val vsew   = io.srcType(1)(1,0)
val signed = io.srcType(1)(2).asBool
val widen  = io.vdType(1,0) === (io.srcType(1)(1,0) + 1.U) 
val vsew_bytes = 1.U << vsew;
val vsew_bits = 8.U << vsew;

val vredsum_vs     = io.op_code === VipuType.vredsum
val vredmax_vs     = (io.op_code === VipuType.vredmax) && io.srcType(1)(2).asBool
val vredmaxu_vs    = (io.op_code === VipuType.vredmax) && !io.srcType(1)(2).asBool
val vredmin_vs     = (io.op_code === VipuType.vredmin) && io.srcType(1)(2).asBool  
val vredminu_vs    = (io.op_code === VipuType.vredmin) && !io.srcType(1)(2).asBool 
val vredand_vs     = io.op_code === VipuType.vredand
val vredor_vs      = io.op_code === VipuType.vredor
val vredxor_vs     = io.op_code === VipuType.vredxor
val vwredsum_vs    = (io.op_code === VipuType.vredsum) && io.srcType(1)(2).asBool && (io.vdType(1,0) === (io.srcType(1)(1,0) + 1.U)) 
val vwredsumu_vs   = (io.op_code === VipuType.vredsum) && !io.srcType(1)(2).asBool && (io.vdType(1,0) === (io.srcType(1)(1,0) + 1.U)) 

def zero(w: Int) = 0.U(w.W)
def umax(w: Int) = ~(0.U(w.W))
def smax(w: Int) = Cat(0.U(1.W), ~(0.U((w-1).W)))
def smin(w: Int) = Cat(1.U(1.W), 0.U((w-1).W))

val ele64 = Wire(UInt(64.W))
val sew = Wire(new SewOH)  // 0:8, 1:16, 2:32, 3:64
sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U)) 

ele64 := 0.U
when (fire) {
  when (vredmax_vs) {
    ele64 := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => smin(n))) 
  } .elsewhen (vredmin_vs) {
    ele64 := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => smax(n))) 
  } .elsewhen (vredminu_vs || vredand_vs) {
    ele64 := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => umax(n))) 
  } .elsewhen (vredminu_vs || vredand_vs) {
    ele64 := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => umax(n))) 
  } 
}

val ele_cnt = vlenb.U/vsew_bytes
val vmask_bits = vmask >> (ele_cnt * uopIdx)
val vlRemain = Mux((vl >= (ele_cnt * uopIdx)), (vl - (ele_cnt * uopIdx)), 0.U) 
val vlRemainBytes = vlRemain << vsew

val vs2_bytes = VecInit(Seq.tabulate(vlenb)(i => vs2((i+1)*8-1, i*8)))
val vs2m_bytes = Wire(Vec(vlenb, UInt(8.W)))
val vs2m_bits = Cat(vs2m_bytes.reverse)

for (i<-0 until vlenb) {
  vs2m_bytes(i) := vs2_bytes(i)
  when ((!vm && !vmask_bits(i.U/vsew_bytes)) || (i.U >= vlRemainBytes)) {
    when (vsew === 0.U) {
      vs2m_bytes(i) := ele64(7, 0) 
    } .elsewhen (vsew === 1.U) {
      vs2m_bytes(i) := ele64((i%2+1)*8-1, (i%2)*8) 
    } .elsewhen (vsew === 2.U) {
      vs2m_bytes(i) := ele64((i%4+1)*8-1, (i%4)*8) 
    } .elsewhen (vsew === 3.U) {
      vs2m_bytes(i) := ele64((i%8+1)*8-1, (i%8)*8) 
    } 
   }
}

def widenPad(x: UInt) = {
  val len = x.getWidth
  Cat(Fill(len-2, x(len-1) && signed), x)
}
 
class Adder_8b(in1: UInt, in2: UInt, cin: UInt) {
  private val bits = Cat(0.U(1.W), in1, cin) +
                     Cat(0.U(1.W), in2, cin)
  val (cout, out) = (bits(9), bits(8, 1))
}

val vs1_zero = Wire(UInt(64.W)) 
val cin0  = Wire(Vec(8, Bool()))
val cout0 = Wire(Vec(8, Bool()))
val vd  = Wire(Vec(5, Vec(8, UInt(8.W))))
val destEew_oneHot = Seq.tabulate(4)(i => io.vdType(1,0) === i.U)

vs1_zero := Mux1H(destEew_oneHot, Seq(8, 16, 32).map(n => Cat(Fill(LaneWidth-n, 0.U), vs1(n-1,0))) :+ vs2(63,0)) 

for (i <- 0 until 5) {
  for (j <- 0 until 8) {
    vd(i)(j) := 0.U
  }
}

for (i <- 0 until 8) {
  val adder_8b = new Adder_8b(vs2m_bits(8*i+7, 8*i), vs2m_bits(64+8*i+7, 8*i), cin0(i))
  cin0(i) := Mux1H(sew.oneHot, Seq(1, 2, 4, 8).map(n => 
    if ((i % n) == 0) 0.U else cout0(i-1))
  )
  cout0(i) := adder_8b.cout
  vd(0)(i) := adder_8b.out
}

val vd0_widen = Wire(UInt(VLEN.W))
val vi1 = Mux(widen, vd0_widen, Cat(Mux(vsew === 3.U, vs1_zero, 0.U(64.W)), Cat(vd(0).reverse))) 
val cin1  = Wire(Vec(8, Bool()))
val cout1 = Wire(Vec(8, Bool()))

vd0_widen := MuxCase(Cat(widenPad(Cat(cout0(7), vd(0)(7))),
                         widenPad(Cat(cout0(6), vd(0)(6))), 
                         widenPad(Cat(cout0(5), vd(0)(5))), 
                         widenPad(Cat(cout0(4), vd(0)(4))), 
                         widenPad(Cat(cout0(3), vd(0)(3))), 
                         widenPad(Cat(cout0(2), vd(0)(2))), 
                         widenPad(Cat(cout0(1), vd(0)(1))), 
                         widenPad(Cat(cout0(0), vd(0)(0)))), 
  Seq(
  sew.is16 -> Cat(widenPad(Cat(cout0(7), vd(0)(7), vd(0)(6))),
                  widenPad(Cat(cout0(5), vd(0)(5), vd(0)(4))), 
                  widenPad(Cat(cout0(3), vd(0)(3), vd(0)(2))), 
                  widenPad(Cat(cout0(1), vd(0)(1), vd(0)(0)))), 
  sew.is32 -> Cat(widenPad(Cat(cout0(7), vd(0)(7), vd(0)(6), vd(0)(5), vd(0)(4))),
                  widenPad(Cat(cout0(3), vd(0)(3), vd(0)(2), vd(0)(1), vd(0)(0)))) 
))

for (i <- 0 until 8) {
  val adder_8b = new Adder_8b(vi1(8*i+7, 8*i), vi1(64+8*i+7, 8*i), cin1(i))
  cin1(i) := Mux1H(destEew_oneHot, Seq(1, 2, 4, 8).map(n => 
    if ((i % n) == 0) 0.U else cout1(i-1))
  )
  cout1(i) := adder_8b.cout
  vd(1)(i) := adder_8b.out
}

val vi2 = Cat(Mux(vsew === 2.U, vs1_zero, 0.U(64.W)), Cat(vd(1).reverse)) 
val cin2  = Wire(Vec(8, Bool()))
val cout2 = Wire(Vec(8, Bool()))

for (i <- 0 until 8) {
  val adder_8b = new Adder_8b(vi2(8*i+7, 8*i), vi2(64+8*i+7, 8*i), cin2(i))
  cin2(i) := Mux1H(destEew_oneHot, Seq(1, 2, 4, 8).map(n => 
    if ((i % n) == 0) 0.U else cout2(i-1))
  )
  cout2(i) := adder_8b.cout
  vd(2)(i) := adder_8b.out
}

val vi3 = Cat(Mux(vsew === 1.U, vs1_zero(31,0), 0.U(32.W)), Cat(vd(2).reverse)(31,0)) 
val cin3  = Wire(Vec(4, Bool()))
val cout3 = Wire(Vec(4, Bool()))

for (i <- 0 until 4) {
  val adder_8b = new Adder_8b(vi3(8*i+7, 8*i), vi3(32+8*i+7, 8*i), cin3(i))
  cin3(i) := Mux1H(destEew_oneHot, Seq(1, 2, 4, 8).map(n => 
    if ((i % n) == 0) 0.U else cout3(i-1))
  )
  cout3(i) := adder_8b.cout
  vd(3)(i) := adder_8b.out
}

val vi4 = Cat(Mux(vsew === 0.U, vs1_zero(15,0), 0.U(16.W)), Cat(vd(3).reverse)(15,0)) 
val cin4  = Wire(Vec(2, Bool()))
val cout4 = Wire(Vec(2, Bool()))

for (i <- 0 until 2) {
  val adder_8b = new Adder_8b(vi4(8*i+7, 8*i), vi4(16+8*i+7, 8*i), cin4(i))
  cin4(i) := Mux1H(destEew_oneHot, Seq(1, 2, 4, 8).map(n => 
    if ((i % n) == 0) 0.U else cout4(i-1))
  )
  cout4(i) := adder_8b.cout
  vd(4)(i) := adder_8b.out
}


val sum_vd = Cat(0.U((VLEN-64).W), Cat(vd(io.srcType(1)(1,0) +1.U).reverse))

val vd_mask = (~0.U(VLEN.W))
val red_vd_tail_one = (vd_mask << vsew_bits) | (sum_vd & (vd_mask >> (VLEN.U - vsew_bits)))
val red_vd_tail_vd = (old_vd & (vd_mask << vsew_bits)) | (sum_vd & (vd_mask >> (VLEN.U - vsew_bits)))

val red_vd = Mux(ta, red_vd_tail_one, red_vd_tail_vd)
val vd_reg = RegInit(0.U(VLEN.W))

when ((io.op_code === VipuType.vredsum) && fire) {
  vd_reg := red_vd
}


io.vd := vd_reg

// /**
//   * Integer Compare & Min/Max instructions
//   */
// val lessThan_vec = Wire(Vec(8, Bool()))
// val equal_vec = Wire(Vec(8, Bool()))
// for (i <- 0 until 8) {
//   lessThan_vec(i) := Mux(signed, (vs2(8*i+7) ^ vs1_adjust(8*i+7)) ^ cout(i), !cout(i))
//   equal_vec(i) := vs2(8*i+7, 8*i) === vs1(8*i+7, 8*i)
// }
// val equal = Cat(equal_vec.reverse)
// val cmpEq = Mux1H(Seq(
//   sew.is8  -> equal,
//   sew.is16 -> Cat(Fill(2, equal(7, 6).andR), Fill(2, equal(5, 4).andR), Fill(2, equal(3, 2).andR), Fill(2, equal(1, 0).andR)),
//   sew.is32 -> Cat(Fill(4, equal(7, 4).andR), Fill(4, equal(3, 0).andR)),
//   sew.is64 -> Fill(8, equal.andR)
// ))
// val cmpNe = ~cmpEq
// val lessThan = Cat(lessThan_vec.reverse)
// val cmpResult = Mux1H(Seq(
//   (funct6 === "b011000".U) -> cmpEq,
//   (funct6 === "b011001".U) -> cmpNe,
//   (funct6(5,1) === "b01101".U) -> lessThan,
//   (funct6(5,1) === "b01110".U) -> (lessThan | cmpEq),
//   (funct6(5,1) === "b01111".U) -> ~(lessThan | cmpEq)
// ))
// 
// //-------- Min/Max --------
// val minMaxResult = Wire(Vec(8, UInt(8.W)))
// val selectVs1 = lessThan_vec.map(_ === funct6(1))
// for (i <- 0 until 8) {
//   val sel = Mux1H(Seq(
//     sew.is8  -> selectVs1(i),
//     sew.is16 -> selectVs1((i/2)*2+1),
//     sew.is32 -> selectVs1((i/4)*4+3),
//     sew.is64 -> selectVs1(7),
//   ))
//   minMaxResult(i) := Mux(sel, vs1(8*i+7, 8*i), vs2(8*i+7, 8*i))
// }
// 
// io.out.vd := Mux(funct6(5, 2) === "b0001".U, Cat(minMaxResult.reverse), Cat(vd.reverse))
// 
// val cmpOut = Mux(addWithCarry, Cat(cout.reverse), cmpResult)
// io.out.cmp := Mux1H(Seq(
//   sew.is8  -> cmpOut,
//   sew.is16 -> Cat(~(0.U(4.W)), cmpOut(7), cmpOut(5), cmpOut(3), cmpOut(1)),
//   sew.is32 -> Cat(~(0.U(6.W)), cmpOut(7), cmpOut(3)),
//   sew.is64 -> Cat(~(0.U(7.W)), cmpOut(7))
// ))




}

object VerilogRed extends App {
  println("Generating the VPU Reduction hardware")
  emitVerilog(new Reduction(), Array("--target-dir", "generated"))
}




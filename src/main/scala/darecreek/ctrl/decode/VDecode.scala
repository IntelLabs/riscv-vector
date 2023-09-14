package darecreek

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import darecreek.ctrl.decode._

class VDecode extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(new VCtrl)
  })

  val vCtrl = Wire(new VCtrl)

  //-- Directly extracted from instruction --
  vCtrl.lsrc(0) := io.in(19, 15)
  vCtrl.lsrc(1) := io.in(24, 20)
  vCtrl.ldest := io.in(11, 7)
  vCtrl.vm := io.in(25)
  vCtrl.funct6 := io.in(31, 26)
  vCtrl.funct3 := io.in(14, 12)

  //-- Decoded signals --
  val vecDecode = Seq((new VecDecode0), (new VecDecode1), (new VecDecode2), 
                      (new VecDecode3), (new VecDecode4), (new VecDecode5))
  val decodersOut = vecDecode map { dec =>
    val truthTable = TruthTable(dec.table, dec.default)
    decoder(QMCMinimizer, io.in, truthTable)
  }

  val vClasses = decodersOut.map(x => x(x.getWidth - 1))
  vCtrl.illegal := !(vClasses.reduce(_ || _))
  val vCtrlSigs = Mux1H(vClasses, decodersOut)

  val ctrls = Seq(vCtrl.lsrcVal(2), vCtrl.lsrcVal(1), vCtrl.lsrcVal(0),
                  vCtrl.ldestVal, vCtrl.rdVal,
                  vCtrl.load, vCtrl.store,
                  vCtrl.arith, vCtrl.crossLane,
                  vCtrl.alu, vCtrl.mul, vCtrl.fp, vCtrl.div, 
                  vCtrl.fixP, vCtrl.redu, vCtrl.mask, vCtrl.perm,
                  vCtrl.widen, vCtrl.widen2, vCtrl.narrow, vCtrl.narrow_to_1
                 )
  ctrls zip vCtrlSigs.asBools.reverse.tail map {case (c, d) => c := d}

  io.out := vCtrl
}


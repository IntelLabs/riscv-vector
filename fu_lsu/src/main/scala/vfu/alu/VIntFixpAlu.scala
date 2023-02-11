/**
  * Integer and fixed-point (except mult and div)
  *   
  * Perform below instructions:
  *     11.1  vadd, ...
  *     11.2  vwadd, ...
  *     11.3  vzext, ...
  *     11.4  vadc, vmadc, ...
  *     11.5  vand, ...
  *     11.6  vsll, ...
  *     11.7  vnsrl, ...
  *     11.8  vmseq, vmsltu, ...
  *     11.9  vminu, ...
  *     11.15 vmerge
  *     11.16 vmv.v.
  *     12.1  vsadd, ...
  *     12.2  vaadd, ...
  *     12.4  vssrl, ...
  *     12.5  vnclip, ...
  */

package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import yunsuan.vector.{VIFuInfo, VIFuInput, VIFuOutput, SewOH, UIntSplit}

class VIntFixpDecode extends Bundle {
  val sub = Bool()
  val misc = Bool()
}

class VIntFixpAlu64b extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(6.W))
    val info = Input(new VIFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val vmask = Input(UInt(8.W))
    val is_sub = Input(Bool())  // subtract
    val isMisc = Input(Bool())

    val vd = Output(UInt(64.W))
  })

  val vIntAdder64b = Module(new VIntAdder64b)
  vIntAdder64b.io.opcode := io.opcode 
  vIntAdder64b.io.info := io.info
  vIntAdder64b.io.srcType := io.srcType
  vIntAdder64b.io.vdType := io.vdType
  vIntAdder64b.io.vs1 := io.vs1
  vIntAdder64b.io.vs2 := io.vs2
  vIntAdder64b.io.vmask := io.vmask
  vIntAdder64b.io.is_sub := io.is_sub

  val vIntMisc64b = Module(new VIntMisc64b)
  vIntMisc64b.io.opcode := io.opcode 
  vIntMisc64b.io.info := io.info
  vIntMisc64b.io.srcType := io.srcType
  vIntMisc64b.io.vdType := io.vdType
  vIntMisc64b.io.vs1 := io.vs1
  vIntMisc64b.io.vs2 := io.vs2
  vIntMisc64b.io.vmask := io.vmask

  val vdAdderS1 = RegNext(vIntAdder64b.io.vd)
  val vdMiscS1 = RegNext(vIntMisc64b.io.vd)
  val isMiscS1 = RegNext(io.isMisc)

  io.vd := Mux(isMiscS1, vdMiscS1, vdAdderS1)
}


class VIntFixpAlu extends Module {
  val io = IO(new Bundle {
    val in = Input(new VIFuInput)
    val out = Output(new VIFuOutput)
  })

  val opcode = io.in.opcode

  val truthTable = TruthTable(VIntFixpMISC.table, VIntFixpMISC.default)
  val decoderOut = decoder(QMCMinimizer, Cat(opcode), truthTable)
  val vIntFixpDecode = decoderOut.asTypeOf(new VIntFixpDecode)

  //------- Two 64b modules form one 128b unit ------
  val vIntFixpAlu64bs = Seq.fill(2)(Module(new VIntFixpAlu64b))
  for (i <- 0 until 2) {
    vIntFixpAlu64bs(i).io.opcode := opcode
    vIntFixpAlu64bs(i).io.info := io.in.info
    vIntFixpAlu64bs(i).io.srcType := io.in.srcType
    vIntFixpAlu64bs(i).io.vdType := io.in.vdType
    vIntFixpAlu64bs(i).io.is_sub := vIntFixpDecode.sub
    vIntFixpAlu64bs(i).io.isMisc := vIntFixpDecode.misc
    // !!!! TODO: support widen !!!!
    vIntFixpAlu64bs(i).io.vs1 := UIntSplit(io.in.vs1, 64)(i)
    vIntFixpAlu64bs(i).io.vs2 := UIntSplit(io.in.vs2, 64)(i)
  }
  for (i <- 0 until 2) {
    vIntFixpAlu64bs(i).io.vmask := io.in.mask(7, 0) //!!!! Todo: Incorrect!!!!
  }

  io.out.vd := Cat(vIntFixpAlu64bs.map(_.io.vd).reverse) //!!!! Todo: Incorrect!!
  io.out.vxsat := false.B //!!!! Todo: Incorrect!!
}
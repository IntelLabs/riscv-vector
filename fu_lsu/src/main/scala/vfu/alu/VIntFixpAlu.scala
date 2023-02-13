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
    val isSub = Input(Bool())  // subtract
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
  vIntAdder64b.io.isSub := io.isSub

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
  val srcTypeVs2 = io.in.srcType(0)
  val srcTypeVs1 = io.in.srcType(1)
  val vdType = io.in.vdType
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val uopIdx = io.in.info.uopIdx
  val signed = srcTypeVs2(3, 2) === 1.U
  val widen = opcode(5, 1) === 0.U && srcTypeVs1(1, 0) =/= srcTypeVs2(1, 0)

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
    vIntFixpAlu64bs(i).io.isSub := vIntFixpDecode.sub
    vIntFixpAlu64bs(i).io.isMisc := vIntFixpDecode.misc
  }
  //---- Widen vs1 & vs2 ----
  def widenPad(x: UInt) = {
    val len = x.getWidth
    Cat(Fill(len, x(len-1) && signed), x)
  }
  val vs = Seq(io.in.vs1, io.in.vs2)
  val vs_widen = Wire(Vec(2, UInt(128.W)))
  val widenCase = Seq(widen, {widen && srcTypeVs2(1, 0) =/= vdType(1, 0)})
  for (i <- 0 until 2) { // 0: vs1,  1: vs2
    val vs_64b = Mux(uopIdx(0), vs(i)(127, 64), vs(i)(63, 0))
    when (widenCase(i)) {
      vs_widen(i) := Mux1H(eewVs1.oneHot.take(3), Seq(8, 16, 32).map(sew => 
                           Cat(UIntSplit(vs_64b, sew).map(widenPad(_)).reverse)))
    }.otherwise {
      vs_widen(i) := vs(i)
    }
  }
  for (i <- 0 until 2) { // 0: high 64b   1: low 64b
    vIntFixpAlu64bs(i).io.vs1 := UIntSplit(vs_widen(0), 64)(i)
    vIntFixpAlu64bs(i).io.vs2 := UIntSplit(vs_widen(1), 64)(i)    
  }
  for (i <- 0 until 2) {
    vIntFixpAlu64bs(i).io.vmask := io.in.mask(7, 0) //!!!! Todo: Incorrect!!!!
  }

  io.out.vd := Cat(vIntFixpAlu64bs.map(_.io.vd).reverse) //!!!! Todo: Incorrect!!
  io.out.vxsat := false.B //!!!! Todo: Incorrect!!
}
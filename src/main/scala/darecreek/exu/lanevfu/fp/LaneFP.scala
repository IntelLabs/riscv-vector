/** *************************************************************************************
  * Copyright (c) 2023-2024 Intel Corporation
  * Vector Acceleration IP core for RISC-V* is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  * See the Mulan PSL v2 for more details.
  * ************************************************************************************* */

package darecreek.exu.lanevfu.fp

import chipsalliance.rocketchip.config.Parameters
import chisel3.{util, _}
import chisel3.util._
import darecreek.exu.vfucore.fp._
import darecreek.exu.vfucore.{LaneFUInput, LaneFUOutput}
import darecreek.exu.vfucore.{VFuModule, VFuParamsKey, VFuParameters}
import darecreek.exu.vfucoreconfig.{VUop, Redirect}
import darecreek.{SewOH, UIntSplit, MaskReorg}

class LaneFP(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val redirect = Input(new Redirect)
    val out = DecoupledIO(new LaneFUOutput)
  })

  val uop = io.out.bits.uop
  val narrow_to_1 = uop.ctrl.narrow_to_1
  val vlmul = Mux(uop.info.vlmul > 3.U, 0.U, uop.info.vlmul)
  val fpu = Module(new VFPUTop()(p))
  val maskKeep = fpu.io.maskKeep
  val maskOff = fpu.io.maskOff
  val fpu_vd = fpu.io.out.bits.vd
  val normal_vd = fpu_vd & maskKeep | maskOff
  val cmp_vd = Wire(UInt(64.W))

  // Output of narrow-to-1
  val fpuCmpOut = Reg(Vec(8, UInt(8.W)))
  val fpuCmpValid = uop.expdEnd && narrow_to_1
  val cmpMasked = normal_vd(7, 0)

  fpuCmpOut(0) := Mux(uop.ctrl.narrow_to_1 && (uop.expdIdx === 0.U), cmpMasked, fpuCmpOut(0))
  for (i <- 1 until 7) {
    fpuCmpOut(i) := Mux(uop.ctrl.narrow_to_1 && (uop.expdIdx === 0.U), ~(0.U(8.W)),
      Mux(uop.ctrl.narrow_to_1 && (uop.expdIdx === i.U), cmpMasked, fpuCmpOut(i)))
  }

  val one64 = ~0.U(64.W)
  cmp_vd := one64
  when((vlmul === 0.U) && uop.expdEnd) {
    cmp_vd := Cat(one64(63, 8), cmpMasked)
  }.elsewhen((vlmul === 1.U) && uop.expdEnd) {
    cmp_vd := Cat(one64(63, 16), cmpMasked, fpuCmpOut(0))
  }.elsewhen((vlmul === 2.U) && uop.expdEnd) {
    cmp_vd := Cat(one64(63, 32), cmpMasked, Cat(fpuCmpOut.reverse)(23, 0))
  }.elsewhen((vlmul === 3.U) && uop.expdEnd) {
    cmp_vd := Cat(cmpMasked, Cat(fpuCmpOut.reverse)(55, 0))
  }

  val cmp_fflag = Wire(UInt(5.W))
  val old_cmp_fflag = RegInit(0.U(5.W))

  when(io.out.bits.uop.ctrl.narrow_to_1) {
    when(io.out.valid && io.out.ready) {
      old_cmp_fflag := 0.U
    }.elsewhen(fpu.io.out.valid) {
      when(io.out.bits.uop.expdIdx === 0.U) {
        old_cmp_fflag := fpu.io.out.bits.fflags
      }.otherwise {
        old_cmp_fflag := old_cmp_fflag | fpu.io.out.bits.fflags
      }
    }
  }

  cmp_fflag := Mux(io.out.bits.uop.expdIdx === 0.U, fpu.io.out.bits.fflags, old_cmp_fflag | fpu.io.out.bits.fflags)

  val vstart_gte_vl = io.out.bits.uop.info.vstart >= io.out.bits.uop.info.vl
  val output_valid = Mux(narrow_to_1, fpuCmpValid & fpu.io.out.valid, fpu.io.out.valid)
  val output_vd = Mux(narrow_to_1 & !vstart_gte_vl, cmp_vd, normal_vd)
  val output_fflag = Mux(vstart_gte_vl, 0.U, Mux(narrow_to_1, cmp_fflag, fpu.io.out.bits.fflags))

  fpu.io.in.bits := io.in.bits
  fpu.io.in.valid := io.in.valid
  io.in.ready := fpu.io.in.ready
  fpu.io.redirect := io.redirect

  val eewVd = SewOH(io.in.bits.uop.info.destEew)
  val maskSplash = Mux(io.in.bits.uop.ctrl.narrow_to_1, io.in.bits.mask, MaskReorg.splash(io.in.bits.mask, eewVd))
  val tailSplash = Mux(io.in.bits.uop.ctrl.narrow_to_1, io.in.bits.tail, MaskReorg.splash(io.in.bits.tail, eewVd))

  fpu.io.in.bits.mask := maskSplash
  fpu.io.in.bits.tail := tailSplash

  // io.out.bits.uop := fpu.io.out.bits.uop
  io.out.bits := fpu.io.out.bits
  io.out.valid := output_valid
  io.out.bits.vd := output_vd
  io.out.bits.fflags := output_fflag
  fpu.io.out.ready := io.out.ready

}
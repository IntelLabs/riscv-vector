package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import firrtl.Utils
import darecreek.exu.vfu.div.VDiv
import xiangshan.Redirect
import darecreek.exu.vfu.perm.Permutation


class VPermWrapper (implicit p : Parameters) extends Module {

  class PermReadRF extends Bundle{
    val rd_en = Bool()
    val rd_preg_idx = UInt(8.W)
  }

  val io = IO(new Bundle {
    val in = Input(new VPermInput)
    val redirect = Input(ValidIO(new Redirect))
    val out = Output(ValidIO(new IexOut))
    val perm_busy = Output(Bool())
    val permReadRF = Output(new PermReadRF)
  })

  val vPerm = Module(new Permutation)
  vPerm.io.in := io.in
  vPerm.io.redirect := io.redirect

  val permWriteNum = RegInit(0.U(4.W))
  when(vPerm.io.out.wb_vld){
      io.out.bits.toRegFileWrite.rfWriteEn   := true.B
      io.out.bits.toRegFileWrite.rfWriteMask := Fill(128/8, 0.U)
      io.out.bits.toRegFileWrite.rfWriteIdx  := vPerm.io.out.uop.regDstIdx + permWriteNum
      io.out.bits.toRegFileWrite.rfWriteData := vPerm.io.out.wb_data
      permWriteNum := permWriteNum + 1.U
  }.otherwise{
      io.out.bits.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
  }

  val permDone = permWriteNum + 1.U === vPerm.io.out.uop.permExpdLen
  when(vPerm.io.out.wb_vld && permDone){
      permWriteNum := 0.U
      io.out.bits.commitInfo.valid := true.B
      io.out.bits.commitInfo.bits.scalarRegWriteEn := vPerm.io.out.uop.scalarRegWriteEn
      io.out.bits.commitInfo.bits.floatRegWriteEn  := vPerm.io.out.uop.floatRegWriteEn
      io.out.bits.commitInfo.bits.ldest            := vPerm.io.out.uop.ldest
      io.out.bits.commitInfo.bits.data             := vPerm.io.out.wb_data
  }.otherwise{
      io.out.bits.commitInfo.valid := false.B
  }

  io.out.valid := vPerm.io.out.wb_vld
  io.permReadRF.rd_en := vPerm.io.out.rd_en
  io.permReadRF.rd_preg_idx := vPerm.io.out.rd_preg_idx
  io.perm_busy := vPerm.io.out.perm_busy
}

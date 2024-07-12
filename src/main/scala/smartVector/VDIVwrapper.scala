package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import firrtl.Utils
import darecreek.exu.vfu.div.VDiv
import xiangshan.Redirect


class VDivWrapper (implicit p : Parameters) extends Module {

  val io = IO(new Bundle {
    val in  = Flipped(Decoupled((new VFuInput)))
    val out = Output(ValidIO(new IexOut)) 
  })

  val vDiv = Module(new VDiv)

  vDiv.io.in.valid := io.in.valid
  vDiv.io.redirect.valid := false.B 
  vDiv.io.redirect.bits := DontCare
  vDiv.io.in.bits  := io.in.bits
  
  val fflagsBuffer   = RegInit(0.U(5.W))
  //val fflagsBufferIn = Wire(UInt(5.W))

  when(vDiv.io.out.valid){
      io.out.bits.toRegFileWrite.rfWriteEn         := vDiv.io.out.bits.uop.rfWriteEn
      io.out.bits.toRegFileWrite.rfWriteMask       := Fill(128/8, 0.U)
      io.out.bits.toRegFileWrite.rfWriteIdx        := vDiv.io.out.bits.uop.ldest
      io.out.bits.toRegFileWrite.rfWriteData       := vDiv.io.out.bits.vd
      io.out.bits.commitInfo.valid                 := vDiv.io.out.bits.uop.uopEnd
      io.out.bits.commitInfo.bits.scalarRegWriteEn := vDiv.io.out.bits.uop.scalarRegWriteEn
      io.out.bits.commitInfo.bits.floatRegWriteEn  := vDiv.io.out.bits.uop.floatRegWriteEn
      io.out.bits.commitInfo.bits.ldest            := vDiv.io.out.bits.uop.ldest
      io.out.bits.commitInfo.bits.data             := vDiv.io.out.bits.vd
      io.out.bits.commitInfo.bits.fflags           := fflagsBuffer | vDiv.io.out.bits.fflags
      fflagsBuffer                                 := fflagsBuffer | vDiv.io.out.bits.fflags
      io.out.bits.commitInfo.bits.vxsat            := false.B
  }.otherwise{
      io.out.bits.toRegFileWrite := 0.U.asTypeOf(new regWriteIn)
      io.out.bits.commitInfo     := 0.U.asTypeOf(Valid(new CommitInfo))
  }

  when(vDiv.io.out.valid & vDiv.io.out.bits.uop.uopEnd){
      fflagsBuffer := false.B
  }

  io.out.valid := vDiv.io.out.valid
  io.in.ready  := vDiv.io.in.ready

  //when io.out.ready is true, the data can be sent to next module
  vDiv.io.out.ready := true.B
}

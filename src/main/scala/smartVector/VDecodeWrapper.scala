package smartVector

import chisel3._
import chisel3.util._
import darecreek.VDecode
import darecreek.exu.vfu.VUopInfo

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}

class VDecodeOutput(implicit p: Parameters) extends Bundle{
  val vCtrl = new darecreek.VCtrl
  val scalar_opnd_1 = UInt(64.W)
  val scalar_opnd_2 = UInt(64.W)
  val vInfo = new VInfo
  //val valid = Bool()
}

class SVDecodeUnit(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new RVUissue))
    val out = Decoupled(new VDecodeOutput)
    //val decode_ready = Output(Bool())
  })

  val decode = Module(new VDecode)
  decode.io.in := io.in.bits.inst

  io.out.bits.vCtrl         := RegEnable(decode.io.out, io.in.valid)
  io.out.bits.scalar_opnd_1 := RegEnable(io.in.bits.rs1, io.in.valid)
  io.out.bits.scalar_opnd_2 := RegEnable(io.in.bits.rs2, io.in.valid)
  
  io.out.valid             := RegNext(io.in.valid)
  io.out.bits.vInfo.vstart := RegEnable(io.in.bits.vInfo.vstart, io.in.valid)
  io.out.bits.vInfo.vl     := RegEnable(io.in.bits.vInfo.vl, io.in.valid)
  io.out.bits.vInfo.vlmul  := RegEnable(io.in.bits.vInfo.vlmul, io.in.valid)
  io.out.bits.vInfo.vsew   := RegEnable(io.in.bits.vInfo.vsew, io.in.valid)
  io.out.bits.vInfo.vma    := RegEnable(io.in.bits.vInfo.vma, io.in.valid)  
  io.out.bits.vInfo.vta    := RegEnable(io.in.bits.vInfo.vta, io.in.valid) 
  io.out.bits.vInfo.vxrm   := RegEnable(io.in.bits.vInfo.vxrm, io.in.valid)
  io.out.bits.vInfo.frm    := RegEnable(io.in.bits.vInfo.frm, io.in.valid)

  //The following code is only for the mv instruction. It needs to be adjusted according to different instructions later.


  //Only receive one instruction, and then set ready to false
  io.in.ready := io.out.ready
}


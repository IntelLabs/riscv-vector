package smartVector

import chisel3._
import chisel3.util._
import darecreek.VDecode
import darecreek.exu.vfu.VUopInfo

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.VInfoCalc
import darecreek.VInfoAll

class VDecodeOutput(implicit p: Parameters) extends Bundle{
  val vCtrl         = new darecreek.VCtrl
  val scalar_opnd_1 = UInt(64.W)
  val scalar_opnd_2 = UInt(64.W)
  val vInfo         = new VInfo
  val eewEmulInfo   = new VInfoAll

  //TODO: need to package the bundle from darecreek
  //val extraInfo_for_VIllegal = new 
}

class SVDecodeUnit(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new RVUissue))
    val out = Decoupled(new VDecodeOutput)
    //val decode_ready = Output(Bool())
    val iexNeedStall = Input(Bool())
  })

  val decode = Module(new VDecode)
  decode.io.in := io.in.bits.inst

  io.out.bits.vCtrl         := RegEnable(decode.io.out, io.in.valid)
  io.out.bits.scalar_opnd_1 := RegEnable(io.in.bits.rs1, io.in.valid)
  io.out.bits.scalar_opnd_2 := RegEnable(io.in.bits.rs2, io.in.valid)

  //val validTmp = Reg(Bool())
  //when (RegNext(io.in.valid) & (~io.out.ready || io.iexNeedStall)){
  //  validTmp := true.B
  //}.otherwise{
  //  validTmp := false.B 
  //}
  //io.out.valid := Mux(RegNext(io.in.valid) & io.out.ready, true.B, validTmp & io.out.ready)
  io.out.valid := RegNext(io.in.valid) 

  io.out.bits.vInfo.vstart  := RegEnable(io.in.bits.vInfo.vstart, io.in.valid)
  io.out.bits.vInfo.vl      := RegEnable(io.in.bits.vInfo.vl    , io.in.valid)
  io.out.bits.vInfo.vlmul   := RegEnable(io.in.bits.vInfo.vlmul , io.in.valid)
  io.out.bits.vInfo.vsew    := RegEnable(io.in.bits.vInfo.vsew  , io.in.valid)
  io.out.bits.vInfo.vma     := RegEnable(io.in.bits.vInfo.vma   , io.in.valid)  
  io.out.bits.vInfo.vta     := RegEnable(io.in.bits.vInfo.vta   , io.in.valid) 
  io.out.bits.vInfo.vxrm    := RegEnable(io.in.bits.vInfo.vxrm  , io.in.valid)
  io.out.bits.vInfo.frm     := RegEnable(io.in.bits.vInfo.frm   , io.in.valid)

  
  val infoCalc = Module(new VInfoCalc)
  infoCalc.io.ctrl       := decode.io.out
  infoCalc.io.csr.frm    := io.in.bits.vInfo.frm   
  infoCalc.io.csr.vxrm   := io.in.bits.vInfo.vxrm  
  infoCalc.io.csr.vl     := io.in.bits.vInfo.vl    
  infoCalc.io.csr.vstart := io.in.bits.vInfo.vstart
  infoCalc.io.csr.vsew   := io.in.bits.vInfo.vsew  
  infoCalc.io.csr.vill   := decode.io.out.illegal  
  infoCalc.io.csr.ma     := io.in.bits.vInfo.vma    
  infoCalc.io.csr.ta     := io.in.bits.vInfo.vta    
  infoCalc.io.csr.vlmul  := io.in.bits.vInfo.vlmul 

  io.out.bits.eewEmulInfo := RegEnable(infoCalc.io.infoAll, io.in.valid)

  io.in.ready := io.out.ready && ~io.iexNeedStall
}


package smartVector

import chisel3._
import chisel3.util._
import darecreek.VDecode
import darecreek.exu.vfu.VUopInfo

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.VInfoCalc
import darecreek.VInfoAll
import darecreek.VCtrl
import darecreek.VIllegalInstrn
import darecreek.VRobPtr

class VDecodeOutput(implicit p: Parameters) extends Bundle{
  val vCtrl         = new darecreek.VCtrl
  val scalar_opnd_1 = UInt(64.W)
  val scalar_opnd_2 = UInt(64.W)
  val float_opnd_1  = UInt(64.W)
  val vInfo         = new VInfo
  val eewEmulInfo   = new VInfoAll
  val floatRed      = Bool()

  //TODO: need to package the bundle from darecreek
  //val extraInfo_for_VIllegal = new 
}

class SVDecodeUnit(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new RVUissue))
    val out = Decoupled(new VDecodeOutput)
    val vLSUXcpt = Input (new VLSUXcpt)
    //val decode_ready = Output(Bool())
    //val iexNeedStall = Input(Bool())
  })

  val decode = Module(new VDecode)
  decode.io.in := io.in.bits.inst

  val isFloatRedu =  decode.io.out.redu === true.B && decode.io.out.funct3 === "b001".U
  
  val decodeOut = Wire(new VCtrl)
  when(isFloatRedu){
    decodeOut := decode.io.out
    decodeOut.fp := true.B
    decodeOut.redu := false.B
  }.otherwise{
    decodeOut := decode.io.out
  }

  val bitsIn = Wire(new VDecodeOutput)

  bitsIn.vCtrl         := decodeOut
  bitsIn.scalar_opnd_1 := io.in.bits.rs1
  bitsIn.scalar_opnd_2 := io.in.bits.rs2
  bitsIn.float_opnd_1  := io.in.bits.frs1
  bitsIn.floatRed      := isFloatRedu
  
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

  val vIllegalInstrn = Module(new VIllegalInstrn)
  vIllegalInstrn.io.validIn    := io.in.valid
  vIllegalInstrn.io.ctrl       := decode.io.out
  vIllegalInstrn.io.csr.frm    := io.in.bits.vInfo.frm   
  vIllegalInstrn.io.csr.vxrm   := io.in.bits.vInfo.vxrm  
  vIllegalInstrn.io.csr.vl     := io.in.bits.vInfo.vl    
  vIllegalInstrn.io.csr.vstart := io.in.bits.vInfo.vstart
  vIllegalInstrn.io.csr.vsew   := io.in.bits.vInfo.vsew  
  vIllegalInstrn.io.csr.vill   := decode.io.out.illegal  
  vIllegalInstrn.io.csr.ma     := io.in.bits.vInfo.vma    
  vIllegalInstrn.io.csr.ta     := io.in.bits.vInfo.vta    
  vIllegalInstrn.io.csr.vlmul  := io.in.bits.vInfo.vlmul 

  vIllegalInstrn.io.infoAll := infoCalc.io.infoAll
  vIllegalInstrn.io.extraInfo_for_VIllegal := infoCalc.io.extraInfo_for_VIllegal
  vIllegalInstrn.io.robPtrIn := 0.U.asTypeOf(new VRobPtr)

  bitsIn.vCtrl.illegal := false.B

  val decodeInValid = io.in.valid

  bitsIn.vInfo.vstart  := io.in.bits.vInfo.vstart
  bitsIn.vInfo.vl      := io.in.bits.vInfo.vl    
  bitsIn.vInfo.vlmul   := io.in.bits.vInfo.vlmul 
  bitsIn.vInfo.vsew    := io.in.bits.vInfo.vsew  
  bitsIn.vInfo.vma     := io.in.bits.vInfo.vma    
  bitsIn.vInfo.vta     := io.in.bits.vInfo.vta   
  bitsIn.vInfo.vxrm    := io.in.bits.vInfo.vxrm  
  bitsIn.vInfo.frm     := io.in.bits.vInfo.frm   

  bitsIn.eewEmulInfo := infoCalc.io.infoAll

  val validReg = RegInit(false.B)
  val bitsReg = RegInit(0.U.asTypeOf(new VDecodeOutput))

  when(io.vLSUXcpt.exception_vld || io.vLSUXcpt.update_vl || io.out.bits.vCtrl.illegal){
    validReg := false.B
  }

  when(!validReg || io.out.ready){
      validReg := decodeInValid
  }
  
  when(decodeInValid & (!validReg || io.out.ready)) {
      bitsReg := bitsIn
  }
  
  val fire = decodeInValid & (!validReg || io.out.ready)
 
  when(RegNext(fire)){
    bitsReg.vCtrl.illegal := vIllegalInstrn.io.ill.valid
  }
  
  io.out.valid := validReg
  io.out.bits  := bitsReg

  io.out.bits.vCtrl.illegal := Mux(RegNext(fire), vIllegalInstrn.io.ill.valid, bitsReg.vCtrl.illegal)

  io.in.ready := io.out.ready || !validReg
}

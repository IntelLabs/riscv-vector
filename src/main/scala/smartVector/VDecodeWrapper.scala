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
    val exceptionOut = Input(Bool())
  })

  val bufferReg = RegInit(0.U.asTypeOf(new RVUissue))
  val bufferValidReg = RegInit(false.B)
  val validReg = RegInit(false.B)
  val illegalReg = RegInit(false.B)

  //set buffer
  when(!bufferValidReg && !(!validReg || io.out.ready) && io.in.valid){
    bufferReg := io.in.bits
    bufferValidReg := io.in.valid
  }

  //has exception, clear buffer
  when(io.vLSUXcpt.exception_vld || io.vLSUXcpt.update_vl || io.out.bits.vCtrl.illegal || illegalReg){
    bufferValidReg := false.B
  }

  //fire to pipeline reg, clear buffer
  when(!validReg || io.out.ready){
      bufferValidReg := false.B
  }

  val muxData = Wire(new RVUissue)
  val muxValid = Wire(Bool())

  muxData := Mux(bufferValidReg, bufferReg, io.in.bits)
  muxValid := Mux(bufferValidReg, bufferValidReg, io.in.valid)

  val decode = Module(new VDecode)
  decode.io.in := muxData.inst

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
  bitsIn.scalar_opnd_1 := muxData.rs1
  bitsIn.scalar_opnd_2 := muxData.rs2
  bitsIn.float_opnd_1  := muxData.frs1
  bitsIn.floatRed      := isFloatRedu
  
  val infoCalc = Module(new VInfoCalc)
  infoCalc.io.ctrl       := decode.io.out
  infoCalc.io.csr.frm    := muxData.vInfo.frm   
  infoCalc.io.csr.vxrm   := muxData.vInfo.vxrm  
  infoCalc.io.csr.vl     := muxData.vInfo.vl    
  infoCalc.io.csr.vstart := muxData.vInfo.vstart
  infoCalc.io.csr.vsew   := muxData.vInfo.vsew  
  infoCalc.io.csr.vill   := decode.io.out.illegal  
  infoCalc.io.csr.ma     := muxData.vInfo.vma    
  infoCalc.io.csr.ta     := muxData.vInfo.vta    
  infoCalc.io.csr.vlmul  := muxData.vInfo.vlmul 

  val vIllegalInstrn = Module(new VIllegalInstrn)
  vIllegalInstrn.io.validIn    := muxValid
  vIllegalInstrn.io.ctrl       := decode.io.out
  vIllegalInstrn.io.csr.frm    := muxData.vInfo.frm   
  vIllegalInstrn.io.csr.vxrm   := muxData.vInfo.vxrm  
  vIllegalInstrn.io.csr.vl     := muxData.vInfo.vl    
  vIllegalInstrn.io.csr.vstart := muxData.vInfo.vstart
  vIllegalInstrn.io.csr.vsew   := muxData.vInfo.vsew  
  vIllegalInstrn.io.csr.vill   := decode.io.out.illegal  
  vIllegalInstrn.io.csr.ma     := muxData.vInfo.vma    
  vIllegalInstrn.io.csr.ta     := muxData.vInfo.vta    
  vIllegalInstrn.io.csr.vlmul  := muxData.vInfo.vlmul 

  vIllegalInstrn.io.infoAll := infoCalc.io.infoAll
  vIllegalInstrn.io.extraInfo_for_VIllegal := infoCalc.io.extraInfo_for_VIllegal
  vIllegalInstrn.io.robPtrIn := 0.U.asTypeOf(new VRobPtr)

  bitsIn.vCtrl.illegal := false.B

  val decodeInValid = muxValid

  bitsIn.vInfo.vstart  := muxData.vInfo.vstart
  bitsIn.vInfo.vl      := muxData.vInfo.vl    
  bitsIn.vInfo.vlmul   := muxData.vInfo.vlmul 
  bitsIn.vInfo.vsew    := muxData.vInfo.vsew  
  bitsIn.vInfo.vma     := muxData.vInfo.vma    
  bitsIn.vInfo.vta     := muxData.vInfo.vta   
  bitsIn.vInfo.vxrm    := muxData.vInfo.vxrm  
  bitsIn.vInfo.frm     := muxData.vInfo.frm   

  bitsIn.eewEmulInfo := infoCalc.io.infoAll

  val bitsReg = RegInit(0.U.asTypeOf(new VDecodeOutput))

  when(!validReg || io.out.ready){
      validReg := decodeInValid
  }

  when(io.vLSUXcpt.exception_vld || io.vLSUXcpt.update_vl || io.out.bits.vCtrl.illegal || illegalReg){
    validReg := false.B
  }
 
  val fire = decodeInValid & (!validReg || io.out.ready)
  when(fire) {
      bitsReg := bitsIn
  }
 
  io.out.valid := validReg
  io.out.bits  := bitsReg

  io.out.bits.vCtrl.illegal := vIllegalInstrn.io.ill.valid && io.out.valid

  when(io.out.bits.vCtrl.illegal){
    illegalReg := true.B
  }

  when(io.exceptionOut){
    illegalReg := false.B
  }

  io.in.ready := !bufferValidReg
}

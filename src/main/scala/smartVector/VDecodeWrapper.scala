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

  io.out.bits.vCtrl         := RegEnable(decodeOut, io.in.valid & io.in.ready)
  io.out.bits.scalar_opnd_1 := RegEnable(io.in.bits.rs1, io.in.valid)
  io.out.bits.scalar_opnd_2 := RegEnable(io.in.bits.rs2, io.in.valid)
  io.out.bits.float_opnd_1  := RegEnable(io.in.bits.frs1, io.in.valid)
  io.out.bits.floatRed      := RegEnable(isFloatRedu, io.in.valid)
  
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

  val VrgatherEi16VV = decode.io.out.funct3 === "b000".U && decode.io.out.funct6 === "001110".U
  val vrgather16Illegal = VrgatherEi16VV && io.in.bits.vInfo.vlmul === "011".U && io.in.bits.vInfo.vsew === "00".U

  io.out.bits.vCtrl.illegal := vIllegalInstrn.io.ill.valid || vrgather16Illegal

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



  io.out.bits.eewEmulInfo := RegEnable(infoCalc.io.infoAll, io.in.valid)

  io.in.ready := io.out.ready
}


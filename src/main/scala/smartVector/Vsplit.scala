package smartVector

import chisel3._
import chisel3.util._
import darecreek.VDecode
import darecreek.exu.vfu.VUopInfo

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import xiangshan.MicroOp
import SmartParam._

class MuopMergeAttr extends Bundle {
    val scalarRegWriteEn = Bool()
    val rfWriteEn = Bool()
    val ldest = UInt(5.W)
    val muopEnd = Bool()
    val alu = Bool()
    val mul = Bool()
    val fp = Bool()
    val div = Bool()
    val fixP = Bool()
    val redu = Bool()
    val mask = Bool()
    val perm = Bool()
    
    //000:8bit
    //001:16bit
    //010:32bit
    //011:64bit
    //111:128bit
    val regBackWidth     = UInt(3.W)
    val regWriteMuopIdx  = UInt(4.W)
}

class VUopCtrlW extends Bundle {
  val funct6      = UInt(6.W)
  val funct3      = UInt(3.W)
  val vm          = Bool()
  val vs1_imm     = UInt(5.W)
  val vs2         = UInt(5.W)
  val widen       = Bool()
  val widen2      = Bool()
  val narrow      = Bool()
  val narrow_to_1 = Bool()
  val load        = Bool()
  val store       = Bool()
  val alu         = Bool() // All low-latency operations
  val mul         = Bool()
  val fp          = Bool()
  val div         = Bool()
  val fixP        = Bool()
  val redu        = Bool()
  val mask        = Bool()
  val perm        = Bool()
  def vv = !funct3(2) && !(funct3(1) && funct3(0))
  def vx = funct3(2)
  def vi = !funct3(2) && funct3(1) && funct3(0)
  def isLdst = load || store
} 

class VUop(implicit p: Parameters) extends Bundle {
  val ctrl = new VUopCtrlW
  val info = new VUopInfo
  val uopIdx = UInt(3.W)
  val uopEnd = Bool()
  // Temp: system uop
  val sysUop = new MicroOp
}

class UopRegInfo(implicit p : Parameters) extends Bundle {
    val vs1           = UInt(128.W)
    val vs2           = UInt(128.W)
    val mask          = UInt(128.W)
    val old_vd        = UInt(128.W)
    val vxsat         = Bool()
}

class Muop(implicit p : Parameters) extends Bundle {
    val uop           = new VUop
    val scalar_opnd_1 = UInt(64.W)
    val scalar_opnd_2 = UInt(64.W)
    val uopRegInfo    = new UopRegInfo
}

class ExcpInfo extends Bundle {
    val exception_vld   = Bool()
    val update_vl       = Bool()
    val update_data     = UInt(bVL.W)
    val xcpt_cause      = new HellaCacheExceptions()
    val illegalInst     = Bool()
}

class Vsplit(implicit p : Parameters) extends Module {
    val io = IO(new Bundle{
        val in = new Bundle{
            val decodeIn  = Flipped(Decoupled(new VDecodeOutput))
            val regFileIn = Input(new regOut)
        }
        val out = new Bundle{
            val mUop          = ValidIO(new Muop)
            val toRegFileRead = Output(new regReadIn)
            val mUopMergeAttr = ValidIO(new MuopMergeAttr)
        }
        val scoreBoardSetIO = Flipped(new ScoreboardSetIO)
        val scoreBoardReadIO = Flipped(new ScoreboardReadIO)
        val lsuStallSplit = Input(Bool()) 
        val iexNeedStall  = Input(Bool())
        val vLSUXcpt = Input (new VLSUXcpt)
        val excpInfo = Output(new ExcpInfo)
    })
    
    val vCtrl         = RegInit(VecInit(Seq.fill(1)(0.U.asTypeOf(new darecreek.VCtrl))))
    val vInfo         = Reg(Vec(1, new VInfo))
    val scalar_opnd_1 = Reg(Vec(1, UInt(64.W)))
    val scalar_opnd_2 = Reg(Vec(1, UInt(64.W)))
    val uopRegInfo    = Reg(Vec(1, new UopRegInfo))
    val idx           = RegInit(UInt(5.W), 0.U)

    //vCtrl(0).illegal     := RegInit(false.B)
    //vCtrl(0).lsrcVal     := RegInit(VecInit(Seq.fill(3)(false.B)))
    //vCtrl(0).ldestVal    := RegInit(false.B)
    //vCtrl(0).rdVal       := RegInit(false.B)
    //vCtrl(0).load        := RegInit(false.B)
    //vCtrl(0).store       := RegInit(false.B)
    //vCtrl(0).arith       := RegInit(false.B)
    //vCtrl(0).crossLane   := RegInit(false.B)
    //vCtrl(0).alu         := RegInit(false.B)
    //vCtrl(0).mul         := RegInit(false.B)
    //vCtrl(0).fp          := RegInit(false.B)
    //vCtrl(0).div         := RegInit(false.B)
    //vCtrl(0).fixP        := RegInit(false.B)
    //vCtrl(0).redu        := RegInit(false.B)
    //vCtrl(0).mask        := RegInit(false.B)
    //vCtrl(0).perm        := RegInit(false.B)
    //vCtrl(0).widen       := RegInit(false.B)
    //vCtrl(0).widen2      := RegInit(false.B)
    //vCtrl(0).narrow      := RegInit(false.B)
    //vCtrl(0).narrow_to_1 := RegInit(false.B)

    val empty :: ongoing :: Nil = Enum(2)
    val currentState = RegInit(empty)
    val currentStateNext = WireDefault(empty) 

    val instFirstIn = (currentState === empty && io.in.decodeIn.valid)

    when (instFirstIn){       
        vCtrl(0)            := io.in.decodeIn.bits.vCtrl
        vInfo(0)            := io.in.decodeIn.bits.vInfo
        scalar_opnd_1(0)    := io.in.decodeIn.bits.scalar_opnd_1
        scalar_opnd_2(0)    := io.in.decodeIn.bits.scalar_opnd_2
        uopRegInfo(0).vxsat := false.B
    }

    //To save power, when do not need to update the vs1, keep it unchanged. 
    //ALU will judge whether use the data, do not worry to send the wrong data 
    uopRegInfo(0).vs1       := Mux(io.in.regFileIn.readVld(0), io.in.regFileIn.readData(0), uopRegInfo(0).vs1)
    uopRegInfo(0).vs2       := Mux(io.in.regFileIn.readVld(1), io.in.regFileIn.readData(1), uopRegInfo(0).vs2)
    uopRegInfo(0).mask      := Mux(io.in.regFileIn.readVld(2), io.in.regFileIn.readData(2), uopRegInfo(0).mask)
    uopRegInfo(0).old_vd    := Mux(io.in.regFileIn.readVld(3), io.in.regFileIn.readData(3), uopRegInfo(0).old_vd)

    val ctrl = Mux(instFirstIn,io.in.decodeIn.bits.vCtrl,vCtrl(0))
    val info = Mux(instFirstIn,io.in.decodeIn.bits.vInfo,vInfo(0))
    val scalarOpnd1 = Mux(instFirstIn,io.in.decodeIn.bits.scalar_opnd_1,scalar_opnd_1(0))
    val scalarOpnd2 = Mux(instFirstIn,io.in.decodeIn.bits.scalar_opnd_2,scalar_opnd_2(0))

    //Because the register file do not always read the register file when instFirstIn
    val vs1     = Mux(io.in.regFileIn.readVld(0), io.in.regFileIn.readData(0), uopRegInfo(0).vs1)
    val vs2     = Mux(io.in.regFileIn.readVld(1), io.in.regFileIn.readData(1), uopRegInfo(0).vs2)
    val mask    = Mux(io.in.regFileIn.readVld(2), io.in.regFileIn.readData(2), uopRegInfo(0).mask)
    val old_vd  = Mux(io.in.regFileIn.readVld(3), io.in.regFileIn.readData(3), uopRegInfo(0).old_vd)
    val v_ext_out = ctrl.alu && ctrl.funct3 === "b010".U && ctrl.funct6 === "b010010".U 
    
    val lsrc1_inc = Wire(UInt(3.W))
    when (ctrl.widen || v_ext_out && ctrl.lsrc(0)(2,1) === 3.U) {
      lsrc1_inc := idx >> 1
    }.elsewhen (v_ext_out && ctrl.lsrc(0)(2,1) === 2.U) {
      lsrc1_inc := idx >> 2
    }.elsewhen (v_ext_out && ctrl.lsrc(0)(2,1) === 1.U) {
      lsrc1_inc := idx >> 3
    }.elsewhen (ctrl.funct6 === "b010100".U) { //VMUNARY0
      lsrc1_inc := 0.U
    }.otherwise {
      lsrc1_inc := idx
    }

    val lsrc0_inc =             //vcompress
          Mux(ctrl.redu || (ctrl.funct6 === "b010111".U && ctrl.funct3 === 2.U), 0.U, 
          Mux(ctrl.widen || ctrl.widen2 || ctrl.narrow, idx >> 1, idx))

    val ldest_inc = Wire(UInt(3.W))
    //ToDo: add ldst idex inc
    //when (ldstCtrlReg(i).indexed && ctrl.isLdst) {
    //  ldest_inc := sewSide_inc
    when (ctrl.narrow) {
      ldest_inc := idx >> 1
    }.elsewhen (ctrl.redu || ctrl.narrow_to_1) {
      ldest_inc := 0.U
    }.otherwise {
      ldest_inc := idx
    }

    //ToDo: redu, widen2,narrow_to_1 need to be add
    val regBackWidth = UInt(3.W)
    when(ctrl.widen) {
        io.out.mUopMergeAttr.bits.regBackWidth := "b111".U
        io.out.mUopMergeAttr.bits.regWriteMuopIdx  := 0.U
    }.elsewhen(ctrl.narrow) {
        io.out.mUopMergeAttr.bits.regBackWidth := "b11".U
        io.out.mUopMergeAttr.bits.regWriteMuopIdx  := idx(0)
    }.otherwise{
        io.out.mUopMergeAttr.bits.regBackWidth := "b111".U
        io.out.mUopMergeAttr.bits.regWriteMuopIdx  := 0.U       
    }
    
    io.out.mUopMergeAttr.valid                 := io.out.mUop.valid
    io.out.mUopMergeAttr.bits.scalarRegWriteEn := ctrl.rdVal
    io.out.mUopMergeAttr.bits.ldest            := ctrl.ldest + ldest_inc
    io.out.mUopMergeAttr.bits.rfWriteEn        := ctrl.ldestVal
    io.out.mUopMergeAttr.bits.muopEnd          := io.out.mUop.bits.uop.uopEnd
    io.out.mUopMergeAttr.bits.alu              := ctrl.alu
    io.out.mUopMergeAttr.bits.mul              := ctrl.mul 
    io.out.mUopMergeAttr.bits.fp               := ctrl.fp  
    io.out.mUopMergeAttr.bits.div              := ctrl.div 
    io.out.mUopMergeAttr.bits.fixP             := ctrl.fixP
    io.out.mUopMergeAttr.bits.redu             := ctrl.redu
    io.out.mUopMergeAttr.bits.mask             := ctrl.mask
    io.out.mUopMergeAttr.bits.perm             := ctrl.perm

    val vs1ReadEn = ctrl.lsrcVal(0)
    val vs2ReadEn = ctrl.lsrcVal(1)
    val vs1Idx = ctrl.lsrc(0) + lsrc0_inc
    val vs2Idx = ctrl.lsrc(1) + lsrc1_inc
    val needStall  = Wire(Bool())
    val hasRegConf = Wire(Vec(2,Bool()))
    val expdLen = Wire(UInt(4.W))
    
    io.scoreBoardReadIO.readAddr1 := vs1Idx
    io.scoreBoardReadIO.readAddr2 := vs2Idx
    when(!vs1ReadEn){
        hasRegConf(0) := false.B
    }.elsewhen (~io.scoreBoardReadIO.readBypassed1){
        hasRegConf(0) := false.B
    }.otherwise{
        hasRegConf(0) := true.B
    }

    when(!vs2ReadEn){
        hasRegConf(1) := false.B
    }.elsewhen (~io.scoreBoardReadIO.readBypassed2){
        hasRegConf(1) := false.B
    }.otherwise{
        hasRegConf(1) := true.B
    }

    needStall := hasRegConf(0) || hasRegConf(1) || io.lsuStallSplit || io.iexNeedStall || 
                 io.in.decodeIn.bits.vCtrl.illegal || io.vLSUXcpt.exception_vld
         
    io.out.mUop.bits.uop.uopIdx := idx
    io.out.mUop.bits.uop.uopEnd := (idx + 1.U === expdLen)

    io.out.mUop.bits.uop.ctrl.funct6      := ctrl.funct6
    io.out.mUop.bits.uop.ctrl.funct3      := ctrl.funct3
    io.out.mUop.bits.uop.ctrl.vm          := ctrl.vm
    io.out.mUop.bits.uop.ctrl.vs1_imm     := ctrl.lsrc(0)
    io.out.mUop.bits.uop.ctrl.vs2         := ctrl.lsrc(1)  
    io.out.mUop.bits.uop.ctrl.widen       := ctrl.widen
    io.out.mUop.bits.uop.ctrl.widen2      := ctrl.widen2
    io.out.mUop.bits.uop.ctrl.narrow      := ctrl.narrow
    io.out.mUop.bits.uop.ctrl.narrow_to_1 := ctrl.narrow_to_1
    io.out.mUop.bits.uop.ctrl.load        := ctrl.load
    io.out.mUop.bits.uop.ctrl.store       := ctrl.store
    io.out.mUop.bits.uop.ctrl.alu         := ctrl.alu  
    io.out.mUop.bits.uop.ctrl.mul         := ctrl.mul 
    io.out.mUop.bits.uop.ctrl.fp          := ctrl.fp  
    io.out.mUop.bits.uop.ctrl.div         := ctrl.div 
    io.out.mUop.bits.uop.ctrl.fixP        := ctrl.fixP
    io.out.mUop.bits.uop.ctrl.redu        := ctrl.redu
    io.out.mUop.bits.uop.ctrl.mask        := ctrl.mask
    io.out.mUop.bits.uop.ctrl.perm        := ctrl.perm

    io.out.mUop.bits.uop.info.ma          := info.vma
    io.out.mUop.bits.uop.info.ta          := info.vta
    io.out.mUop.bits.uop.info.vl          := info.vl
    io.out.mUop.bits.uop.info.vstart      := info.vstart
    io.out.mUop.bits.uop.info.vsew        := info.vsew
    io.out.mUop.bits.uop.info.vlmul       := info.vlmul
    io.out.mUop.bits.uop.info.vxrm        := info.vxrm
    io.out.mUop.bits.uop.info.frm         := info.frm
    io.out.mUop.bits.uop.sysUop           := 0.U.asTypeOf(new MicroOp)

    io.out.mUop.bits.scalar_opnd_1        := scalarOpnd1
    io.out.mUop.bits.scalar_opnd_2        := scalarOpnd2

    io.out.mUop.bits.uopRegInfo.vxsat     := false.B          
    io.out.mUop.bits.uopRegInfo.vs1       := vs1
    io.out.mUop.bits.uopRegInfo.vs2       := vs2
    io.out.mUop.bits.uopRegInfo.old_vd    := old_vd
    io.out.mUop.bits.uopRegInfo.mask      := mask

    io.out.toRegFileRead.rfReadEn(0)          := io.out.mUop.valid && ctrl.lsrcVal(0)
    io.out.toRegFileRead.rfReadEn(1)          := io.out.mUop.valid && ctrl.lsrcVal(1)
    io.out.toRegFileRead.rfReadEn(2)          := io.out.mUop.valid && ~ctrl.vm
    io.out.toRegFileRead.rfReadEn(3)          := io.out.mUop.valid && (ctrl.ldestVal || ctrl.store)
    io.out.toRegFileRead.rfReadIdx(0)         := ctrl.lsrc(0) + lsrc0_inc
    io.out.toRegFileRead.rfReadIdx(1)         := ctrl.lsrc(1) + lsrc1_inc
    io.out.toRegFileRead.rfReadIdx(2)         := 0.U
    io.out.toRegFileRead.rfReadIdx(3)         := ctrl.ldest + ldest_inc

    io.scoreBoardSetIO.setEn     := RegNext(io.out.mUop.valid && ctrl.ldestVal)
    io.scoreBoardSetIO.setAddr   := RegNext(io.out.mUopMergeAttr.bits.ldest)

    when ((instFirstIn || currentState === ongoing) & ~needStall){
        io.out.mUop.valid := true.B
        idx := idx + 1.U
    }.otherwise{
        io.out.mUop.valid := false.B
    }

    val expdLenReg = Reg(UInt(4.W))
    val emulVd  = io.in.decodeIn.bits.eewEmulInfo.emulVd
    val emulVs1 = io.in.decodeIn.bits.eewEmulInfo.emulVs1
    val emulVs2 = io.in.decodeIn.bits.eewEmulInfo.emulVs2
    val expdLenTmp = Mux(emulVd >= emulVs1, Mux(emulVd >= emulVs2, emulVd, emulVs2), Mux(emulVs1 >= emulVs2, emulVs1, emulVs2))
    when(instFirstIn){
        expdLenReg := expdLenTmp
    }
    expdLen := Mux(instFirstIn, expdLenTmp, expdLenReg)

    switch(currentState){
        is(empty){
            when(instFirstIn && needStall){
                currentStateNext := ongoing
            }.elsewhen(io.in.decodeIn.valid && expdLen === 1.U){
                currentStateNext := empty
                idx := 0.U              
            }.elsewhen(io.in.decodeIn.valid && expdLen =/= 1.U){
                currentStateNext := ongoing
            }.otherwise{
                currentStateNext := empty
            }
        }
        is(ongoing){
            when(needStall){
                currentStateNext := ongoing
            }.elsewhen((idx + 1.U) === expdLen){
                currentStateNext := empty
                idx := 0.U
            }.elsewhen((idx + 1.U) < expdLen){
                currentStateNext := ongoing
            }
        }
    }

    currentState := currentStateNext

    io.in.decodeIn.ready := (currentStateNext === empty)
    
    //assert(io.in.valid && currentState === ongoing, "when has ongoing inst, can not accept a new one")

    when(io.in.decodeIn.bits.vCtrl.illegal || io.vLSUXcpt.exception_vld){
        currentStateNext := empty
        idx := 0.U
        io.in.decodeIn.ready := true.B
    }

    io.excpInfo.exception_vld := io.vLSUXcpt.exception_vld || io.in.decodeIn.bits.vCtrl.illegal
    io.excpInfo.illegalInst   := io.in.decodeIn.bits.vCtrl.illegal
    io.excpInfo.update_vl     := io.vLSUXcpt.update_vl
    io.excpInfo.update_data   := io.vLSUXcpt.update_data
    io.excpInfo.xcpt_cause    := io.vLSUXcpt.xcpt_cause

}


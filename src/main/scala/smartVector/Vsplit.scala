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
    
    //000:8bit
    //001:16bit
    //010:32bit
    //011:64bit
    //111:128bit
    val regBackWidth     = UInt(3.W)
    val regWriteMuopIdx  = UInt(4.W)
}

class VUopCtrlW extends Bundle {
  val funct6 = UInt(6.W)
  val funct3 = UInt(3.W)
  val vm = Bool()
  val vs1_imm = UInt(5.W)
  val widen = Bool()
  val widen2 = Bool()
  val narrow = Bool()
  val narrow_to_1 = Bool()
  val load = Bool()
  val store = Bool()
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
    val vxsat         = Bool()
}

class Muop(implicit p : Parameters) extends Bundle {
    val uop           = new VUop
    val scalar_opnd_1 = UInt(64.W)
    val scalar_opnd_2 = UInt(64.W)
    val uopRegInfo    = new UopRegInfo
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
            val mUopMergeAttr = Output(new MuopMergeAttr)
        }
        val scoreBoardSetIO = Flipped(new ScoreboardSetIO)
        val scoreBoardReadIO = Flipped(new ScoreboardReadIO)
        val stallSplit = Input(Bool()) 
    })
    
    val vCtrl         = Reg(Vec(1, new darecreek.VCtrl))
    val vInfo         = Reg(Vec(1, new VInfo))
    val scalar_opnd_1 = Reg(Vec(1, UInt(64.W)))
    val scalar_opnd_2 = Reg(Vec(1, UInt(64.W)))
    val uopRegInfo    = Reg(Vec(1, new UopRegInfo))
    val idx           = RegInit(UInt(5.W), 0.U)

    val empty :: ongoing :: Nil = Enum(2)
    val currentState = RegInit(empty)
    val currentStateNext = WireDefault(empty) 

    val instFirstIn = (currentState === empty && io.in.decodeIn.valid)

    when (instFirstIn){       
        vCtrl(0)                 := io.in.decodeIn.bits.vCtrl
        vInfo(0)                 := io.in.decodeIn.bits.vInfo
        scalar_opnd_1(0)         := io.in.decodeIn.bits.scalar_opnd_1
        scalar_opnd_2(0)         := io.in.decodeIn.bits.scalar_opnd_2

        //To save power, when do not need to update the vs1, keep it unchanged. 
        //ALU will judge whether use the data, do not worry to send the wrong data 
        uopRegInfo(0).vs1        := Mux(io.in.regFileIn.readVld(0), io.in.regFileIn.readData(0), uopRegInfo(0).vs1)
        uopRegInfo(0).vs2        := Mux(io.in.regFileIn.readVld(1), io.in.regFileIn.readData(1), uopRegInfo(0).vs2)

        uopRegInfo(0).vxsat      := false.B
    } 

    val ctrl = Mux(instFirstIn,io.in.decodeIn.bits.vCtrl,vCtrl(0))
    val info = Mux(instFirstIn,io.in.decodeIn.bits.vInfo,vInfo(0))
    val vs1  = Mux(instFirstIn,io.in.regFileIn.readData(0), uopRegInfo(0).vs1)
    val vs2  = Mux(instFirstIn,io.in.regFileIn.readData(1), uopRegInfo(0).vs2)
    val scalar_opnd_1_ = Mux(instFirstIn,io.in.decodeIn.bits.scalar_opnd_1,scalar_opnd_1(0))
    val scalar_opnd_2_ = Mux(instFirstIn,io.in.decodeIn.bits.scalar_opnd_2,scalar_opnd_2(0))
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
        io.out.mUopMergeAttr.regBackWidth := "b111".U
        io.out.mUopMergeAttr.regWriteMuopIdx  := 0.U
    }.elsewhen(ctrl.narrow) {
        io.out.mUopMergeAttr.regBackWidth := "b11".U
        io.out.mUopMergeAttr.regWriteMuopIdx  := idx(0)
    }.otherwise{
        io.out.mUopMergeAttr.regBackWidth := "b111".U
        io.out.mUopMergeAttr.regWriteMuopIdx  := 0.U       
    }
    io.out.mUopMergeAttr.scalarRegWriteEn := ctrl.rdVal
    io.out.mUopMergeAttr.ldest            := ctrl.ldest + ldest_inc
    io.out.mUopMergeAttr.rfWriteEn        := ctrl.ldestVal
    io.out.mUopMergeAttr.muopEnd          := io.out.mUop.bits.uop.uopEnd
    io.out.mUopMergeAttr.alu              := ctrl.alu

    val vs1ReadEn = ctrl.lsrcVal(0)
    val vs2ReadEn = ctrl.lsrcVal(1)
    val vs1Idx = ctrl.lsrc(0) + lsrc0_inc
    val vs2Idx = ctrl.lsrc(1) + lsrc1_inc
    val needStall  = Wire(Bool())
    val hasRegConf = Wire(Vec(2,Bool()))
    
    io.scoreBoardReadIO.readAddr1 := vs1Idx
    io.scoreBoardReadIO.readAddr2 := vs2Idx
    when(!vs1ReadEn){
        hasRegConf(0) := false.B
    }.elsewhen (~io.scoreBoardReadIO.read1 || ~io.scoreBoardReadIO.readBypassed1){
        hasRegConf(0) := false.B
    }.otherwise{
        hasRegConf(0) := true.B
    }

    when(!vs2ReadEn){
        hasRegConf(1) := false.B
    }.elsewhen (~io.scoreBoardReadIO.read2 || ~io.scoreBoardReadIO.readBypassed2){
        hasRegConf(1) := false.B
    }.otherwise{
        hasRegConf(1) := true.B
    }

    needStall := hasRegConf(0) || hasRegConf(1) || io.stallSplit
         
    io.out.mUop.bits.uop.uopIdx := idx
    io.out.mUop.bits.uop.uopEnd := (idx + 1.U === vInfo(0).vlmul)

    io.out.mUop.bits.uop.ctrl.funct6      := ctrl.funct6
    io.out.mUop.bits.uop.ctrl.funct3      := ctrl.funct3
    io.out.mUop.bits.uop.ctrl.vm          := ctrl.vm
    io.out.mUop.bits.uop.ctrl.vs1_imm     := ctrl.lsrc(0)
    io.out.mUop.bits.uop.ctrl.widen       := ctrl.widen
    io.out.mUop.bits.uop.ctrl.widen2      := ctrl.widen2
    io.out.mUop.bits.uop.ctrl.narrow      := ctrl.narrow
    io.out.mUop.bits.uop.ctrl.narrow_to_1 := ctrl.narrow_to_1
    io.out.mUop.bits.uop.ctrl.load        := ctrl.load
    io.out.mUop.bits.uop.ctrl.store       := ctrl.store
    
    io.out.mUop.bits.uop.info.ma          := info.vma
    io.out.mUop.bits.uop.info.ta          := info.vta
    io.out.mUop.bits.uop.info.vl          := info.vl
    io.out.mUop.bits.uop.info.vstart      := info.vstart
    io.out.mUop.bits.uop.info.vsew        := info.vsew
    io.out.mUop.bits.uop.info.vlmul       := info.vlmul
    io.out.mUop.bits.uop.info.vxrm        := info.vxrm
    io.out.mUop.bits.uop.info.frm         := info.frm
    io.out.mUop.bits.uop.sysUop           := 0.U.asTypeOf(new MicroOp)

    io.out.mUop.bits.scalar_opnd_1        := scalar_opnd_1_
    io.out.mUop.bits.scalar_opnd_2        := scalar_opnd_2_

    io.out.mUop.bits.uopRegInfo.vxsat     := false.B          
    io.out.mUop.bits.uopRegInfo.vs1       := vs1
    io.out.mUop.bits.uopRegInfo.vs2       := vs2

    io.out.toRegFileRead.rfReadEn(0)          := ctrl.lsrcVal(0)
    io.out.toRegFileRead.rfReadEn(1)          := ctrl.lsrcVal(1)
    io.out.toRegFileRead.rfReadIdx(0)         := ctrl.lsrc(0) + lsrc0_inc
    io.out.toRegFileRead.rfReadIdx(1)         := ctrl.lsrc(1) + lsrc1_inc

    io.scoreBoardSetIO.setEn     := true.B
    io.scoreBoardSetIO.setAddr   := io.out.mUopMergeAttr.ldest

    when ((instFirstIn || currentState === ongoing) & ~needStall){
        io.out.mUop.valid := true.B
        idx := idx + 1.U
    }.otherwise{
        io.out.mUop.valid := false.B
    }

    switch(currentState){
        is(empty){
            when(needStall){
                currentStateNext := ongoing
            }.elsewhen(io.in.decodeIn.valid && io.in.decodeIn.bits.vInfo.vlmul === 1.U){
                currentStateNext := empty
                idx := 0.U              
            }.elsewhen(io.in.decodeIn.valid && io.in.decodeIn.bits.vInfo.vlmul =/= 1.U){
                currentStateNext := ongoing
            }.otherwise{
                currentStateNext := empty
            }
        }
        is(ongoing){
            when(needStall){
                currentStateNext := ongoing
            }.elsewhen(idx + 1.U === vInfo(0).vlmul){
                currentStateNext := empty
                idx := 0.U
            }.elsewhen(idx + 1.U < vInfo(0).vlmul){
                currentStateNext := ongoing
            }
        }
    }

    currentState := currentStateNext

    io.in.decodeIn.ready := (currentState === empty)
    
    //assert(io.in.valid && currentState === ongoing, "when has ongoing inst, can not accept a new one")

}


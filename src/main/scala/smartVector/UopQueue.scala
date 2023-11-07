package smartVector

import chisel3._
import chisel3.util._
import darecreek.VDecode
import darecreek.exu.vfu.VUopInfo

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.VUop
import xiangshan.MicroOp
import SmartParam._

class UopAttribute extends Bundle {
    val ldest = UInt(5.W)
    val scalarRegWriteEn = Bool()
}

class UopRegInfo extends Bundle {
    val vs1       = UInt(128.W)
    val vs2       = UInt(128.W)
    val rfWriteEn = Bool()
    val vxsat     = Bool()
}

class UopQueueOutput(implicit p : Parameters) extends Bundle {
    val uop = new VUop
    val scalar_opnd_1 = UInt(64.W)
    val scalar_opnd_2 = UInt(64.W)
    val uopAttribute = new UopAttribute
    val uopRegInfo   = new UopRegInfo
}

class UopQueue(implicit p : Parameters) extends Module {
    val io = IO(new Bundle{
        val in = new Bundle{
            val decodeIn  = Flipped(Decoupled(new VDecodeOutput))
            val regFileIn = Input(new regOut)
            val regWriteEn = Input(Bool())
            val regWriteIdx = Input(UInt(5.W))
        }
        val out = new Bundle{
            val mUop        = ValidIO(new UopQueueOutput)
            val toRegFile   = Output(new regReadIn)
            val mUopRegAttr = Output(new MUopRegAttr)
        } 
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
        //uopRegInfo(0).vs1        := Mux(io.in.regFileIn.readVld(0), io.in.regFileIn.readData(0), uopRegInfo(0).vs1)
        //uopRegInfo(0).vs2        := Mux(io.in.regFileIn.readVld(1), io.in.regFileIn.readData(1), uopRegInfo(0).vs2)

        uopRegInfo(0).rfWriteEn  := io.in.decodeIn.bits.vCtrl.ldestVal
        uopRegInfo(0).vxsat      := false.B
    } 

    val ctrl = Mux(instFirstIn,io.in.decodeIn.bits.vCtrl,vCtrl(0))
    val info = Mux(instFirstIn,io.in.decodeIn.bits.vInfo,vInfo(0))
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

    //object Vlmul_to_lmul {
    //// vlmul --> LMUL --> max(LMUL, 1)
    //// Note: set result to 1 if LMUL < 1
    //    def apply(vlmul: UInt): UInt = {
    //        val y0 = !vlmul(1) && !vlmul(0) || vlmul(2)
    //        val y1 = !vlmul(2) && !vlmul(1) && vlmul(0)
    //        val y2 = !vlmul(2) && vlmul(1) && !vlmul(0)
    //        val y3 = !vlmul(2) && vlmul(1) && vlmul(0)
    //        Cat(y3, y2, y1, y0)
    //    }
    //}
//
    //val expdLen = Wire(UInt(4.W))
    ////val lmul = Vlmul_to_lmul(info.vlmul)
    //val lmul = info.vlmul
    //when (ctrl.widen || ctrl.widen2 || ctrl.narrow) {
    //  expdLen := Mux(info.vlmul(2), 1.U, lmul << 1)  // If lmul < 1, expdLen = 1 for widen/narrow
    //}.elsewhen (ctrl.funct6 === "b100111".U && ctrl.funct3 === "b011".U) {//Whole register move
    //  expdLen := ctrl.lsrc(0)(2, 0) +& 1.U
    //}.otherwise {
    //  expdLen := lmul
    //}
    //io.out.mUopRegAttr.expdLen := expdLen
    ////

    //ToDo: redu, widen2,narrow_to_1 need to be add
    val regBackWidth = UInt(3.W)
    when(ctrl.widen) {
        io.out.mUopRegAttr.regBackWidth := "b111".U
        io.out.mUopRegAttr.regWriteMuopIdx  := 0.U
    }.elsewhen(ctrl.narrow) {
        io.out.mUopRegAttr.regBackWidth := "b11".U
        io.out.mUopRegAttr.regWriteMuopIdx  := idx(0)
    }.otherwise{
        io.out.mUopRegAttr.regBackWidth := "b111".U
        io.out.mUopRegAttr.regWriteMuopIdx  := 0.U       
    }

    val sboard = new Scoreboard(NVPhyRegs, false)
    val vs1ReadEn = Mux(instFirstIn,io.in.decodeIn.bits.vCtrl.lsrcVal(0),vCtrl(0).lsrcVal(0))
    val vs2ReadEn = Mux(instFirstIn,io.in.decodeIn.bits.vCtrl.lsrcVal(1),vCtrl(0).lsrcVal(1))
    val vs1Idx = Mux(instFirstIn,io.in.decodeIn.bits.vCtrl.lsrc(0),vCtrl(0).lsrc(0)) + lsrc0_inc
    val vs2Idx = Mux(instFirstIn,io.in.decodeIn.bits.vCtrl.lsrc(1),vCtrl(0).lsrc(1)) + lsrc1_inc
    val needStall  = Wire(Bool())
    val hasRegConf = Wire(Vec(2,Bool()))
    val canForWard = Wire(Vec(2,Bool()))

    sboard.clear(io.in.regWriteEn, io.in.regWriteIdx)
   
    when(vs1ReadEn && sboard.read(vs1Idx) && ~sboard.readBypassed(vs1Idx)){
        canForWard(0) := true.B
    }.otherwise{
        canForWard(0) := false.B
    }
    printf("read=%b\n",sboard.read(vs1Idx))
    printf("bypass=%b\n",sboard.readBypassed(vs1Idx))

    when(vs2ReadEn && sboard.read(vs2Idx) && ~sboard.readBypassed(vs2Idx)){
        canForWard(1) := true.B
    }.otherwise{
        canForWard(1) := false.B
    }

    when(!vs1ReadEn){
        hasRegConf(0) := false.B
    }.elsewhen (~sboard.read(vs1Idx) || canForWard(0)){
        hasRegConf(0) := false.B
    }.otherwise{
        hasRegConf(0) := true.B
    }

    when(!vs2ReadEn){
        hasRegConf(1) := false.B
    }.elsewhen (~sboard.read(vs2Idx) || canForWard(1)){
        hasRegConf(1) := false.B
    }.otherwise{
        hasRegConf(1) := true.B
    }

    needStall := hasRegConf(0) || hasRegConf(1)

    when (currentState === empty && io.in.decodeIn.valid && ~needStall){

        io.out.toRegFile.rfReadEn(0)  := io.in.decodeIn.bits.vCtrl.lsrcVal(0)
        io.out.toRegFile.rfReadEn(1)  := io.in.decodeIn.bits.vCtrl.lsrcVal(1)
        io.out.toRegFile.rfReadIdx(0) := io.in.decodeIn.bits.vCtrl.lsrc(0) + lsrc0_inc
        io.out.toRegFile.rfReadIdx(1) := io.in.decodeIn.bits.vCtrl.lsrc(1) + lsrc1_inc
        //io.out.toRegFile.bits.rfWriteEn    := false.B
        //io.out.toRegFile.bits.rfWriteIdx   := DontCare
        //io.out.toRegFile.bits.rfWriteData  := DontCare
        //io.out.toRegFile.vxsat        := false.B
        //io.out.toRegFile.valid             := io.out.toRegFile.bits.rfReadEn.reduce(_ || _)

        io.out.mUop.valid := true.B       
        io.out.mUop.bits.uop.uopIdx := idx
        io.out.mUop.bits.uop.uopEnd := (idx + 1.U === io.in.decodeIn.bits.vInfo.vlmul)

        io.out.mUop.bits.uop.ctrl.funct6      := io.in.decodeIn.bits.vCtrl.funct6
        io.out.mUop.bits.uop.ctrl.funct3      := io.in.decodeIn.bits.vCtrl.funct3
        io.out.mUop.bits.uop.ctrl.vm          := io.in.decodeIn.bits.vCtrl.vm
        io.out.mUop.bits.uop.ctrl.vs1_imm     := io.in.decodeIn.bits.vCtrl.lsrc(0)
        io.out.mUop.bits.uop.ctrl.widen       := io.in.decodeIn.bits.vCtrl.widen
        io.out.mUop.bits.uop.ctrl.widen2      := io.in.decodeIn.bits.vCtrl.widen2
        io.out.mUop.bits.uop.ctrl.narrow      := io.in.decodeIn.bits.vCtrl.narrow
        io.out.mUop.bits.uop.ctrl.narrow_to_1 := io.in.decodeIn.bits.vCtrl.narrow_to_1
      
        io.out.mUop.bits.uop.info.ma          := io.in.decodeIn.bits.vInfo.vma
        io.out.mUop.bits.uop.info.ta          := io.in.decodeIn.bits.vInfo.vta
        io.out.mUop.bits.uop.info.vl          := io.in.decodeIn.bits.vInfo.vl
        io.out.mUop.bits.uop.info.vstart      := io.in.decodeIn.bits.vInfo.vstart
        io.out.mUop.bits.uop.info.vsew        := io.in.decodeIn.bits.vInfo.vsew
        io.out.mUop.bits.uop.info.vlmul       := io.in.decodeIn.bits.vInfo.vlmul
        io.out.mUop.bits.uop.info.vxrm        := io.in.decodeIn.bits.vInfo.vxrm
        io.out.mUop.bits.uop.info.frm         := io.in.decodeIn.bits.vInfo.frm
        io.out.mUop.bits.uop.sysUop           := 0.U.asTypeOf(new MicroOp)

        io.out.mUop.bits.scalar_opnd_1        := io.in.decodeIn.bits.scalar_opnd_1
        io.out.mUop.bits.scalar_opnd_2        := io.in.decodeIn.bits.scalar_opnd_2

        io.out.mUop.bits.uopAttribute.scalarRegWriteEn := io.in.decodeIn.bits.vCtrl.rdVal
        //TODO: different inst type has different methods.
        //TODO: when is widen, the ldest = ldest + idx
        //TODO: when is narrow, two adjacent has same idx
        io.out.mUop.bits.uopAttribute.ldest            := io.in.decodeIn.bits.vCtrl.ldest + ldest_inc

        io.out.mUop.bits.uopRegInfo.rfWriteEn := io.in.decodeIn.bits.vCtrl.ldestVal
        io.out.mUop.bits.uopRegInfo.vxsat     := false.B
        io.out.mUop.bits.uopRegInfo.vs1       := io.in.regFileIn.readData(0)
        io.out.mUop.bits.uopRegInfo.vs2       := io.in.regFileIn.readData(1)
        idx := idx + 1.U
    }.elsewhen(currentState === ongoing && ~needStall){
        io.out.mUop.valid := true.B       
        io.out.mUop.bits.uop.uopIdx := idx
        io.out.mUop.bits.uop.uopEnd := (idx + 1.U === vInfo(0).vlmul)

        io.out.mUop.bits.uop.ctrl.funct6      := vCtrl(0).funct6
        io.out.mUop.bits.uop.ctrl.funct3      := vCtrl(0).funct3
        io.out.mUop.bits.uop.ctrl.vm          := vCtrl(0).vm
        io.out.mUop.bits.uop.ctrl.vs1_imm     := vCtrl(0).lsrc(0)
        io.out.mUop.bits.uop.ctrl.widen       := vCtrl(0).widen
        io.out.mUop.bits.uop.ctrl.widen2      := vCtrl(0).widen2
        io.out.mUop.bits.uop.ctrl.narrow      := vCtrl(0).narrow
        io.out.mUop.bits.uop.ctrl.narrow_to_1 := vCtrl(0).narrow_to_1
       
        io.out.mUop.bits.uop.info.ma          := vInfo(0).vma
        io.out.mUop.bits.uop.info.ta          := vInfo(0).vta
        io.out.mUop.bits.uop.info.vl          := vInfo(0).vl
        io.out.mUop.bits.uop.info.vstart      := vInfo(0).vstart
        io.out.mUop.bits.uop.info.vsew        := vInfo(0).vsew
        io.out.mUop.bits.uop.info.vlmul       := vInfo(0).vlmul
        io.out.mUop.bits.uop.info.vxrm        := vInfo(0).vxrm
        io.out.mUop.bits.uop.info.frm         := vInfo(0).frm
        io.out.mUop.bits.uop.sysUop           := 0.U.asTypeOf(new MicroOp)

        io.out.mUop.bits.scalar_opnd_1        := scalar_opnd_1(0)
        io.out.mUop.bits.scalar_opnd_2        := scalar_opnd_2(0)

        io.out.mUop.bits.uopAttribute.scalarRegWriteEn := vCtrl(0).rdVal
        //TODO: different inst type has different methods.
        //TODO: when is widen, the ldest = ldest + idx
        //TODO: when is narrow, two adjacent has same idx
        io.out.mUop.bits.uopAttribute.ldest   := vCtrl(0).ldest + ldest_inc

        io.out.mUop.bits.uopRegInfo.rfWriteEn := uopRegInfo(0).rfWriteEn
        io.out.mUop.bits.uopRegInfo.vxsat     := false.B          
        io.out.mUop.bits.uopRegInfo.vs1       := io.in.regFileIn.readData(0)
        io.out.mUop.bits.uopRegInfo.vs2       := io.in.regFileIn.readData(1)

        io.out.toRegFile.rfReadEn(0)          := vCtrl(0).lsrcVal(0)
        io.out.toRegFile.rfReadEn(1)          := vCtrl(0).lsrcVal(1)
        io.out.toRegFile.rfReadIdx(0)         := vCtrl(0).lsrc(0) + lsrc0_inc
        io.out.toRegFile.rfReadIdx(1)         := vCtrl(0).lsrc(1) + lsrc1_inc

        idx := idx + 1.U
    }.otherwise{
        io.out.mUop.valid := false.B
        io.out.mUop.bits := 0.U.asTypeOf(new UopQueueOutput)
        io.out.mUopRegAttr := 0.U.asTypeOf(new MUopRegAttr)
        io.out.toRegFile := 0.U.asTypeOf(new regReadIn)
    }

    when(needStall){
        currentStateNext := ongoing
    }.otherwise{
        when (currentState === empty && io.in.decodeIn.valid && io.in.decodeIn.bits.vInfo.vlmul === 1.U){
            currentStateNext := empty
            idx := 0.U
        }
        when (currentState === empty && io.in.decodeIn.valid && io.in.decodeIn.bits.vInfo.vlmul =/= 1.U){
            currentStateNext := ongoing
        }
        when (currentState === ongoing && idx + 1.U === vInfo(0).vlmul){
            currentStateNext := empty
            idx := 0.U
        }.elsewhen(currentState === ongoing && idx + 1.U < vInfo(0).vlmul){
            currentStateNext := ongoing
        }
    }
    currentState := currentStateNext

    io.in.decodeIn.ready := (currentStateNext === empty)
    sboard.set(io.out.mUop.valid, io.out.mUop.bits.uopAttribute.ldest)

    //assert(io.in.valid && currentState === ongoing, "when has ongoing inst, can not accept a new one")

}


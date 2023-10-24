package smartVector

import chisel3._
import chisel3.util._
import darecreek.VDecode
import darecreek.exu.vfu.VUopInfo

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.VUop
import xiangshan.MicroOp

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
        }
        val out = new Bundle{
            val mUop = ValidIO(new UopQueueOutput)
            val toRegFile = ValidIO(new regIn)
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

    when (currentState === empty && io.in.decodeIn.valid){       
        vCtrl(0)                 := io.in.decodeIn.bits.vCtrl
        vInfo(0)                 := io.in.decodeIn.bits.vInfo
        scalar_opnd_1(0)         := io.in.decodeIn.bits.scalar_opnd_1
        scalar_opnd_2(0)         := io.in.decodeIn.bits.scalar_opnd_2

        //To save power, when do not need to update the vs1, keep it unchanged. 
        //ALU will judge whether use the data, do not worry to send the wrong data 
        uopRegInfo(0).vs1        := Mux(io.in.regFileIn.readVld(0), io.in.regFileIn.readData(0), uopRegInfo(0).vs1)
        uopRegInfo(0).vs2        := Mux(io.in.regFileIn.readVld(1), io.in.regFileIn.readData(1), uopRegInfo(0).vs2)

        uopRegInfo(0).rfWriteEn  := io.in.decodeIn.bits.vCtrl.ldestVal
        //uopRegInfo(0).vxsat      := io.in.decodeIn.bits.vCtrl.
    } 

    when (currentState === empty && io.in.decodeIn.valid){

        io.out.toRegFile.bits.rfReadEn(0)  := io.in.decodeIn.bits.vCtrl.lsrcVal(0)
        io.out.toRegFile.bits.rfReadEn(1)  := io.in.decodeIn.bits.vCtrl.lsrcVal(1)
        io.out.toRegFile.bits.rfReadIdx(0) := io.in.decodeIn.bits.vCtrl.lsrc(0) + idx
        io.out.toRegFile.bits.rfReadIdx(1) := io.in.decodeIn.bits.vCtrl.lsrc(1) + idx
        io.out.toRegFile.bits.rfWriteEn    := false.B
        io.out.toRegFile.bits.rfWriteIdx   := DontCare
        io.out.toRegFile.bits.rfWriteData  := DontCare
        io.out.toRegFile.bits.vxsat        := false.B
        io.out.toRegFile.valid             := io.out.toRegFile.bits.rfReadEn.reduce(_ || _)

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
        io.out.mUop.bits.uopAttribute.ldest            := io.in.decodeIn.bits.vCtrl.ldest + idx

        io.out.mUop.bits.uopRegInfo.rfWriteEn := io.in.decodeIn.bits.vCtrl.ldestVal
        //io.out.bits.uopRegInfo.vxsat     := io.in.decodeIn.bits.toReg.bits.vxsat
        io.out.mUop.bits.uopRegInfo.vs1       := 0.U
        io.out.mUop.bits.uopRegInfo.vs2       := 0.U
        idx := idx + 1.U
    }.elsewhen(currentState === ongoing){
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
        io.out.mUop.bits.uopAttribute.ldest            := vCtrl(0).ldest + idx

        io.out.mUop.bits.uopRegInfo           := uopRegInfo(0)
        idx := idx + 1.U
    }.otherwise{
        io.out.mUop.valid := false.B
        io.out.mUop.bits := 0.U.asTypeOf(new UopQueueOutput)
    }

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
    currentState := currentStateNext

    io.in.decodeIn.ready := (currentStateNext === empty)

    //assert(io.in.valid && currentState === ongoing, "when has ongoing inst, can not accept a new one")

}


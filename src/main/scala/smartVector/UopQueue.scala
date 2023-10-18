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
        val in = Flipped(Decoupled(new VDecodeOutput))
        val out = ValidIO(new UopQueueOutput)
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

    when (currentState === empty && io.in.valid){       
        vCtrl(0)                 := io.in.bits.vCtrl
        vInfo(0)                 := io.in.bits.vInfo
        scalar_opnd_1(0)         := io.in.bits.scalar_opnd_1
        scalar_opnd_2(0)         := io.in.bits.scalar_opnd_2

        //vmv do not need the reg file read data, so set it to 0 
        uopRegInfo(0).vs1        := 0.U
        uopRegInfo(0).vs2        := 0.U

        uopRegInfo(0).rfWriteEn  := io.in.bits.toReg.bits.rfWriteEn
        uopRegInfo(0).vxsat      := io.in.bits.toReg.bits.vxsat
    } 

    when (currentState === empty && io.in.valid){
        io.out.valid := true.B       
        io.out.bits.uop.uopIdx := idx
        io.out.bits.uop.uopEnd := (idx + 1.U === io.in.bits.vInfo.vlmul)

        io.out.bits.uop.ctrl.funct6      := io.in.bits.vCtrl.funct6
        io.out.bits.uop.ctrl.funct3      := io.in.bits.vCtrl.funct3
        io.out.bits.uop.ctrl.vm          := io.in.bits.vCtrl.vm
        io.out.bits.uop.ctrl.vs1_imm     := io.in.bits.vCtrl.lsrc(0)
        io.out.bits.uop.ctrl.widen       := io.in.bits.vCtrl.widen
        io.out.bits.uop.ctrl.widen2      := io.in.bits.vCtrl.widen2
        io.out.bits.uop.ctrl.narrow      := io.in.bits.vCtrl.narrow
        io.out.bits.uop.ctrl.narrow_to_1 := io.in.bits.vCtrl.narrow_to_1
        
        io.out.bits.uop.info.ma          := io.in.bits.vInfo.vma
        io.out.bits.uop.info.ta          := io.in.bits.vInfo.vta
        io.out.bits.uop.info.vl          := io.in.bits.vInfo.vl
        io.out.bits.uop.info.vstart      := io.in.bits.vInfo.vstart
        io.out.bits.uop.info.vsew        := io.in.bits.vInfo.vsew
        io.out.bits.uop.info.vlmul       := io.in.bits.vInfo.vlmul
        io.out.bits.uop.info.vxrm        := io.in.bits.vInfo.vxrm
        io.out.bits.uop.info.frm         := io.in.bits.vInfo.frm
        io.out.bits.uop.sysUop           := 0.U.asTypeOf(new MicroOp)

        io.out.bits.scalar_opnd_1        := io.in.bits.scalar_opnd_1
        io.out.bits.scalar_opnd_2        := io.in.bits.scalar_opnd_2

        io.out.bits.uopAttribute.ldest   := io.in.bits.vCtrl.ldest

        io.out.bits.uopRegInfo.rfWriteEn := io.in.bits.toReg.bits.rfWriteEn
        io.out.bits.uopRegInfo.vxsat     := io.in.bits.toReg.bits.vxsat
        io.out.bits.uopRegInfo.vs1       := 0.U
        io.out.bits.uopRegInfo.vs2       := 0.U
        idx := idx + 1.U
    }.elsewhen(currentState === ongoing){
        io.out.valid := true.B       
        io.out.bits.uop.uopIdx := idx
        io.out.bits.uop.uopEnd := (idx + 1.U === vInfo(0).vlmul)

        io.out.bits.uop.ctrl.funct6      := vCtrl(0).funct6
        io.out.bits.uop.ctrl.funct3      := vCtrl(0).funct3
        io.out.bits.uop.ctrl.vm          := vCtrl(0).vm
        io.out.bits.uop.ctrl.vs1_imm     := vCtrl(0).lsrc(0)
        io.out.bits.uop.ctrl.widen       := vCtrl(0).widen
        io.out.bits.uop.ctrl.widen2      := vCtrl(0).widen2
        io.out.bits.uop.ctrl.narrow      := vCtrl(0).narrow
        io.out.bits.uop.ctrl.narrow_to_1 := vCtrl(0).narrow_to_1
        
        io.out.bits.uop.info.ma          := vInfo(0).vma
        io.out.bits.uop.info.ta          := vInfo(0).vta
        io.out.bits.uop.info.vl          := vInfo(0).vl
        io.out.bits.uop.info.vstart      := vInfo(0).vstart
        io.out.bits.uop.info.vsew        := vInfo(0).vsew
        io.out.bits.uop.info.vlmul       := vInfo(0).vlmul
        io.out.bits.uop.info.vxrm        := vInfo(0).vxrm
        io.out.bits.uop.info.frm         := vInfo(0).frm
        io.out.bits.uop.sysUop           := 0.U.asTypeOf(new MicroOp)

        io.out.bits.scalar_opnd_1        := scalar_opnd_1(0)
        io.out.bits.scalar_opnd_2        := scalar_opnd_2(0)

        io.out.bits.uopAttribute.ldest   := vCtrl(0).ldest

        io.out.bits.uopRegInfo           := uopRegInfo(0)
        idx := idx + 1.U
    }.otherwise{
        io.out.valid := false.B
        io.out.bits := 0.U.asTypeOf(new UopQueueOutput)
    }

    when (currentState === empty && io.in.valid && io.in.bits.vInfo.vlmul === 1.U){
        currentStateNext := empty
        idx := 0.U
    }
    when (currentState === empty && io.in.valid && io.in.bits.vInfo.vlmul =/= 1.U){
        currentStateNext := ongoing
    }
    when (currentState === ongoing && idx + 1.U === vInfo(0).vlmul){
        currentStateNext := empty
        idx := 0.U
    }.elsewhen(currentState === ongoing && idx + 1.U < vInfo(0).vlmul){
        currentStateNext := ongoing
    }
    currentState := currentStateNext

    io.in.ready := (currentStateNext === empty)

    //assert(io.in.valid && currentState === ongoing, "when has ongoing inst, can not accept a new one")

}


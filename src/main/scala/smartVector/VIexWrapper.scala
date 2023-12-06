package smartVector

//import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
//import chiseltest.WriteVcdAnnotation
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.mac._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.VInstructions._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chipsalliance.rocketchip.config
import xiangshan.backend.rob.RobPtr


class VIexWrapper(implicit p : Parameters) extends Module {
  
  val io = IO(new Bundle {
    val in = Input(ValidIO(new Muop))
    val out = ValidIO(new VAluOutput)
    val iexNeedStall = Output(Bool())
  })

  val SValu  = Module(new VAluWrapper()(p))
  val SVMac  = Module(new VMacWrapper()(p))
  val SVMask = Module(new VMaskWrapper()(p))

  val empty :: ongoing :: Nil = Enum(2)
  val currentState = RegInit(empty)
  val currentStateNext = WireDefault(empty) 

  val outValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid

  switch(currentState){
    is(empty){
      when(io.in.valid && ~io.in.bits.uop.ctrl.alu && ~io.in.bits.uop.ctrl.mask){
        currentStateNext := ongoing
      }.otherwise{
        currentStateNext := empty
      }
    }
    is(ongoing){
      when(outValid){
          currentStateNext := empty
      }.otherwise{
          currentStateNext := ongoing
      }
    }
  } 

  currentState := currentStateNext
  io.iexNeedStall := (currentStateNext === ongoing)
  assert(!(currentState === ongoing && io.in.valid), "when current state is ongoing, should not has new inst in")
  

  SValu.io.in.valid  := io.in.valid && io.in.bits.uop.ctrl.alu
  SVMac.io.in.valid  := io.in.valid && io.in.bits.uop.ctrl.mul
  SVMask.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.mask

  SValu.io.in.bits.vfuInput.uop   := io.in.bits.uop
  SValu.io.in.bits.vfuInput.vs1   := io.in.bits.uopRegInfo.vs1
  SValu.io.in.bits.vfuInput.vs2   := io.in.bits.uopRegInfo.vs2
  SValu.io.in.bits.vfuInput.rs1   := io.in.bits.scalar_opnd_1
  SValu.io.in.bits.vfuInput.oldVd := io.in.bits.uopRegInfo.old_vd
  SValu.io.in.bits.vfuInput.mask  := io.in.bits.uopRegInfo.mask

  SVMac.io.in.bits.vfuInput.uop   := io.in.bits.uop
  SVMac.io.in.bits.vfuInput.vs1   := io.in.bits.uopRegInfo.vs1
  SVMac.io.in.bits.vfuInput.vs2   := io.in.bits.uopRegInfo.vs2
  SVMac.io.in.bits.vfuInput.rs1   := io.in.bits.scalar_opnd_1
  SVMac.io.in.bits.vfuInput.oldVd := io.in.bits.uopRegInfo.old_vd
  SVMac.io.in.bits.vfuInput.mask  := io.in.bits.uopRegInfo.mask

  SVMask.io.in.bits.vfuInput.uop   := io.in.bits.uop
  SVMask.io.in.bits.vfuInput.vs1   := io.in.bits.uopRegInfo.vs1
  SVMask.io.in.bits.vfuInput.vs2   := io.in.bits.uopRegInfo.vs2
  SVMask.io.in.bits.vfuInput.rs1   := io.in.bits.scalar_opnd_1
  SVMask.io.in.bits.vfuInput.oldVd := io.in.bits.uopRegInfo.old_vd
  SVMask.io.in.bits.vfuInput.mask  := io.in.bits.uopRegInfo.mask

  io.out.bits  := Mux(SVMask.io.out.valid,SVMask.io.out.bits,Mux(SVMac.io.out.valid,SVMac.io.out.bits,SValu.io.out.bits))
  io.out.valid := outValid

}





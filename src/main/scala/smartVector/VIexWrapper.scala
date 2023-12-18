package smartVector

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.mac._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.VInstructions._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chipsalliance.rocketchip.config
import xiangshan.backend.rob.RobPtr

class IexOutput extends Bundle {
  val vd = UInt(128.W)
  val vxsat = Bool()
  val fflags = UInt(5.W)
}

class VIexWrapper(implicit p : Parameters) extends Module {
  
  val io = IO(new Bundle {
    val in = Input(ValidIO(new Muop))
    val out = ValidIO(new IexOutput)
    val iexNeedStall = Output(Bool())
  })

  val SValu   = Module(new VAluWrapper()(p))
  val SVMac   = Module(new VMacWrapper()(p))
  val SVMask  = Module(new VMaskWrapper()(p))
  val SVReduc = Module(new VReducWrapper()(p))
  val SVDiv   = Module(new VDivWrapper()(p))

  val empty :: ongoing :: Nil = Enum(2)
  val currentState = RegInit(empty)
  val currentStateNext = WireDefault(empty) 

  val outValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid || 
                 SVReduc.io.out.valid || SVDiv.io.out.valid

  switch(currentState){
    is(empty){
      when(io.in.valid && ~io.in.bits.uop.ctrl.alu && ~io.in.bits.uop.ctrl.isLdst && ~io.in.bits.uop.ctrl.mask){
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
  assert(!(!SVDiv.io.in.ready && io.in.valid), "when div is not ready, should not has new inst in")
  
  SValu.io.in.valid   := io.in.valid && io.in.bits.uop.ctrl.alu
  SVMac.io.in.valid   := io.in.valid && io.in.bits.uop.ctrl.mul
  SVMask.io.in.valid  := io.in.valid && io.in.bits.uop.ctrl.mask
  SVReduc.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.redu
  SVDiv.io.in.valid   := io.in.valid && io.in.bits.uop.ctrl.div

  Seq(SValu.io.in.bits, SVMac.io.in.bits, SVMask.io.in.bits, SVReduc.io.in.bits, SVDiv.io.in.bits).foreach {fu =>
    fu.uop   := io.in.bits.uop
    fu.vs1   := io.in.bits.uopRegInfo.vs1
    fu.vs2   := io.in.bits.uopRegInfo.vs2
    fu.rs1   := io.in.bits.scalar_opnd_1
    fu.oldVd := io.in.bits.uopRegInfo.old_vd
    fu.mask  := io.in.bits.uopRegInfo.mask
  }

  val fixCycleResult = Wire(new VAluOutput)

  val fixCycleVld1H = Seq(SValu.io.out.valid, SVMac.io.out.valid, SVMask.io.out.valid, 
                          SVReduc.io.out.valid)

  val fixCycleResult1H = Seq(SValu.io.out.bits, SVMac.io.out.bits, SVMask.io.out.bits,
                             SVReduc.io.out.bits)

  fixCycleResult  := Mux1H(fixCycleVld1H, fixCycleResult1H)

  val fixCycleValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid || SVReduc.io.out.valid
  
  when(fixCycleValid){
    io.out.bits.fflags := false.B
    io.out.bits.vd     := fixCycleResult.vd
    io.out.bits.vxsat  := fixCycleResult.vxsat
  }.elsewhen(SVDiv.io.out.valid){
    io.out.bits.fflags := SVDiv.io.out.bits.fflags
    io.out.bits.vd     := SVDiv.io.out.bits.vd
    io.out.bits.vxsat  := false.B
  }.otherwise{
    io.out.bits.fflags := false.B
    io.out.bits.vd     := 0.U
    io.out.bits.vxsat  := false.B
  }
  io.out.valid := outValid

}





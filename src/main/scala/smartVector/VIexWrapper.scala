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

class VPermRegIn extends Bundle{
  val rdata = UInt(128.W)
  val rvalid = Bool()
}

class VIexWrapper(implicit p : Parameters) extends Module {
  
  val io = IO(new Bundle {
    val in = Input(ValidIO(new Muop))
    val out = ValidIO(new IexOutput)
    val permOut = new(VPermOutput)
    val permRegIn = Input(new(VPermRegIn))
    val iexNeedStall = Output(Bool())
  })

  val SValu   = Module(new VAluWrapper()(p))
  val SVMac   = Module(new VMacWrapper()(p))
  val SVMask  = Module(new VMaskWrapper()(p))
  val SVReduc = Module(new VReducWrapper()(p))
  val SVDiv   = Module(new VDivWrapper()(p))
  val SVPerm  = Module(new VPermWrapper()(p))

  val empty :: ongoing :: Nil = Enum(2)
  val currentState = RegInit(empty)
  val currentStateNext = WireDefault(empty) 

  val outValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid || 
                 SVReduc.io.out.valid || SVDiv.io.out.valid || SVPerm.io.out.wb_vld

  val oneCycleLatIn = io.in.valid & (io.in.bits.uop.ctrl.alu || io.in.bits.uop.ctrl.mask)
  val twoCycleLatIn = io.in.valid & (io.in.bits.uop.ctrl.mul || io.in.bits.uop.ctrl.redu)
  val noFixLatIn    = io.in.valid & (io.in.bits.uop.ctrl.div || io.in.bits.uop.ctrl.perm)
  val twoCycleReg = RegEnable(twoCycleLatIn, io.in.valid)
  val fixLatVld   = SVDiv.io.out.valid || SVPerm.io.out.wb_vld

  switch(currentState){
    is(empty){
      when(io.in.valid && ~io.in.bits.uop.ctrl.alu && ~io.in.bits.uop.ctrl.isLdst && ~io.in.bits.uop.ctrl.mask){
        currentStateNext := ongoing
      }.otherwise{
        currentStateNext := empty
      }
    }
    is(ongoing){
      when(twoCycleReg || fixLatVld){
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
  assert(!(SVPerm.io.out.perm_busy && io.in.valid), "when perm is busy, should not has new inst in")
  
  SValu.io.in.valid   := io.in.valid && io.in.bits.uop.ctrl.alu
  SVMac.io.in.valid   := io.in.valid && io.in.bits.uop.ctrl.mul
  SVMask.io.in.valid  := io.in.valid && io.in.bits.uop.ctrl.mask
  SVReduc.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.redu
  SVDiv.io.in.valid   := io.in.valid && io.in.bits.uop.ctrl.div
  SVPerm.io.in.rvalid := io.in.valid && io.in.bits.uop.ctrl.perm

  Seq(SValu.io.in.bits, SVMac.io.in.bits, SVMask.io.in.bits, SVReduc.io.in.bits, SVDiv.io.in.bits).foreach {iex =>
    iex.uop   := io.in.bits.uop
    iex.vs1   := io.in.bits.uopRegInfo.vs1
    iex.vs2   := io.in.bits.uopRegInfo.vs2
    iex.rs1   := io.in.bits.scalar_opnd_1
    iex.oldVd := io.in.bits.uopRegInfo.old_vd
    iex.mask  := io.in.bits.uopRegInfo.mask
  }

  SVPerm.io.in.uop := io.in.bits.uop
  SVPerm.io.in.rs1 := io.in.bits.scalar_opnd_1 || fudian
  SVPerm.io.in.vs1_preg_idx := VecInit(Seq.tabulate(8)(i => io.in.bits.uop.ctrl.lsrc(0) + i.U))
  SVPerm.io.in.vs2_preg_idx := VecInit(Seq.tabulate(8)(i => io.in.bits.uop.ctrl.lsrc(1) + i.U))
  SVPerm.io.in.old_vd_preg_idx := VecInit(Seq.tabulate(8)(i => io.in.bits.uop.ctrl.ldest + i.U))
  SVPerm.io.in.mask_preg_idx := 0.U cunyi
  SVPerm.io.in.uop_valid := io.in.valid & io.in.bits.uop.ctrl.perm
  SVPerm.io.in.rdata := io.permRegIn.rdata
  SVPerm.io.in.rvalid := io.permRegIn.rvalid


  io.permOut := SVPerm.io.out

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





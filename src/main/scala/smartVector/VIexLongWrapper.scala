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
import darecreek.Vlmul_to_lmul

class IexOutput extends Bundle {
  val vd = UInt(128.W)
  val vxsat = Bool()
  val fflags = UInt(5.W)
}

class VPermRegIn extends Bundle{
  val rdata = UInt(128.W)
  val rvalid = Bool()
}

class VIexLongWrapper(implicit p : Parameters) extends Module {
  
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
  val SVFpu   = Module(new VSFPUWrapper()(p))

  val validReg = RegInit(false.B)
  val bitsReg  = RegInit(0.U.asTypeOf(new Muop))
  //val divNotReady  = io.in.bits.uop.ctrl.div  & ~SVDiv.io.in.ready
  //val fpuNotReady  = io.in.bits.uop.ctrl.fp   & ~SVFpu.io.in.ready
  //val permNotReady = io.in.bits.uop.ctrl.perm & SVPerm.io.out.perm_busy
  val divNotReady  = ~SVDiv.io.in.ready
  val fpuNotReady  = ~SVFpu.io.in.ready
  val permNotReady = SVPerm.io.out.perm_busy
  val ready    = ~(divNotReady || fpuNotReady || permNotReady)

  when(!validReg || ready){
    validReg := io.in.valid && !io.in.bits.uop.ctrl.isLdst
  }

  when(io.in.valid){
    bitsReg := io.in.bits
  }

  val empty :: ongoing :: Nil = Enum(2)
  val currentState = RegInit(empty)
  val currentStateNext = WireDefault(empty) 

  val outValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid || 
                 SVReduc.io.out.valid || SVDiv.io.out.valid || SVFpu.io.out.valid

  val permDone = Wire(Bool())
  val permWriteNum = RegInit(0.U(4.W))
  when(SVPerm.io.out.wb_vld){
      permWriteNum := permWriteNum + 1.U
  }

  when(SVPerm.io.out.wb_vld && (permWriteNum + 1.U === Vlmul_to_lmul(SVPerm.io.out.uop.info.vlmul))){
      permWriteNum := 0.U
      permDone := true.B
  }.otherwise{
      permDone := false.B
  }

  val oneCycleLatIn = validReg & (bitsReg.uop.ctrl.alu || bitsReg.uop.ctrl.mask)
  val twoCycleLatIn = validReg & (bitsReg.uop.ctrl.mul || bitsReg.uop.ctrl.redu)
  val noFixLatIn    = validReg & (bitsReg.uop.ctrl.div || bitsReg.uop.ctrl.perm || bitsReg.uop.ctrl.fp)
  val twoCycleReg = RegEnable(twoCycleLatIn, validReg)
  val fixLatVld   = SVDiv.io.out.valid || permDone || SVFpu.io.out.valid

  switch(currentState){
    is(empty){
      when(validReg && ~bitsReg.uop.ctrl.alu && ~bitsReg.uop.ctrl.isLdst && ~bitsReg.uop.ctrl.mask){
        currentStateNext := ongoing
      }.otherwise{
        currentStateNext := empty
      }
    }
    is(ongoing){
      when(twoCycleReg || fixLatVld || bitsReg.uop.ctrl.floatRed && SVFpu.io.in.ready && ~bitsReg.uop.uopEnd){
          currentStateNext := empty
      }.otherwise{
          currentStateNext := ongoing
      }
    }
  } 

  currentState := currentStateNext
  val iexNeedStallTmp = (currentStateNext === ongoing) || ~ready

  //if is floatRed, when is ready, the next uop valid will be high in same cycle.
  //and the first's ready match the second's valid, it will cause second's ready invalid
  io.iexNeedStall := Mux(bitsReg.uop.ctrl.floatRed && !iexNeedStallTmp , RegNext(iexNeedStallTmp) , iexNeedStallTmp)
  //assert(!(currentState === ongoing && validFinal), "when current state is ongoing, should not has new inst in")
  //assert(!(!SVDiv.io.in.ready && validFinal), "when div is not ready, should not has new inst in")
  //assert(!(SVPerm.io.out.perm_busy && validFinal), "when perm is busy, should not has new inst in")
  
  SValu.io.in.valid   := validReg && bitsReg.uop.ctrl.alu
  SVMac.io.in.valid   := validReg && bitsReg.uop.ctrl.mul
  SVMask.io.in.valid  := validReg && bitsReg.uop.ctrl.mask
  SVReduc.io.in.valid := validReg && bitsReg.uop.ctrl.redu
  SVDiv.io.in.valid   := validReg && bitsReg.uop.ctrl.div
  SVPerm.io.in.rvalid := validReg && bitsReg.uop.ctrl.perm
  SVFpu.io.in.valid   := validReg && bitsReg.uop.ctrl.fp

  Seq(SValu.io.in.bits, SVMac.io.in.bits, SVMask.io.in.bits, SVReduc.io.in.bits, SVDiv.io.in.bits, SVFpu.io.in.bits).foreach {iex =>
    iex.uop   := bitsReg.uop
    iex.vs1   := bitsReg.uopRegInfo.vs1
    iex.vs2   := bitsReg.uopRegInfo.vs2
    iex.rs1   := bitsReg.scalar_opnd_1
    iex.oldVd := bitsReg.uopRegInfo.old_vd
    iex.mask  := bitsReg.uopRegInfo.mask
  }

  SVPerm.io.in.uop := bitsReg.uop
  //TODO: when id float inst, the rs1 should read from float register file
  SVPerm.io.in.rs1 := bitsReg.scalar_opnd_1 // || float

  SVPerm.io.in.vs1_preg_idx    := VecInit(Seq.tabulate(8)(i => bitsReg.uop.ctrl.lsrc(0) + Mux(io.in.bits.uop.ctrl.vGatherEi16EEW32, i.U >> 1, Mux(io.in.bits.uop.ctrl.vGatherEi16EEW64, i.U >> 2, i.U))))
  SVPerm.io.in.vs2_preg_idx    := VecInit(Seq.tabulate(8)(i => bitsReg.uop.ctrl.lsrc(1) + i.U))
  SVPerm.io.in.old_vd_preg_idx := VecInit(Seq.tabulate(8)(i => bitsReg.uop.ctrl.ldest   + Mux(io.in.bits.uop.ctrl.vGatherEi16EEW8, i.U >> 1, i.U)))
  SVPerm.io.in.mask_preg_idx := 0.U
  SVPerm.io.in.uop_valid := validReg & bitsReg.uop.ctrl.perm
  SVPerm.io.in.rdata := io.permRegIn.rdata
  SVPerm.io.in.rvalid := io.permRegIn.rvalid
  SVPerm.io.redirect.valid := false.B
  SVPerm.io.redirect.bits := DontCare

  io.permOut := SVPerm.io.out

  val fixCycleResult = Wire(new VAluOutput)

  val fixCycleVld1H = Seq(SValu.io.out.valid, SVMac.io.out.valid, SVMask.io.out.valid, 
                          SVReduc.io.out.valid)

  val fixCycleResult1H = Seq(SValu.io.out.bits, SVMac.io.out.bits, SVMask.io.out.bits,
                             SVReduc.io.out.bits)

  fixCycleResult  := Mux1H(fixCycleVld1H, fixCycleResult1H)

  val fixCycleValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid || SVReduc.io.out.valid
  
  when(fixCycleValid){
    io.out.bits.fflags := 0.U
    io.out.bits.vd     := fixCycleResult.vd
    io.out.bits.vxsat  := fixCycleResult.vxsat
  }.elsewhen(SVDiv.io.out.valid){
    io.out.bits.fflags := SVDiv.io.out.bits.fflags
    io.out.bits.vd     := SVDiv.io.out.bits.vd
    io.out.bits.vxsat  := false.B
  }.elsewhen(SVFpu.io.out.valid){
    io.out.bits.fflags := SVFpu.io.out.bits.fflags
    io.out.bits.vd     := SVFpu.io.out.bits.vd
    io.out.bits.vxsat  := false.B
  }otherwise{
    io.out.bits.fflags := 0.U
    io.out.bits.vd     := 0.U
    io.out.bits.vxsat  := false.B
  }
  io.out.valid := outValid
}





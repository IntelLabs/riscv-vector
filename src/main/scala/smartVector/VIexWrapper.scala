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

class VIexWrapper(implicit p : Parameters) extends Module {
  
  val io = IO(new Bundle {
    val in = Input(ValidIO(new Muop))
    val out = ValidIO(new IexOutput)
    val permOut = new(VPermOutput)
    val permRegIn = Input(new(VPermRegIn))
    val iexNeedStall = Output(Bool())
  })

  // IEX modules
  val SValu   = Module(new VAluWrapper()(p))
  val SVMac   = Module(new VMacWrapper()(p))
  val SVMask  = Module(new VMaskWrapper()(p))
  val SVReduc = Module(new VReducWrapper()(p))
  val SVDiv   = Module(new VDivWrapper()(p))
  val SVPerm  = Module(new VPermWrapper()(p))
  val SVFpu   = Module(new VSFPUWrapper()(p))

  val empty :: ongoing :: Nil = Enum(2)
  val currentState = RegInit(empty)
  val currentStateNext = WireDefault(empty) 

  // IEX input source
  //val mUop = io.in.bits
  val mUopValid = io.in.valid && ~io.in.bits.uop.ctrl.isLdst

  //generate buffer
  val bufferReg = RegInit(0.U.asTypeOf(new Muop))
  val bufferValidReg = RegInit(false.B)

  //set buffer
  val sameType = Wire(Bool())
  val currentModuleReg = RegInit(0.U(3.W))
  sameType := (io.in.bits.uop.ctrl.alu || io.in.bits.uop.ctrl.mask) && currentModuleReg === 1.U ||
              (io.in.bits.uop.ctrl.mul || io.in.bits.uop.ctrl.redu) && currentModuleReg === 2.U ||
              io.in.bits.uop.ctrl.div && currentModuleReg === 3.U && SVDiv.io.in.ready ||
              io.in.bits.uop.ctrl.fp  && currentModuleReg === 4.U && SVFpu.io.in.ready ||
              io.in.bits.uop.ctrl.perm && currentModuleReg === 5.U && ~SVPerm.io.out.perm_busy

  when(!bufferValidReg && mUopValid && ~sameType && currentState === ongoing){
    bufferValidReg := mUopValid
    bufferReg := io.in.bits
  }

  //when send to execute module, clear buffer
  when(currentState === empty || currentState === ongoing && sameType){
    bufferValidReg := false.B
  }

  //Mux data from input and buffer
  val muxData = Wire(new Muop)
  val muxValid = Wire(Bool())

  muxData := Mux(bufferValidReg, bufferReg, io.in.bits)
  muxValid := Mux(bufferValidReg, bufferValidReg, mUopValid)

  // val divNotReady  = ~SVDiv.io.in.ready
  // val fpuNotReady  = ~SVFpu.io.in.ready
  // val permNotReady = SVPerm.io.out.perm_busy
  // val ready    = ~(divNotReady || fpuNotReady || permNotReady)

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

  val oneCycleLatIn = muxValid & (muxData.uop.ctrl.alu || muxData.uop.ctrl.mask)
  val twoCycleLatIn = muxValid & (muxData.uop.ctrl.mul || muxData.uop.ctrl.redu)
  val noFixLatIn    = muxValid & (muxData.uop.ctrl.div || muxData.uop.ctrl.perm || muxData.uop.ctrl.fp)
  val twoCycleReg   = RegEnable(twoCycleLatIn, muxValid)
  val fixLatVld     = SVDiv.io.out.valid || permDone || SVFpu.io.out.valid

  switch(currentState){
    is(empty){
      when(muxValid && ~muxData.uop.ctrl.alu && ~muxData.uop.ctrl.mask){
      //&& ~(muxData.uop.ctrl.narrow_to_1 && ~muxData.uop.uopEnd)){
        currentStateNext := ongoing
      }.otherwise{
        currentStateNext := empty
      }
    }
    is(ongoing){
      when(muxValid && ~muxData.uop.ctrl.alu && ~muxData.uop.ctrl.mask){
        currentStateNext := ongoing
      }.elsewhen(twoCycleReg || fixLatVld){
      //when(twoCycleReg || fixLatVld  || (muxData.uop.ctrl.floatRed && SVFpu.io.in.ready && ~(muxData.uop.uopIdx === 0.U))){
          currentStateNext := empty
      }.otherwise{
          currentStateNext := ongoing
      }
    }
  } 

  currentState := currentStateNext
 
  SValu.io.in.valid   := muxValid && muxData.uop.ctrl.alu
  SVMac.io.in.valid   := muxValid && muxData.uop.ctrl.mul
  SVMask.io.in.valid  := muxValid && muxData.uop.ctrl.mask
  SVReduc.io.in.valid := muxValid && muxData.uop.ctrl.redu
  SVDiv.io.in.valid   := muxValid && muxData.uop.ctrl.div
  SVPerm.io.in.rvalid := muxValid && muxData.uop.ctrl.perm
  SVFpu.io.in.valid   := muxValid && muxData.uop.ctrl.fp

  when(SValu.io.in.valid || SVMask.io.in.valid){
    currentModuleReg := 1.U
  }.elsewhen(SVMac.io.in.valid || SVReduc.io.in.valid){
    currentModuleReg := 2.U
  }.elsewhen(SVDiv.io.in.valid){
    currentModuleReg := 3.U
  }.elsewhen(SVFpu.io.in.valid){
    currentModuleReg := 4.U
  }.elsewhen(SVPerm.io.in.rvalid){
    currentModuleReg := 5.U
  }.otherwise{
    currentModuleReg := 0.U
  }


  Seq(SValu.io.in.bits, SVMac.io.in.bits, SVMask.io.in.bits, SVReduc.io.in.bits, SVDiv.io.in.bits, SVFpu.io.in.bits).foreach {iex =>
    iex.uop   := muxData.uop
    iex.vs1   := muxData.uopRegInfo.vs1
    iex.vs2   := muxData.uopRegInfo.vs2
    iex.rs1   := muxData.scalar_opnd_1
    iex.oldVd := muxData.uopRegInfo.old_vd
    iex.mask  := muxData.uopRegInfo.mask
  }

  SVPerm.io.in.uop := muxData.uop
  //TODO: when id float inst, the rs1 should read from float register file
  SVPerm.io.in.rs1 := muxData.scalar_opnd_1 // || float

  SVPerm.io.in.vs1_preg_idx    := VecInit(Seq.tabulate(8)(i => muxData.uop.ctrl.lsrc(0) +
                                  Mux(io.in.bits.uop.ctrl.vGatherEi16EEW32, i.U >> 1, 
                                  Mux(io.in.bits.uop.ctrl.vGatherEi16EEW64, i.U >> 2, i.U))))
  SVPerm.io.in.vs2_preg_idx    := VecInit(Seq.tabulate(8)(i => muxData.uop.ctrl.lsrc(1) + i.U))
  SVPerm.io.in.old_vd_preg_idx := VecInit(Seq.tabulate(8)(i => muxData.uop.ctrl.ldest   + 
                                  Mux(io.in.bits.uop.ctrl.vGatherEi16EEW8, i.U >> 1, i.U)))
  SVPerm.io.in.mask_preg_idx := 0.U
  SVPerm.io.in.uop_valid := muxValid & muxData.uop.ctrl.perm
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

  io.iexNeedStall := bufferValidReg
}




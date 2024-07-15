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

class IexOut(implicit p : Parameters) extends Bundle{
  val toRegFileWrite = new regWriteIn
  val commitInfo =  ValidIO(new CommitInfo)
}

class IexIn(implicit p : Parameters) extends Bundle{
  val mUop = new Muop
  val mergeInfo = new MuopMergeAttr
}

class VIexWrapper(implicit p : Parameters) extends Module {
  
  val io = IO(new Bundle {
    val in = Input(ValidIO(new IexIn))
    val out = Output(ValidIO(new IexOut))
    val permOut = new PermReadRF
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
  val mUopValid = io.in.valid && ~io.in.bits.mUop.uop.ctrl.isLdst

  //generate buffer
  val bufferReg = RegInit(0.U.asTypeOf(new IexIn))
  val bufferValidReg = RegInit(false.B)

  //set buffer
  val sameType = Wire(Bool())
  val currentModuleReg = RegInit(0.U(3.W))
  sameType := (io.in.bits.mUop.uop.ctrl.alu || io.in.bits.mUop.uop.ctrl.mask) && currentModuleReg === 1.U ||
              (io.in.bits.mUop.uop.ctrl.mul || io.in.bits.mUop.uop.ctrl.redu) && currentModuleReg === 2.U ||
               io.in.bits.mUop.uop.ctrl.div && currentModuleReg === 3.U && SVDiv.io.in.ready ||
               io.in.bits.mUop.uop.ctrl.fp  && currentModuleReg === 4.U && SVFpu.io.in.ready ||
               io.in.bits.mUop.uop.ctrl.perm && currentModuleReg === 5.U && ~SVPerm.io.perm_busy

  // FIFO队列用于维护指令计数
  //val maxInstructions : Int = 16
  //val instructionQueue = Module(new Queue(new Bundle {
  //  val uopEnd = Bool()
  //}, maxInstructions))
  //val instructionCounter = instructionQueue.io.count
  val instNum = RegInit(0.U(5.W))

  //when(!bufferValidReg && mUopValid && (~sameType && currentState === ongoing || instructionQueue.io.count === 16.U)){
  when(!bufferValidReg && mUopValid && (~sameType && currentState === ongoing || instNum === 31.U)){
    bufferValidReg := mUopValid
    bufferReg := io.in.bits
  }

  //when send to execute module, clear buffer
  when(currentState === empty || currentState === ongoing && sameType){
    bufferValidReg := false.B
  }

  //Mux data from input and buffer
  val muxData = Wire(new IexIn)
  val muxValid = Wire(Bool())

  muxData := Mux(bufferValidReg, bufferReg, io.in.bits)
  muxValid := Mux(bufferValidReg, bufferValidReg, mUopValid)

  val outValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid || 
                 SVReduc.io.out.valid || SVDiv.io.out.valid || SVFpu.io.out.valid || SVPerm.io.out.valid

  val resultNarrowTo1 = muxData.mUop.uop.ctrl.narrow_to_1 || muxData.mUop.uop.ctrl.floatRed

  switch(currentState){
    is(empty){
      when(muxValid && (~resultNarrowTo1 || muxData.mUop.uop.uopEnd)){
          //instructionQueue.io.enq.valid := true.B
          //instructionQueue.io.enq.bits.uopEnd := io.in.bits.uop.uopEnd
          instNum := instNum + 1.U       
      }
    }
    is(ongoing){
      when(muxValid && (~resultNarrowTo1 || muxData.mUop.uop.uopEnd)){
          //instructionQueue.io.enq.valid := true.B
          //instructionQueue.io.enq.bits.uopEnd := io.in.bits.uop.uopEnd
          instNum := instNum + 1.U 
      }.elsewhen(outValid){
        //instructionQueue.io.deq.valid := true.B
        //instructionQueue.io.deq.bits     
        instNum := instNum - 1.U
      }
    }
  }

  //when(instructionCounter === 0.U){
  when(instNum === 0.U){
    currentStateNext := empty
  }.otherwise{
    currentStateNext := ongoing
  } 

  currentState := currentStateNext
 
  SValu.io.in.valid   := muxValid && muxData.mUop.uop.ctrl.alu
  SVMac.io.in.valid   := muxValid && muxData.mUop.uop.ctrl.mul
  SVMask.io.in.valid  := muxValid && muxData.mUop.uop.ctrl.mask
  SVReduc.io.in.valid := muxValid && muxData.mUop.uop.ctrl.redu
  SVDiv.io.in.valid   := muxValid && muxData.mUop.uop.ctrl.div
  SVPerm.io.in.rvalid := muxValid && muxData.mUop.uop.ctrl.perm
  SVFpu.io.in.valid   := muxValid && muxData.mUop.uop.ctrl.fp

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
    iex.uop.ctrl                    := muxData.mUop.uop.ctrl
    iex.uop.info                    := muxData.mUop.uop.info
    iex.uop.uopIdx                  := muxData.mUop.uop.uopIdx
    iex.uop.uopEnd                  := muxData.mUop.uop.uopEnd
    iex.uop.sysUop.scalarRegWriteEn := muxData.mergeInfo.scalarRegWriteEn
    iex.uop.sysUop.floatRegWriteEn  := muxData.mergeInfo.floatRegWriteEn
    iex.uop.sysUop.rfWriteEn        := muxData.mergeInfo.rfWriteEn
    iex.uop.sysUop.ldest            := muxData.mergeInfo.ldest
    iex.uop.sysUop.permExpdLen      := muxData.mergeInfo.permExpdLen
    iex.uop.sysUop.regDstIdx        := muxData.mergeInfo.regDstIdx
    iex.uop.sysUop.regCount         := muxData.mergeInfo.regCount
    iex.uop.sysUop.robIdx           := 0.U.asTypeOf(new RobPtr)
    iex.vs1   := muxData.mUop.uopRegInfo.vs1
    iex.vs2   := muxData.mUop.uopRegInfo.vs2
    iex.rs1   := muxData.mUop.scalar_opnd_1
    iex.oldVd := muxData.mUop.uopRegInfo.old_vd
    iex.mask  := muxData.mUop.uopRegInfo.mask
  }

  Seq(SValu.io.mergeInfo, SVMac.io.mergeInfo, SVMask.io.mergeInfo, SVReduc.io.mergeInfo).foreach {iex =>
    iex       := muxData.mergeInfo
  }
  SVPerm.io.in.uop.ctrl                    := muxData.mUop.uop.ctrl
  SVPerm.io.in.uop.info                    := muxData.mUop.uop.info
  SVPerm.io.in.uop.uopIdx                  := muxData.mUop.uop.uopIdx
  SVPerm.io.in.uop.uopEnd                  := muxData.mUop.uop.uopEnd
  SVPerm.io.in.uop.sysUop.scalarRegWriteEn := muxData.mergeInfo.scalarRegWriteEn
  SVPerm.io.in.uop.sysUop.floatRegWriteEn  := muxData.mergeInfo.floatRegWriteEn
  SVPerm.io.in.uop.sysUop.rfWriteEn        := muxData.mergeInfo.rfWriteEn
  SVPerm.io.in.uop.sysUop.ldest            := muxData.mergeInfo.ldest
  SVPerm.io.in.uop.sysUop.permExpdLen      := muxData.mergeInfo.permExpdLen
  SVPerm.io.in.uop.sysUop.regDstIdx        := muxData.mergeInfo.regDstIdx
  SVPerm.io.in.uop.sysUop.regCount         := muxData.mergeInfo.regCount
  SVPerm.io.in.uop.sysUop.robIdx           := 0.U.asTypeOf(new RobPtr)
  //TODO: when id float inst, the rs1 should read from float register file
  SVPerm.io.in.rs1 := muxData.mUop.scalar_opnd_1 // || float

  SVPerm.io.in.vs1_preg_idx    := VecInit(Seq.tabulate(8)(i => muxData.mUop.uop.ctrl.lsrc(0) +
                                  Mux(io.in.bits.mUop.uop.ctrl.vGatherEi16EEW32, i.U >> 1, 
                                  Mux(io.in.bits.mUop.uop.ctrl.vGatherEi16EEW64, i.U >> 2, i.U))))
  SVPerm.io.in.vs2_preg_idx    := VecInit(Seq.tabulate(8)(i => muxData.mUop.uop.ctrl.lsrc(1) + i.U))
  SVPerm.io.in.old_vd_preg_idx := VecInit(Seq.tabulate(8)(i => muxData.mUop.uop.ctrl.ldest   + 
                                  Mux(io.in.bits.mUop.uop.ctrl.vGatherEi16EEW8, i.U >> 1, i.U)))
  SVPerm.io.in.mask_preg_idx := 0.U
  SVPerm.io.in.uop_valid := muxValid & muxData.mUop.uop.ctrl.perm
  SVPerm.io.in.rdata := io.permRegIn.rdata
  SVPerm.io.in.rvalid := io.permRegIn.rvalid
  SVPerm.io.redirect.valid := false.B
  SVPerm.io.redirect.bits := DontCare

  io.permOut.rd_en  := SVPerm.io.permReadRF.rd_en
  io.permOut.rd_preg_idx := SVPerm.io.permReadRF.rd_preg_idx
 

  val Result = Wire(new IexOut)

  val CycleVld1H = Seq(SValu.io.out.valid, SVMac.io.out.valid, SVMask.io.out.valid, SVReduc.io.out.valid,
                       SVDiv.io.out.valid, SVFpu.io.out.valid, SVPerm.io.out.valid)

  val CycleResult1H = Seq(SValu.io.out.bits, SVMac.io.out.bits, SVMask.io.out.bits, SVReduc.io.out.bits,
                       SVDiv.io.out.bits, SVFpu.io.out.bits, SVPerm.io.out.bits)

  Result  := Mux1H(CycleVld1H, CycleResult1H)

  val fixCycleValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid || SVReduc.io.out.valid
  
  io.out.bits := Result
  io.out.valid := fixCycleValid || SVPerm.io.out.valid || SVFpu.io.out.valid || SVPerm.io.out.valid

  io.iexNeedStall := bufferValidReg
}




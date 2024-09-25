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
import _root_.darecreek.exu.vfucore.fp.VFPU.S

//class IexOutput extends Bundle {
//  val vd = UInt(128.W)
//  val vxsat = Bool()
//  val fflags = UInt(5.W)
//}
//
//class VPermRegIn extends Bundle{
//  val rdata = UInt(128.W)
//  val rvalid = Bool()
//}
//
//class IexOut(implicit p : Parameters) extends Bundle{
//  val toRegFileWrite = new regWriteIn
//  val commitInfo =  ValidIO(new CommitInfo)
//}
//
//class IexIn(implicit p : Parameters) extends Bundle{
//  val mUop = new Muop
//  val mergeInfo = new MuopMergeAttr
//}

class VIexWrapperNew extends Module {
  
  val io = IO(new Bundle {
    val in = Input(ValidIO(new IexIn))
    val out = Output(ValidIO(new IexOut))
    val permOut = new PermReadRF
    val permRegIn = Input(new VPermRegIn)
    val iexNeedStall = Output(Bool())
    val readSBoardSetIO = Flipped(new ScoreboardSetIO)
    val readSBoardReadIO = Flipped(new ScoreboardReadIO)
    val readSBoardClearIO = Flipped(new ScoreboardClearIO)
  })

  // IEX modules
  val crossLane = Module(new SVCrossLaneExu())
  val lane = Module(new SVLaneExu())

  val empty :: ongoing :: Nil = Enum(2)
  val currentState = RegInit(empty)
  val currentStateNext = WireDefault(empty) 

  // IEX input source
  //val mUop = io.in.bits
  val mUopValid = io.in.valid && ~io.in.bits.mUop.uop.ctrl.isLdst

  //generate buffer
  val bufferReg = RegInit(0.U.asTypeOf(new IexIn))
  val bufferValidReg = RegInit(false.B)

  val ctrl = io.in.bits.mUop.uop.ctrl
  val bufferCtrl = bufferReg.mUop.uop.ctrl
  val validVec = VecInit(Seq(ctrl.alu, ctrl.mul, ctrl.fp, ctrl.redu, ctrl.mask, ctrl.perm, ctrl.div))
  val bufferValidVec = VecInit(Seq(bufferCtrl.alu, bufferCtrl.mul, bufferCtrl.fp, bufferCtrl.redu, bufferCtrl.mask, bufferCtrl.perm, bufferCtrl.div))
  val readyVec = VecInit(lane.io.in.readys, crossLane.io.in.readys)
  
  val inAccept = (validVec zip readyVec).map{case(v,r) => v && r}.reduce(_ | _) 
  val bufferAccept = (bufferValidVec zip readyVec).map{case(v,r) => v && r}.reduce(_ | _)

  val muxData = Wire(new IexIn)
  val muxValid = Wire(Bool())

  when(bufferValidReg && bufferAccept){
    muxValid := true.B
    muxData := bufferReg
    bufferValidReg := false.B
  }.elsewhen(mUopValid && inAccept){
    muxValid := true.B
    muxData := io.in.bits
  }.otherwise{
    muxValid := false.B
  }

  when(~bufferValidReg && mUopValid && ~inAccept){
    bufferValidReg := mUopValid
    bufferReg := io.in.bits
  }

  val outValid = crossLane.io.out.valid || lane.io.out.valid

  val muxCtrl = muxData.mUop.uop.ctrl
  crossLane.io.in.valid := muxCtrl.redu || muxCtrl.mask || muxCtrl.perm || muxCtrl.div
  lane.io.in.valid := muxCtrl.alu || muxCtrl.mul || muxCtrl.fp
  
  Seq(lane.io.in.bits, crossLane.io.in.bits).foreach {iex =>
    iex.uop.expdLen                 := muxData.mUop.uop.ctrl.expdLen
    iex.uop.expdIdx                 := muxData.mUop.uop.uopIdx
    iex.uop.expdEnd                 := muxData.mUop.uop.uopEnd
    iex.uop.lsrcExpd(0)             := muxData.mUop.uop.lsrcExpd(0)
    iex.uop.lsrcExpd(1)             := muxData.mUop.uop.lsrcExpd(1)
    iex.uop.ldestExpd               := muxData.mUop.uop.ldestExpd
    iex.uop.psrc(0)                 := muxData.mUop.uop.lsrcExpd(0)
    iex.uop.psrc(1)                 := muxData.mUop.uop.lsrcExpd(1)
    iex.uop.psrc(2)                 := muxData.mUop.uop.ldestExpd
    iex.uop.psrc(3)                 := 0.U
    iex.uop.pdest                   := muxData.mUop.uop.ldestExpd
    iex.uop.psrcVal(0)              := muxData.mUop.uop.psrcVal(0)
    iex.uop.psrcVal(1)              := muxData.mUop.uop.psrcVal(1)
    iex.uop.psrcVal(2)              := muxData.mUop.uop.old_pdest
    iex.uop.psrcVal(3)              := vm
    iex.uop.pdestVal                := muxData.mUop.uop.pdestVal

    iex.uop.sysUop.scalarRegWriteEn := muxData.mergeInfo.scalarRegWriteEn
    iex.uop.sysUop.floatRegWriteEn  := muxData.mergeInfo.floatRegWriteEn
    iex.uop.sysUop.rfWriteEn        := muxData.mergeInfo.rfWriteEn
    iex.uop.sysUop.ldest            := muxData.mergeInfo.ldest
    iex.uop.sysUop.permExpdLen      := muxData.mergeInfo.permExpdLen
    iex.uop.sysUop.regDstIdx        := muxData.mergeInfo.regDstIdx
    iex.uop.sysUop.regCount         := muxData.mergeInfo.regCount
    iex.uop.sysUop.robIdx           := 0.U.asTypeOf(new RobPtr)
    iex.vSrc(0)   := muxData.mUop.uopRegInfo.vs1
    iex.vSrc(1)   := muxData.mUop.uopRegInfo.vs2
    iex.rs1   := muxData.mUop.scalar_opnd_1
    iex.vSrc(2) := muxData.mUop.uopRegInfo.old_vd
    iex.vSrc(3)  := muxData.mUop.uopRegInfo.mask
  }


  SVPerm.io.in.vs1_preg_idx    := VecInit(Seq.tabulate(8)(i => muxData.mUop.uop.ctrl.lsrc(0) +
                                  Mux(io.in.bits.mUop.uop.ctrl.vGatherEi16EEW32, i.U >> 1, 
                                  Mux(io.in.bits.mUop.uop.ctrl.vGatherEi16EEW64, i.U >> 2, i.U))))
  SVPerm.io.in.vs2_preg_idx    := VecInit(Seq.tabulate(8)(i => muxData.mUop.uop.ctrl.lsrc(1) + i.U))
  SVPerm.io.in.old_vd_preg_idx := VecInit(Seq.tabulate(8)(i => muxData.mUop.uop.ctrl.ldest   + 
                                  Mux(io.in.bits.mUop.uop.ctrl.vGatherEi16EEW8, i.U >> 1, i.U)))

  io.readSBoardSetIO.setMultiEn := SVPerm.io.in.rvalid
  io.readSBoardSetIO.setNum := Mux(io.in.bits.mUop.uop.ctrl.vGatherEi16EEW64, 2.U, 
                               Mux(io.in.bits.mUop.uop.ctrl.vGatherEi16EEW32, 4.U, 8.U))
  io.readSBoardSetIO.setAddr := muxData.mUop.uop.ctrl.lsrc(0)   

  io.readSBoardSetIO.setMultiEn2 := SVPerm.io.in.rvalid 
  io.readSBoardSetIO.setNum2     := Mux(io.in.bits.mUop.uop.ctrl.vGatherEi16EEW8, 4.U, 8.U)
  io.readSBoardSetIO.setAddr2    := muxData.mUop.uop.ctrl.lsrc(1)                         

  SVPerm.io.in.mask_preg_idx := 0.U
  SVPerm.io.in.uop_valid := muxValid & muxData.mUop.uop.ctrl.perm
  SVPerm.io.in.rdata := io.permRegIn.rdata
  SVPerm.io.in.rvalid := io.permRegIn.rvalid
  SVPerm.io.redirect.valid := false.B
  SVPerm.io.redirect.bits := DontCare

  io.permOut.rd_en  := SVPerm.io.permReadRF.rd_en
  io.permOut.rd_preg_idx := SVPerm.io.permReadRF.rd_preg_idx

  io.readSBoardClearIO.clearEn := SVPerm.io.permReadRF.rd_en
  io.readSBoardClearIO.clearAddr := SVPerm.io.permReadRF.rd_preg_idx
 
  val Result = Wire(new IexOut)

  val CycleVld1H = VecInit(Seq(SValu.io.out.valid, SVMac.io.out.valid, SVMask.io.out.valid, SVReduc.io.out.valid,
                       SVDiv.io.out.valid, SVFpu.io.out.valid, SVPerm.io.out.valid))

  val CycleResult1H = Seq(SValu.io.out.bits, SVMac.io.out.bits, SVMask.io.out.bits, SVReduc.io.out.bits,
                       SVDiv.io.out.bits, SVFpu.io.out.bits, SVPerm.io.out.bits)

  // 默认值，假设没有 valid 信号时的默认结果
  val defaultBits = CycleResult1H.head

  // 初始化 oldestBits 为 defaultBits
  val oldestBits = WireDefault(defaultBits)
  val oldestIdx = Wire(UInt(3.W))

  def selectOlder(bits1: UInt, bits2: UInt): UInt = {
    Mux(bits1(4) === bits2(4), 
      Mux(bits1(3,0) > bits2(3,0), bits1, bits2),  // 当 bits1(4) == bits2(4) 时，选取
      Mux(bits1(3,0) < bits2(3,0), bits1, bits2)   // 当 bits1(4) != bits2(4) 时，比较 bits1(4) 和 bits2(4)
    )
  }

  for(i <- 0 until CycleVld1H.length){
    when(CycleVld1H(i)){
      oldestBits := selectOlder(CycleResult1H(i).commitInfo.bits.sdId, oldestBits.commitInfo.bits.sdId)
      oldestIdx := i.U
    }
  }

  Result := oldestBits
  io.readSBoardReadIO.readAddr1 := Result.toRegFileWrite.rfWriteIdx
  when(io.readSBoardReadIO.readBypassed1){
    Result.commitInfo.valid := false.B
    Result.toRegFileWrite.rfWriteEn := false.B
  }

  //val fixCycleValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid || SVReduc.io.out.valid
  
  io.out.bits := Result
  io.out.valid := CycleVld1H.reduce(_ || _) && !io.readSBoardReadIO.readBypassed1

  io.iexNeedStall := bufferValidReg
}




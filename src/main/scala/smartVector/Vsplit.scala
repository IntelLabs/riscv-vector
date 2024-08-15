package smartVector

import chisel3._
import chisel3.util._
import darecreek.VDecode
import darecreek.exu.vfu.VUopInfo

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.rocket._ 
import xiangshan.MicroOp
import SmartParam._
import darecreek.lsu.LdstDecoder
import darecreek.Vlmul_to_lmul
import darecreek.VInfoAll

class MuopMergeAttr extends Bundle {
    val scalarRegWriteEn = Bool()
    val floatRegWriteEn = Bool()
    val rfWriteEn = Bool()
    val ldest = UInt(5.W)
    val muopEnd = Bool()
    val alu = Bool()
    val mul = Bool()
    val fp = Bool()
    val div = Bool()
    val fixP = Bool()
    val redu = Bool()
    val mask = Bool()
    val perm = Bool()
    val permExpdLen = UInt(4.W)
    val regDstIdx = UInt(5.W)
    val regCount  = UInt(4.W)
    
    //000:8bit
    //001:16bit
    //010:32bit
    //011:64bit
    //111:128bit
    val regBackWidth     = UInt(3.W)
    val regWriteMuopIdx  = UInt(4.W)
}

class VUopCtrlW extends Bundle {
  val funct6      = UInt(6.W)
  val funct3      = UInt(3.W)
  val vm          = Bool()
  val vs1_imm     = UInt(5.W)
  val vs2         = UInt(5.W)
  val widen       = Bool()
  val widen2      = Bool()
  val narrow      = Bool()
  val narrow_to_1 = Bool()
  val load        = Bool()
  val store       = Bool()
  val alu         = Bool() 
  val mul         = Bool()
  val fp          = Bool()
  val div         = Bool()
  val fixP        = Bool()
  val redu        = Bool()
  val mask        = Bool()
  val perm        = Bool()
  val floatRed    = Bool()
  val vGatherEi16EEW8  = Bool()
  val vGatherEi16EEW16 = Bool()
  val vGatherEi16EEW32 = Bool()
  val vGatherEi16EEW64 = Bool()
  val lsrc = Vec(2, UInt(5.W)) //0: vs1/imm5   1: vs2
  val ldest = UInt(5.W)
  def vv = !funct3(2) && !(funct3(1) && funct3(0))
  def vx = funct3(2)
  def vi = !funct3(2) && funct3(1) && funct3(0)
  def isLdst = load || store
} 

class VUop(implicit p: Parameters) extends Bundle {
  val ctrl      = new VUopCtrlW
  val info      = new VUopInfo
  val uopIdx    = UInt(7.W) // max 16 elements * 8 register for segment 0~127
  val segIndex  = UInt(3.W)
  val uopEnd    = Bool()
  // Temp: system uop
  val sysUop    = new MicroOp
}

class UopRegInfo(implicit p : Parameters) extends Bundle {
    val vs1           = UInt(128.W)
    val vs2           = UInt(128.W)
    val mask          = UInt(128.W)
    val old_vd        = UInt(128.W)
    val vxsat         = Bool()
}

class Muop(implicit p : Parameters) extends Bundle {
    val uop           = new VUop
    val scalar_opnd_1 = UInt(64.W)
    val scalar_opnd_2 = UInt(64.W)
    val uopRegInfo    = new UopRegInfo
}

class ExcpInfo extends Bundle {
    val exception_vld   = Bool()
    val update_vl       = Bool()
    val update_data     = UInt(bVL.W)
    val xcpt_cause      = new HellaCacheExceptions()
    val xcpt_addr       = UInt(addrWidth.W)
    val illegalInst     = Bool()
    val update_float    = Bool()
    val reg_idx         = UInt(5.W)
}

class Vsplit(implicit p : Parameters) extends Module {
    val io = IO(new Bundle{
        val in = new Bundle{
            val decodeIn  = Flipped(Decoupled(new VDecodeOutput))
            val regFileIn = Input(new regOut)
        }
        val out = new Bundle{
            val mUop          = ValidIO(new Muop)
            val toRegFileRead = Output(new regReadIn)
            val mUopMergeAttr = ValidIO(new MuopMergeAttr)
        }
        val scoreBoardSetIO = Flipped(new ScoreboardSetIO)
        val scoreBoardReadIO = Flipped(new ScoreboardReadIO)
        val lsuStallSplit = Input(Bool())
        val lsuEmpty      = Input(Bool()) 
        val iexNeedStall  = Input(Bool())
        val vLSUXcpt = Input (new VLSUXcpt)
        val excpInfo = Output(new ExcpInfo)
    })

    val expdWidth     = 8 // 1~128
    
    //val vCtrl         = RegInit(VecInit(Seq.fill(1)(0.U.asTypeOf(new darecreek.VCtrl))))
    //val vInfo         = Reg(Vec(1, new VInfo))
    //val scalar_opnd_1 = Reg(Vec(1, UInt(64.W)))
    //val scalar_opnd_2 = Reg(Vec(1, UInt(64.W)))
    //val float_opnd_1  = Reg(Vec(1, UInt(64.W)))
    val uopRegInfo    = Reg(Vec(1, new UopRegInfo))
    //val eewEmulInfo   = Reg(Vec(1, new VInfoAll))
    val idx           = RegInit(UInt(expdWidth.W), 0.U)
    //val floatRedReg   = Reg(Vec(1, Bool()))

    val empty :: ongoing :: Nil = Enum(2)
    val currentState = RegInit(empty)
    val currentStateNext = WireDefault(empty) 

    val instDecodeIn = io.in.decodeIn.valid && currentState === empty
    val fire2Exu = Wire(Bool())
    val fire2PipeReg = Wire(Bool())

    val pipeRegReady    = WireInit(false.B)
    //val exuReady        = ~io.lsuStallSplit && ~io.iexNeedStall
    val exuReady        = io.lsuEmpty && ~io.iexNeedStall

    //when (instFirstIn){       
    //    vCtrl(0)            := io.in.decodeIn.bits.vCtrl
    //    vInfo(0)            := io.in.decodeIn.bits.vInfo
    //    scalar_opnd_1(0)    := io.in.decodeIn.bits.scalar_opnd_1
    //    scalar_opnd_2(0)    := io.in.decodeIn.bits.scalar_opnd_2
    //    float_opnd_1(0)     := io.in.decodeIn.bits.float_opnd_1
    //    eewEmulInfo(0)      := io.in.decodeIn.bits.eewEmulInfo
    //    uopRegInfo(0).vxsat := false.B
    //    floatRedReg(0)      := io.in.decodeIn.bits.floatRed
    //}

    //To save power, when do not need to update the vs1, keep it unchanged. 
    //ALU will judge whether use the data, do not worry to send the wrong data 
    uopRegInfo(0).vs1       := Mux(io.in.regFileIn.readVld(0), io.in.regFileIn.readData(0), uopRegInfo(0).vs1)
    uopRegInfo(0).vs2       := Mux(io.in.regFileIn.readVld(1), io.in.regFileIn.readData(1), uopRegInfo(0).vs2)
    uopRegInfo(0).mask      := Mux(io.in.regFileIn.readVld(2), io.in.regFileIn.readData(2), uopRegInfo(0).mask)
    uopRegInfo(0).old_vd    := Mux(io.in.regFileIn.readVld(3), io.in.regFileIn.readData(3), uopRegInfo(0).old_vd)

    //val ctrl = Mux(instFirstIn, io.in.decodeIn.bits.vCtrl, vCtrl(0))
    //val info = Mux(instFirstIn, io.in.decodeIn.bits.vInfo, vInfo(0))
    //val scalarOpnd1  = Mux(instFirstIn, io.in.decodeIn.bits.scalar_opnd_1, scalar_opnd_1(0))
    //val scalarOpnd2  = Mux(instFirstIn, io.in.decodeIn.bits.scalar_opnd_2, scalar_opnd_2(0))
    //val floatOpnd1   = Mux(instFirstIn, io.in.decodeIn.bits.float_opnd_1 , float_opnd_1(0))
    //val floatRed     = Mux(instFirstIn, io.in.decodeIn.bits.floatRed     , floatRedReg(0))
    //val eewEmulInfo1 = Mux(instFirstIn, io.in.decodeIn.bits.eewEmulInfo  , eewEmulInfo(0))

    val ctrl         = io.in.decodeIn.bits.vCtrl
    val info         = io.in.decodeIn.bits.vInfo
    val scalarOpnd1  = io.in.decodeIn.bits.scalar_opnd_1
    val scalarOpnd2  = io.in.decodeIn.bits.scalar_opnd_2
    val floatOpnd1   = io.in.decodeIn.bits.float_opnd_1 
    val floatRed     = io.in.decodeIn.bits.floatRed     
    val eewEmulInfo1 = io.in.decodeIn.bits.eewEmulInfo  

    //Because the register file do not always read the register file when instFirstIn
    val vs1     = Mux(io.in.regFileIn.readVld(0), io.in.regFileIn.readData(0), uopRegInfo(0).vs1)
    val vs2     = Mux(io.in.regFileIn.readVld(1), io.in.regFileIn.readData(1), uopRegInfo(0).vs2)
    val mask    = Mux(io.in.regFileIn.readVld(2), io.in.regFileIn.readData(2), uopRegInfo(0).mask)
    val old_vd  = Mux(io.in.regFileIn.readVld(3), io.in.regFileIn.readData(3), uopRegInfo(0).old_vd)
    val v_ext_out = ctrl.alu && ctrl.funct3 === "b010".U && ctrl.funct6 === "b010010".U 
  
    val isfloat = !ctrl.isLdst && (ctrl.funct3 === "b101".U || ctrl.funct3 === "b001".U)
    val vGatherEi16 = ctrl.perm && ctrl.funct6 === "b001110".U && ctrl.funct3 === "b000".U
    val vGatherEi16EEW8  = vGatherEi16 && eewEmulInfo1.veewVd === "b000".U
    val vGatherEi16EEW16 = vGatherEi16 && eewEmulInfo1.veewVd === "b001".U
    val vGatherEi16EEW32 = vGatherEi16 && eewEmulInfo1.veewVd === "b010".U
    val vGatherEi16EEW64 = vGatherEi16 && eewEmulInfo1.veewVd === "b011".U
    val scalar_float_opnd_1 = Mux(isfloat, floatOpnd1, scalarOpnd1)
    val ldst = ctrl.load || ctrl.store
    val ldstCtrl = LdstDecoder(ctrl.funct6, ctrl.lsrc(1))
    val nfield = ctrl.funct6(5, 3) +& 1.U
    val vlmul = info.vlmul
    val lmul = Vlmul_to_lmul(vlmul)
    val indexIncBase = Wire(UInt(3.W)) 
    val emulVd  = Mux(ctrl.ldestVal || ctrl.store, eewEmulInfo1.emulVd,  0.U(4.W))
    val emulVs1 = Mux(ctrl.lsrcVal(0), eewEmulInfo1.emulVs1, 0.U(4.W))
    val emulVs2 = Mux(ctrl.lsrcVal(1), eewEmulInfo1.emulVs2, 0.U(4.W))
    val idxVdInc = Wire(Bool())
    val idxVs2Inc = Wire(Bool())

    val expdLenReg  =  Reg(UInt(expdWidth.W))
    val expdLenSeg  = Wire(UInt(expdWidth.W))
    val expdLenIdx  = Wire(UInt(expdWidth.W))
    val expdLenLdSt = Wire(UInt(expdWidth.W))
    val expdLenIn   = Wire(UInt(expdWidth.W))
    val expdLen     = Wire(UInt(expdWidth.W))

    //Due to the design of vmask, these instructions need to be split into lmul, 
    //but the same data must be sent each time
    val vcpop    = ctrl.mask && ctrl.funct6 === "b010000".U && ctrl.lsrc(0) === "b10000".U
    val viota    = ctrl.mask && ctrl.funct6 === "b010100".U && ctrl.lsrc(0) === "b10000".U 
    val vid      = ctrl.mask && ctrl.funct6 === "b010100".U && ctrl.lsrc(0) === "b10001".U
    val vmaskExcp = vcpop || viota || vid


    val segIndex = idx % nfield
    val uopIdx   = Mux(ldst && ldstCtrl.segment, (idx - segIndex)/nfield, idx)
    // for segment ldst insts

    val log2DestElen = MuxLookup(eewEmulInfo1.veewVd, 4.U, Seq(
        "b000".U -> 4.U, // VLEN has 16 elements
        "b001".U -> 3.U, // 8
        "b010".U -> 2.U, // 4
        "b011".U -> 1.U, // 2
    ))

    val log2SrcElen = MuxLookup(eewEmulInfo1.veewVs2, 4.U, Seq(
        "b000".U -> 4.U, // VLEN has 16 elements
        "b001".U -> 3.U, // 8
        "b010".U -> 2.U, // 4
        "b011".U -> 1.U, // 2
    ))

    val log2EmulVd = MuxLookup(emulVd, 1.U, Seq(
        1.U -> 0.U,
        2.U -> 1.U,
        4.U -> 2.U,
        8.U -> 3.U,
    ))

    val segmentRegNotFirstElem = uopIdx - (uopIdx >> log2DestElen << log2DestElen) =/= 0.U
    // for non-segment indexed ldst insts
    idxVdInc  := emulVs2 >= emulVd
    idxVs2Inc := emulVs2 < emulVd

    when (emulVs2 / emulVd === 8.U || emulVd / emulVs2 === 8.U){
        indexIncBase := 3.U
    }.elsewhen(emulVs2 / emulVd === 4.U || emulVd / emulVs2 === 4.U){
        indexIncBase := 2.U
    }.elsewhen(emulVs2 / emulVd === 2.U || emulVd / emulVs2 === 2.U){
        indexIncBase := 1.U
    }.otherwise{
        indexIncBase := 0.U
    }


    // * BEGIN
    // * register inc

    val lsrc0_inc =             //vcompress
          Mux(ctrl.redu || floatRed || (ctrl.funct6 === "b010111".U && ctrl.funct3 === 2.U) || vmaskExcp, 0.U, 
          Mux(ctrl.widen || ctrl.widen2 || ctrl.narrow, idx >> 1, idx))
              
    val lsrc1_inc = Wire(UInt(3.W))

    when(ldst && ldstCtrl.segment && ldstCtrl.indexed) { // segment indexed ldst insts
      lsrc1_inc := (idx / nfield) >> log2SrcElen
    }.elsewhen (ldst && ldstCtrl.indexed){              // non-segment indexed ldst insts
      lsrc1_inc := Mux(idxVs2Inc, (idx >> indexIncBase) % emulVs2, idx % emulVs2)
    }.elsewhen(ctrl.widen && !ctrl.redu && !floatRed || v_ext_out && ctrl.lsrc(0)(2,1) === 3.U) {
      lsrc1_inc := idx >> 1
    }.elsewhen (v_ext_out && ctrl.lsrc(0)(2,1) === 2.U) {
      lsrc1_inc := idx >> 2
    }.elsewhen (v_ext_out && ctrl.lsrc(0)(2,1) === 1.U) {
      lsrc1_inc := idx >> 3
    }.elsewhen (ctrl.funct6 === "b010100".U || vmaskExcp) { //VMUNARY0
      lsrc1_inc := 0.U
    }.otherwise {
      lsrc1_inc := idx
    }

    val ldest_inc = Wire(UInt(4.W))

    when (ldst && ldstCtrl.segment) {
      ldest_inc := (segIndex << log2EmulVd) + (uopIdx >> log2DestElen)
    }.elsewhen (ldst && ldstCtrl.indexed){
      ldest_inc := Mux(idxVdInc, idx >> indexIncBase, idx)
    }.elsewhen(ctrl.narrow) {
      ldest_inc := idx >> 1
    }.elsewhen (ctrl.redu  || floatRed || ctrl.narrow_to_1 || vcpop) {
      ldest_inc := 0.U
    }.otherwise {
      ldest_inc := idx
    }

    // * register inc
    // * END


    // * BEGIN
    // * organize muop merge attr
    //ToDo: redu, widen2,narrow_to_1 need to be add
    val mUopIn = Wire(ValidIO(new Muop))
    val mUopMergeAttrIn = Wire(ValidIO(new MuopMergeAttr))

    val regBackWidth = UInt(3.W)
    when(ctrl.narrow) {
        mUopMergeAttrIn.bits.regBackWidth := "b11".U
        mUopMergeAttrIn.bits.regWriteMuopIdx  := idx(0)
    }.otherwise{
        mUopMergeAttrIn.bits.regBackWidth := "b111".U
        mUopMergeAttrIn.bits.regWriteMuopIdx  := 0.U       
    }
    
    mUopMergeAttrIn.valid                 := mUopIn.valid
    mUopMergeAttrIn.bits.scalarRegWriteEn := ctrl.rdVal && !isfloat  
    mUopMergeAttrIn.bits.floatRegWriteEn  := ctrl.rdVal && isfloat
    mUopMergeAttrIn.bits.ldest            := ctrl.ldest + ldest_inc
    mUopMergeAttrIn.bits.rfWriteEn        := ctrl.ldestVal
    mUopMergeAttrIn.bits.muopEnd          := mUopIn.bits.uop.uopEnd
    mUopMergeAttrIn.bits.alu              := ctrl.alu
    mUopMergeAttrIn.bits.mul              := ctrl.mul 
    mUopMergeAttrIn.bits.fp               := ctrl.fp  
    mUopMergeAttrIn.bits.div              := ctrl.div 
    mUopMergeAttrIn.bits.fixP             := ctrl.fixP
    mUopMergeAttrIn.bits.redu             := ctrl.redu
    mUopMergeAttrIn.bits.mask             := ctrl.mask
    mUopMergeAttrIn.bits.perm             := ctrl.perm
    //Just for perm instruction
    mUopMergeAttrIn.bits.permExpdLen      := lmul
    mUopMergeAttrIn.bits.regDstIdx        := ctrl.ldest
    mUopMergeAttrIn.bits.regCount         := Mux(info.vl === 0.U, 1.U, nfield << log2EmulVd)

    // * organize muop merge attr
    // * END
    

    // * BEGIN
    // * read & write scoreboard
    val vs1ReadEn  = ctrl.lsrcVal(0)
    val vs2ReadEn  = ctrl.lsrcVal(1)
    
    //in one inst, different uop may has same ldest, when the first one set the scoreboard, the second one 
    // should not be stalled
    val ldest_inc_last = RegInit(15.U(4.W))
    val sameLdest = Wire(Bool())
    when(ldstCtrl.segment && segmentRegNotFirstElem) {
        sameLdest := true.B
    }.elsewhen(ldest_inc === ldest_inc_last){
        sameLdest := true.B
    }.otherwise{
        sameLdest := false.B
    }

    when(~mUopIn.bits.uop.uopEnd & fire2PipeReg){ 
        ldest_inc_last := ldest_inc
    }
    when(mUopIn.bits.uop.uopEnd & fire2PipeReg){
        ldest_inc_last := 15.U
    }

    val vs3ReadEn  = (ctrl.store || ctrl.ldestVal) & ~sameLdest
    val maskReadEn = ~ctrl.vm
    val vs1Idx     = ctrl.lsrc(0) + lsrc0_inc
    val vs2Idx     = ctrl.lsrc(1) + lsrc1_inc
    val vs3Idx     = ctrl.ldest   + ldest_inc
    //val needStall  = Wire(Bool())
    val hasRegConf = Wire(Vec(4,Bool()))
    
    io.scoreBoardReadIO.readAddr1    := vs1Idx
    io.scoreBoardReadIO.readAddr2    := vs2Idx
    io.scoreBoardReadIO.readAddr3    := vs3Idx
    io.scoreBoardReadIO.readMaskAddr := 0.U
    io.scoreBoardReadIO.readNum1     := emulVs1
    io.scoreBoardReadIO.readNum2     := emulVs2 

    when(!vs1ReadEn || vs1Idx === ctrl.ldest + ldest_inc){
        hasRegConf(0) := false.B
    }.elsewhen(ctrl.perm){
        hasRegConf(0) := io.scoreBoardReadIO.readBypassed1N
    }.elsewhen (~io.scoreBoardReadIO.readBypassed1){
        hasRegConf(0) := false.B
    }.otherwise{
        hasRegConf(0) := true.B
    }

    when(!vs2ReadEn){
        hasRegConf(1) := false.B
    }.elsewhen(ctrl.perm){
        hasRegConf(1) := io.scoreBoardReadIO.readBypassed2N
    }.elsewhen (~io.scoreBoardReadIO.readBypassed2){
        hasRegConf(1) := false.B
    }.otherwise{
        hasRegConf(1) := true.B
    }

    when(!vs3ReadEn){
        hasRegConf(2) := false.B
    }.elsewhen(ctrl.perm){
        hasRegConf(2) := io.scoreBoardReadIO.readBypassed3N
    }.elsewhen (~io.scoreBoardReadIO.readBypassed3){
        hasRegConf(2) := false.B
    }.otherwise{
        hasRegConf(2) := true.B
    }

    when(!maskReadEn){
        hasRegConf(3) := false.B
    }.elsewhen (~io.scoreBoardReadIO.readBypassed4){
        hasRegConf(3) := false.B
    }.otherwise{
        hasRegConf(3) := true.B
    }

    //narrowTo1NoStall need to deal with
    //val narrowTo1NoStall = ctrl.narrow_to_1 
    val regConf = hasRegConf(0) || hasRegConf(1) || hasRegConf(2) || hasRegConf(3) 

    io.out.toRegFileRead.rfReadEn(0)      := fire2PipeReg && ctrl.lsrcVal(0)
    io.out.toRegFileRead.rfReadEn(1)      := fire2PipeReg && ctrl.lsrcVal(1)
    io.out.toRegFileRead.rfReadEn(2)      := fire2PipeReg && ~ctrl.vm
    io.out.toRegFileRead.rfReadEn(3)      := fire2PipeReg && (ctrl.ldestVal || ctrl.store)
    io.out.toRegFileRead.rfReadIdx(0)     := ctrl.lsrc(0) + lsrc0_inc
    io.out.toRegFileRead.rfReadIdx(1)     := ctrl.lsrc(1) + lsrc1_inc
    io.out.toRegFileRead.rfReadIdx(2)     := 0.U
    io.out.toRegFileRead.rfReadIdx(3)     := ctrl.ldest + ldest_inc

    io.scoreBoardSetIO.setEn      := RegNext(fire2PipeReg && ctrl.ldestVal && (~ctrl.perm || ~(ldstCtrl.segment && segmentRegNotFirstElem)))
    io.scoreBoardSetIO.setMultiEn := RegNext(fire2PipeReg && ctrl.ldestVal && ctrl.perm)
    io.scoreBoardSetIO.setNum     := RegNext(emulVd)
    io.scoreBoardSetIO.setAddr    := RegNext(mUopMergeAttrIn.bits.ldest)

    // * read & write scoreboard
    // * END

    val hasExcp = ctrl.illegal || io.vLSUXcpt.exception_vld || io.vLSUXcpt.update_vl    

    mUopIn.bits.uop.uopIdx   := uopIdx
    mUopIn.bits.uop.segIndex := segIndex
    mUopIn.bits.uop.uopEnd   := (idx + 1.U === expdLen)

    mUopIn.bits.uop.ctrl.funct6      := ctrl.funct6
    mUopIn.bits.uop.ctrl.funct3      := ctrl.funct3
    mUopIn.bits.uop.ctrl.vm          := ctrl.vm
    mUopIn.bits.uop.ctrl.vs1_imm     := ctrl.lsrc(0)
    mUopIn.bits.uop.ctrl.vs2         := ctrl.lsrc(1)  
    mUopIn.bits.uop.ctrl.widen       := ctrl.widen
    mUopIn.bits.uop.ctrl.widen2      := ctrl.widen2
    mUopIn.bits.uop.ctrl.narrow      := ctrl.narrow
    mUopIn.bits.uop.ctrl.narrow_to_1 := ctrl.narrow_to_1
    mUopIn.bits.uop.ctrl.load        := ctrl.load
    mUopIn.bits.uop.ctrl.store       := ctrl.store
    mUopIn.bits.uop.ctrl.alu         := ctrl.alu  
    mUopIn.bits.uop.ctrl.mul         := ctrl.mul 
    mUopIn.bits.uop.ctrl.fp          := ctrl.fp  
    mUopIn.bits.uop.ctrl.div         := ctrl.div 
    mUopIn.bits.uop.ctrl.fixP        := ctrl.fixP
    mUopIn.bits.uop.ctrl.redu        := ctrl.redu
    mUopIn.bits.uop.ctrl.mask        := ctrl.mask
    mUopIn.bits.uop.ctrl.perm        := ctrl.perm
    mUopIn.bits.uop.ctrl.lsrc        := ctrl.lsrc
    mUopIn.bits.uop.ctrl.ldest       := ctrl.ldest
    mUopIn.bits.uop.ctrl.floatRed    := floatRed
    mUopIn.bits.uop.ctrl.vGatherEi16EEW8  := vGatherEi16EEW8
    mUopIn.bits.uop.ctrl.vGatherEi16EEW16 := vGatherEi16EEW16
    mUopIn.bits.uop.ctrl.vGatherEi16EEW32 := vGatherEi16EEW32
    mUopIn.bits.uop.ctrl.vGatherEi16EEW64 := vGatherEi16EEW64

    mUopIn.bits.uop.info.ma          := info.vma
    mUopIn.bits.uop.info.ta          := info.vta
    mUopIn.bits.uop.info.vl          := info.vl
    mUopIn.bits.uop.info.vstart      := info.vstart
    mUopIn.bits.uop.info.vsew        := info.vsew
    mUopIn.bits.uop.info.vlmul       := info.vlmul
    mUopIn.bits.uop.info.vxrm        := info.vxrm
    mUopIn.bits.uop.info.frm         := info.frm
    mUopIn.bits.uop.sysUop           := 0.U.asTypeOf(new MicroOp)

    mUopIn.bits.scalar_opnd_1        := scalar_float_opnd_1
    mUopIn.bits.scalar_opnd_2        := scalarOpnd2

    mUopIn.bits.uopRegInfo.vxsat     := false.B          
    mUopIn.bits.uopRegInfo.vs1       := vs1
    mUopIn.bits.uopRegInfo.vs2       := vs2
    mUopIn.bits.uopRegInfo.old_vd    := old_vd
    mUopIn.bits.uopRegInfo.mask      := mask


    when((instDecodeIn || currentState === ongoing) & ~regConf & ~hasExcp){
        mUopIn.valid := true.B   
    }.otherwise{
        mUopIn.valid := false.B
    }

    // * BEGIN
    // * Split FSM
    val ldStEmulVd  = eewEmulInfo1.emulVd
    val ldStEmulVs2 = eewEmulInfo1.emulVs2
  
    expdLenSeg  := Mux(1.U >= info.vl * nfield, 1.U, info.vl * nfield)
    expdLenIdx  := Mux(ldStEmulVd >= ldStEmulVs2, ldStEmulVd, ldStEmulVs2)
    expdLenLdSt := Mux(ldst && ldstCtrl.segment, expdLenSeg,
                        Mux(ldstCtrl.wholeReg, eewEmulInfo1.emulVd,
                        Mux(ldst && ldstCtrl.indexed, expdLenIdx, ldStEmulVd)))
    
    val maxOfVs12Vd = Mux(emulVd >= emulVs1, Mux(emulVd >= emulVs2, emulVd, emulVs2), Mux(emulVs1 >= emulVs2, emulVs1, emulVs2))
    val vmv_vfmv    = ctrl.alu && !ctrl.opi && ctrl.funct6 === "b010000".U

    //val expdLenIn = Mux(ldst, expdLenLdSt, Mux(ctrl.perm || vmv_vfmv, 1.U , maxOfVs12Vd))
    expdLenIn := Mux(ldst, expdLenLdSt,
                    Mux(ctrl.perm || vmv_vfmv, 1.U, (Mux(vcpop || viota || vid, lmul, maxOfVs12Vd))))
    
    when(instDecodeIn){
        expdLenReg := expdLenIn
    }
    expdLen := Mux(instDecodeIn, expdLenIn, expdLenReg)

    switch(currentState){
        is(empty){
            when(hasExcp){
                currentStateNext := empty
            }.elsewhen(instDecodeIn && (regConf || ~pipeRegReady)){
                currentStateNext := ongoing
            }.elsewhen(instDecodeIn && expdLen === 1.U){
                currentStateNext := empty            
            }.elsewhen(instDecodeIn && expdLen =/= 1.U){
                currentStateNext := ongoing
            }.otherwise{
                currentStateNext := empty
            }
        }
        is(ongoing){
            when(hasExcp){
                currentStateNext := empty
            }.elsewhen(regConf || ~pipeRegReady){
                currentStateNext := ongoing
            }.elsewhen(((idx + 1.U) === expdLen)){
                currentStateNext := empty
            }.elsewhen((idx + 1.U) < expdLen){
                currentStateNext := ongoing
            }
        }
    }

    currentState := currentStateNext

    when(hasExcp || currentStateNext === empty) {
        idx := 0.U
    }.elsewhen((instDecodeIn || currentState === ongoing) & ~regConf & pipeRegReady & ~hasExcp){
        idx := idx + 1.U    
    }

    io.in.decodeIn.ready := currentStateNext === empty

    // * Split FSM
    // * END
    

    // * BEGIN
    // * Pipeline Register
    val validReg        = RegInit(false.B)
    val bitsReg         = RegInit(0.U.asTypeOf(new Muop))
    val mergeAttrReg    = RegInit(0.U.asTypeOf(new MuopMergeAttr))

    pipeRegReady        := exuReady || (!validReg)
    fire2PipeReg        := pipeRegReady && mUopIn.valid
    fire2Exu            := exuReady && validReg

    // when exu accepts the current muop or when there is no uop accept new muop
    when (pipeRegReady) {
        validReg := mUopIn.valid
        bitsReg := mUopIn.bits
        mergeAttrReg := mUopMergeAttrIn.bits
    }

    when (io.vLSUXcpt.exception_vld || io.vLSUXcpt.update_vl){
        validReg := false.B
    }

    // * Pipeline Register
    // * END


    // * BEGIN
    // * Output mUop to EXU
    val muopOutValid             = fire2Exu && ~(io.vLSUXcpt.exception_vld || io.vLSUXcpt.update_vl)
    io.out.mUop.valid           := muopOutValid
    io.out.mUopMergeAttr.valid  := muopOutValid
    io.out.mUop.bits            := bitsReg
    io.out.mUopMergeAttr.bits   := mergeAttrReg
    // * Output mUop to EXU
    // * END


    // * BEGIN
    // * Output ExcpInfo
    io.excpInfo.exception_vld := io.vLSUXcpt.exception_vld || RegNext((ctrl.illegal & instDecodeIn))
    io.excpInfo.illegalInst   := RegNext(ctrl.illegal)
    io.excpInfo.update_float  := ctrl.rdVal && isfloat
    io.excpInfo.reg_idx       := ctrl.ldest
    io.excpInfo.update_vl     := io.vLSUXcpt.update_vl
    io.excpInfo.update_data   := io.vLSUXcpt.update_data
    io.excpInfo.xcpt_addr     := io.vLSUXcpt.xcpt_addr
    io.excpInfo.xcpt_cause    := io.vLSUXcpt.xcpt_cause
    // * Output ExcpInfo
    // * END

}


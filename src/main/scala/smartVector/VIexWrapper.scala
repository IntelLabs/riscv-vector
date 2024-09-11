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
import matrix.AccTileConstants.{maxNumMatTiles, mxuMeshCols, mxuMeshRows, mxuPECols, mxuPERows, mxuTileCols, mxuTileRows, numAccTiles, numReadPorts, numVLdPorts, rLenbSz, usingInnerProd}
import matrix._
import matrix.MatrixParameters._

class IexOutput extends Bundle {
  val vd = UInt(128.W)
  val vxsat = Bool()
  val fflags = UInt(5.W)
}

class VPermRegIn extends Bundle {
  val rdata = UInt(128.W)
  val rvalid = Bool()
}

class VIexWrapper(implicit p: Parameters) extends Module {

  val io = IO(new Bundle {
    val in = Input(ValidIO(new Muop))
    val out = ValidIO(new IexOutput)

    val rowWriteReq = if (hasMatrix) Some(Input(Vec(numVLdPorts, Valid(new SliceCtrls())))) else None
    val rowWriteData = if (hasMatrix) Some(Input(Vec(numVLdPorts, UInt((mxuPECols * 32).W)))) else None
    val rowWriteMask = if (hasMatrix) Some(Input(Vec(numVLdPorts, UInt((mxuPECols).W)))) else None
    val rowWriteByteMask = if (hasMatrix) Some(Input(Vec(numVLdPorts, UInt((mxuPECols).W)))) else None
    val matrix_in = if (hasMatrix) Some(Input(Valid(new mma_in()))) else None
    val acc_out = if (hasMatrix) Some(Output(Valid(UInt((mxuPERows * mxuPECols * 32).W)))) else None

    val permOut = new(VPermOutput)
    val permRegIn = Input(new(VPermRegIn))
    val iexNeedStall = Output(Bool())
  })

  // IEX modules
  val SValu = Module(new VAluWrapper()(p))
  val SVMac = Module(new VMacWrapper()(p))
  val SVMask = Module(new VMaskWrapper()(p))
  val SVReduc = Module(new VReducWrapper()(p))
  val SVDiv = Module(new VDivWrapper()(p))
  val SVPerm = Module(new VPermWrapper()(p))
  val SVFpu = Module(new VSFPUWrapper()(p))

  val empty :: ongoing :: Nil = Enum(2)
  val currentState = RegInit(empty)
  val currentStateNext = WireDefault(empty)

  // IEX input source
  val mUop = io.in.bits
  val mUopValid = io.in.valid && ~io.in.bits.uop.ctrl.isLdst

  // val divNotReady  = ~SVDiv.io.in.ready
  // val fpuNotReady  = ~SVFpu.io.in.ready
  // val permNotReady = SVPerm.io.out.perm_busy
  // val ready    = ~(divNotReady || fpuNotReady || permNotReady)
  if (hasMatrix) {
    val mma = Module(new Mesh()(p))
    io.acc_out.get := mma.io.accout(0)
    for (c <- 0 until mxuMeshCols) {
      mma.io.macReq(c).valid := io.matrix_in.get.valid
      mma.io.macReq(c).bits.src1Ridx := 0.U
      mma.io.macReq(c).bits.src2Ridx := 0.U
      mma.io.macReq(c).bits.dstRidx := 0.U
      mma.io.macReq(c).bits.srcType := io.matrix_in.get.bits.srcType
      mma.io.macReq(c).bits.outType := io.matrix_in.get.bits.dstType
      mma.io.macReq(c).bits.aluType := 0.U
      mma.io.macReq(c).bits.macInit := false.B
      mma.io.macReq(c).bits.macLast := false.B
      mma.io.macReq(c).bits.autoClr := false.B
      mma.io.macReq(c).bits.autoCvt := false.B
      //    mma.io.macReq(c).bits.prodLen := 0.U
      mma.io.macReq(c).bits.dirCal := 0.U
      mma.io.macReq(c).bits.rm := 0.U
      mma.io.macReqSrcB(c) := io.matrix_in.get.bits.srcB
      mma.io.macReqSrcC(c) := 0.U
    }

    for (c <- 0 until mxuMeshRows) {
      mma.io.macReqSrcA(c) := io.matrix_in.get.bits.srcA
      mma.io.macReqSrcD(c) := 0.U
    }

    mma.io.clrReq.valid := false.B
    mma.io.clrReq.bits.ridx := 0.U

    for (c <- 0 until numReadPorts) {
      mma.io.rowReadReq(c).valid := false.B
      mma.io.colReadReq(c).valid := false.B
      mma.io.rowReadReq(c).bits.ridx := 0.U
      mma.io.rowReadReq(c).bits.sidx := 0.U
      mma.io.rowReadReq(c).bits.sew := 0.U
      mma.io.colReadReq(c).bits.ridx := 0.U
      mma.io.colReadReq(c).bits.sidx := 0.U
      mma.io.colReadReq(c).bits.sew := 0.U
    }

    for (c <- 0 until numVLdPorts) {
      mma.io.rowWriteReq(c) := io.rowWriteReq.get(c)
      mma.io.rowWriteMask(c) := io.rowWriteMask.get(c)
      mma.io.rowWriteByteMask(c) := io.rowWriteByteMask.get(c)
      mma.io.rowWriteData(c) := io.rowWriteData.get(c)
      mma.io.colWriteReq(c).valid := false.B
      mma.io.colWriteReq(c).bits.ridx := 0.U
      mma.io.colWriteReq(c).bits.sidx := 0.U
      mma.io.colWriteReq(c).bits.sew := 0.U
      mma.io.colWriteData(c) := 0.U
      mma.io.colWriteMask(c) := 0.U
    }
  }

  val outValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid ||
    SVReduc.io.out.valid || SVDiv.io.out.valid || SVFpu.io.out.valid
  val permDone = Wire(Bool())
  val permWriteNum = RegInit(0.U(4.W))
  when(SVPerm.io.out.wb_vld) {
    permWriteNum := permWriteNum + 1.U
  }

  when(SVPerm.io.out.wb_vld && (permWriteNum + 1.U === Vlmul_to_lmul(SVPerm.io.out.uop.info.vlmul))) {
    permWriteNum := 0.U
    permDone := true.B
  }.otherwise {
    permDone := false.B
  }

  val oneCycleLatIn = mUopValid & (mUop.uop.ctrl.alu || mUop.uop.ctrl.mask)
  val twoCycleLatIn = mUopValid & (mUop.uop.ctrl.mul || mUop.uop.ctrl.redu)
  val noFixLatIn = mUopValid & (mUop.uop.ctrl.div || mUop.uop.ctrl.perm || mUop.uop.ctrl.fp)
  val twoCycleReg = RegEnable(twoCycleLatIn, mUopValid)
  val fixLatVld = SVDiv.io.out.valid || permDone || SVFpu.io.out.valid

  switch(currentState) {
    is(empty) {
      when(mUopValid && !io.matrix_in.get.valid && ~mUop.uop.ctrl.alu && ~mUop.uop.ctrl.isLdst && ~mUop.uop.ctrl.mask && ~(mUop.uop.ctrl.narrow_to_1 && ~mUop.uop.uopEnd)) {
        currentStateNext := ongoing
      }.otherwise {
        currentStateNext := empty
      }
    }
    is(ongoing) {
      //when(twoCycleReg || fixLatVld){
      //when(twoCycleReg || fixLatVld || mUop.uop.ctrl.floatRed && SVFpu.io.in.ready && ~mUop.uop.uopEnd){
      when(twoCycleReg || fixLatVld || (mUop.uop.ctrl.floatRed && SVFpu.io.in.ready && ~(mUop.uop.uopIdx === 0.U))) {
        currentStateNext := empty
      }.otherwise {
        currentStateNext := ongoing
      }
    }
  }

  currentState := currentStateNext
  //io.iexNeedStall := (currentStateNext === ongoing) || ~ready
  //val ready = currentState === empty || (mUop.uop.ctrl.floatRed && SVFpu.io.in.ready && ~(mUop.uop.uopIdx === 0.U))
  io.iexNeedStall := (currentState === ongoing)

  //if is floatRed, when is ready, the next uop valid will be high in same cycle.
  //and the first's ready match the second's valid, it will cause second's ready invalid
  //io.iexNeedStall := Mux(bitsReg.uop.ctrl.floatRed && !iexNeedStallTmp , RegNext(iexNeedStallTmp) , iexNeedStallTmp)
  //assert(!(currentState === ongoing && validFinal), "when current state is ongoing, should not has new inst in")
  //assert(!(!SVDiv.io.in.ready && validFinal), "when div is not ready, should not has new inst in")
  //assert(!(SVPerm.io.out.perm_busy && validFinal), "when perm is busy, should not has new inst in")

  // val fpFire = RegInit(false.B) 
  // val divFire = RegInit(false.B)
  // val perFire = RegInit(false.B)

  // when(mUopValid && mUop.uop.ctrl.fp && SVFpu.io.in.ready){
  //   fpFire := true.B
  // }

  // when(mUopValid && mUop.uop.ctrl.div && SVDiv.io.in.ready){
  //   divFire := true.B
  // }

  // when(mUopValid && mUop.uop.ctrl.perm && ~SVPerm.io.out.perm_busy){
  //   perFire := true.B
  // }

  // when(currentStateNext === empty){
  //   fpFire := false.B
  //   divFire := false.B
  //   perFire := false.B
  // }

  SValu.io.in.valid := mUopValid && mUop.uop.ctrl.alu
  SVMac.io.in.valid := mUopValid && mUop.uop.ctrl.mul
  SVMask.io.in.valid := mUopValid && mUop.uop.ctrl.mask
  SVReduc.io.in.valid := mUopValid && mUop.uop.ctrl.redu
  SVDiv.io.in.valid := mUopValid && mUop.uop.ctrl.div
  SVPerm.io.in.rvalid := mUopValid && mUop.uop.ctrl.perm
  SVFpu.io.in.valid := mUopValid && mUop.uop.ctrl.fp

  Seq(SValu.io.in.bits, SVMac.io.in.bits, SVMask.io.in.bits, SVReduc.io.in.bits, SVDiv.io.in.bits, SVFpu.io.in.bits).foreach { iex =>
    iex.uop := mUop.uop
    iex.vs1 := mUop.uopRegInfo.vs1
    iex.vs2 := mUop.uopRegInfo.vs2
    iex.rs1 := mUop.scalar_opnd_1
    iex.oldVd := mUop.uopRegInfo.old_vd
    iex.mask := mUop.uopRegInfo.mask
  }

  SVPerm.io.in.uop := mUop.uop
  //TODO: when id float inst, the rs1 should read from float register file
  SVPerm.io.in.rs1 := mUop.scalar_opnd_1 // || float

  SVPerm.io.in.vs1_preg_idx := VecInit(Seq.tabulate(8)(i => mUop.uop.ctrl.lsrc(0) +
    Mux(io.in.bits.uop.ctrl.vGatherEi16EEW32, i.U >> 1,
      Mux(io.in.bits.uop.ctrl.vGatherEi16EEW64, i.U >> 2, i.U))))
  SVPerm.io.in.vs2_preg_idx := VecInit(Seq.tabulate(8)(i => mUop.uop.ctrl.lsrc(1) + i.U))
  SVPerm.io.in.old_vd_preg_idx := VecInit(Seq.tabulate(8)(i => mUop.uop.ctrl.ldest +
    Mux(io.in.bits.uop.ctrl.vGatherEi16EEW8, i.U >> 1, i.U)))
  SVPerm.io.in.mask_preg_idx := 0.U
  SVPerm.io.in.uop_valid := mUopValid & mUop.uop.ctrl.perm
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

  fixCycleResult := Mux1H(fixCycleVld1H, fixCycleResult1H)

  val fixCycleValid = SValu.io.out.valid || SVMac.io.out.valid || SVMask.io.out.valid || SVReduc.io.out.valid

  when(fixCycleValid) {
    io.out.bits.fflags := 0.U
    io.out.bits.vd := fixCycleResult.vd
    io.out.bits.vxsat := fixCycleResult.vxsat
  }.elsewhen(SVDiv.io.out.valid) {
    io.out.bits.fflags := SVDiv.io.out.bits.fflags
    io.out.bits.vd := SVDiv.io.out.bits.vd
    io.out.bits.vxsat := false.B
  }.elsewhen(SVFpu.io.out.valid) {
    io.out.bits.fflags := SVFpu.io.out.bits.fflags
    io.out.bits.vd := SVFpu.io.out.bits.vd
    io.out.bits.vxsat := false.B
  } otherwise {
    io.out.bits.fflags := 0.U
    io.out.bits.vd := 0.U
    io.out.bits.vxsat := false.B
  }
  io.out.valid := outValid
}




package grapecoveDCache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import _root_.circt.stage.ChiselStage

class MSHR(id: Int) extends Module() {
  val io = IO(new MSHREntryIO)

  val mode_idle :: mode_req_enqueue :: mode_resp_wait :: mode_replay :: mode_clear :: Nil = Enum(5)
  private val state                                                                       = RegInit(mode_idle)

  // req
//  val probeReq    = io.req === MSHRReqType.probe && io.lineAddrMatch && io.reqValid
  val probeReq    = io.probeValid
  val replayReq   = io.req === MSHRReqType.replay && io.reqValid
  val allocateReq = io.req === MSHRReqType.alloc && io.reqValid

  // info regs & wires
  val sentPermission = RegInit(TLPermissions.NtoB)
  val lineAddrReg    = RegEnable(io.allocLineAddr, 0.U, allocateReq && state === mode_idle)

  val metaCounter = RegInit(0.U(log2Up(nMSHRMetas).W))

  // match & output
  val allocLineAddrMatch = lineAddrReg === io.allocLineAddr && state =/= mode_idle
  io.allocLineAddrMatch := allocLineAddrMatch

  /////////////// allocate state flag
  // current entry is sent as read, but here is a write req
  val privErr = (state >= mode_resp_wait) && (sentPermission === TLPermissions.NtoB) && isWriteIntent(io.reqCmd)
  // it is not allowed for write -> read -> write
  val readAfterWriteFlag = RegInit(false.B)
  val trueWriteFlag      = RegInit(false.B)
  val wrwErr             = readAfterWriteFlag && isWrite(io.reqCmd)

  val probeState = Mux(
    state > mode_resp_wait,
    ProbeMSHRState.hitBlockB,
    MuxLookup(io.probePermission, ProbeMSHRState.hitBlockB)(
      Seq(
        TLPermissions.toN -> ProbeMSHRState.hitBlockN,
        TLPermissions.toB -> Mux(
          sentPermission === TLPermissions.NtoB,
          ProbeMSHRState.hitGo,
          ProbeMSHRState.hitBlockB,
        ),
      )
    ),
  )

  io.probeState         := Mux(probeReq, probeState, ProbeMSHRState.miss)
  io.probeLineAddrMatch := lineAddrReg === io.probeLineAddr && probeReq

  readAfterWriteFlag := MuxCase(
    readAfterWriteFlag,
    Seq(
      (state === mode_clear)                                                               -> false.B,
      (allocLineAddrMatch && trueWriteFlag && isRead(io.reqCmd) && !isPrefetch(io.reqCmd)) -> true.B,
    ),
  )

  // don't have enough space to store current inst
  val isFull = metaCounter === (nMSHRMetas - 1).asUInt
//  dontTouch(isFull)

  val stallReq =
    allocLineAddrMatch && (!(state <= mode_resp_wait) || isFull || privErr || wrwErr) && !isPrefetch(io.reqCmd)
  io.stallReq := stallReq
  io.isEmpty  := state === mode_idle

  /////////////// refill state flag & io
  io.writeCounter := Mux(
    trueWriteFlag && allocateReq && isWrite(io.reqCmd),
    metaCounter - 1.U,
    metaCounter,
  )

  io.replayCounter := metaCounter

  // if already has write req, metaCounter won't change
  when(state === mode_clear) {
    metaCounter   := 0.U
    trueWriteFlag := false.B
  }.elsewhen(allocateReq && !stallReq) {
    when(state === mode_idle) {
      metaCounter   := 1.U
      trueWriteFlag := io.isUpgrade || isWrite(io.reqCmd)
    }.elsewhen(allocLineAddrMatch && !isPrefetch(io.reqCmd)) {
      when(!trueWriteFlag && isWrite(io.reqCmd)) {
        trueWriteFlag := true.B
        metaCounter   := metaCounter + 1.U
      }.elsewhen(isRead(io.reqCmd)) {
        metaCounter := metaCounter + 1.U
      }
    }
  }

  /////////////// sent info
  when(state === mode_clear) {
    sentPermission := TLPermissions.NtoB
  }.elsewhen(allocateReq && !stallReq) {
    when(state === mode_idle) {
      sentPermission := Mux(
        io.isUpgrade,
        TLPermissions.BtoT,
        Mux(isWriteIntent(io.reqCmd), TLPermissions.NtoT, TLPermissions.NtoB),
      )
    }.elsewhen(allocLineAddrMatch && !isPrefetch(io.reqCmd)) {
      sentPermission := Mux(
        sentPermission === TLPermissions.NtoB && isWriteIntent(io.reqCmd),
        TLPermissions.NtoT,
        sentPermission,
      )
    }
  }
//    .elsewhen(probeReq) {
//    sentPermission := Mux(
//      io.probePermission === TLPermissions.toN && sentPermission === TLPermissions.BtoT,
//      TLPermissions.NtoT,
//      sentPermission,
//    )
//  }

  // enqueue the sender
  io.senderPermission := sentPermission

  io.senderLineAddr := lineAddrReg

  // FSM
  state := MuxLookup(state, state)(
    Seq(
      mode_idle        -> Mux(allocateReq, mode_req_enqueue, state),
      mode_req_enqueue -> Mux(io.senderResp, mode_resp_wait, state),
      mode_resp_wait   -> Mux(replayReq, mode_replay, state),
      mode_replay      -> Mux(io.replayFinish, mode_clear, state),
      mode_clear       -> mode_idle,
    )
  )

}

class ReplayModule extends Module() {
  val io = IO(new ReplayModuleIO())
  dontTouch(io.innerIO.bits.meta)
  private val replayReg = RegInit(0.U(blockBits.W))

  val mode_idle :: mode_replay :: mode_send_replace :: mode_wait_replace :: mode_clear :: Nil = Enum(5)
  private val state                                                                           = RegInit(mode_idle)

  val metaCounter = RegInit(0.U(log2Up(nMSHRMetas).W))

  val initEnable = io.innerIO.fire
  val mshrEntryIdx = RegEnable(
    Mux(state === mode_clear, 0.U, io.innerIO.bits.entryIdx),
    0.U(log2Up(nMSHRs).W),
    initEnable | state === mode_clear,
  )
  val totalCounter   = RegEnable(io.innerIO.bits.counter, 0.U(log2Up(nMSHRMetas).W), initEnable)
  val replayLineAddr = RegEnable(io.innerIO.bits.lineAddr, 0.U(lineAddrWidth.W), initEnable)
  val replayPerm     = RegEnable(io.innerIO.bits.perm, TLPermissions.toB, initEnable)

  val underReplay = initEnable || state === mode_replay
//  val replayMetaValid = RegEnable(metaCounter < totalCounter, underReplay)
  val replayMeta = RegEnable(io.innerIO.bits.meta, underReplay)

  val writeRecord = RegInit(false.B)

  writeRecord := MuxLookup(state, writeRecord)(
    Seq(
      mode_clear  -> false.B,
      mode_replay -> (writeRecord | isWrite(replayMeta.cmd)),
    )
  )

  val loadgen =
    new LoadGen(
      replayMeta.size,
      replayMeta.signed,
      replayMeta.offset,
      replayReg,
      false.B,
      blockBytes,
    ).data

  def maskedStoreGen(mask: UInt, originData: UInt, newData: UInt): UInt = {
    val originDataByte = originData.asTypeOf(Vec(blockBytes, UInt(8.W)))
    val newDataByte    = newData.asTypeOf(Vec(blockBytes, UInt(8.W)))
    val res            = Wire(Vec(blockBytes, UInt(8.W)))
    for (i <- 0 until blockBytes)
      res(i) := Mux(mask(i), newDataByte(i), originDataByte(i))
    res.asUInt
  }

  metaCounter := MuxCase(
    metaCounter,
    Seq(
      (state === mode_clear) -> 0.U,
      underReplay            -> (metaCounter + 1.U),
    ),
  )

  replayReg := MuxCase(
    replayReg,
    Seq(
      initEnable -> io.innerIO.bits.data,
      ((state === mode_replay) && isWrite(replayMeta.cmd)) ->
        maskedStoreGen(io.innerIO.bits.mask, replayReg, io.innerIO.bits.data),
    ),
  )

  state := MuxLookup(state, state)(
    Seq(
      mode_idle         -> Mux(io.innerIO.fire, mode_replay, state),
      mode_replay       -> Mux(metaCounter >= totalCounter, mode_send_replace, state),
      mode_send_replace -> Mux(io.toReplace.ready, mode_wait_replace, state),
      mode_wait_replace -> Mux(
        io.replaceStatus === ReplaceStatus.replace_finish,
        mode_clear,
        Mux(io.replaceStatus === ReplaceStatus.replace_replay, mode_send_replace, state),
      ),
      mode_clear -> mode_idle,
    )
  )

  // inner connect
  io.innerIO.ready := state === mode_idle
  io.idxMeta       := metaCounter
  io.replayIdx     := Mux(state === mode_idle, io.innerIO.bits.entryIdx, mshrEntryIdx)

  // replace signal connect
  io.toReplace.valid := state === mode_send_replace

  io.toReplace.bits.state := MuxLookup(replayPerm, ClientStates.Branch)(
    Seq(
      TLPermissions.toB -> ClientStates.Branch,
      TLPermissions.toT -> Mux(writeRecord, ClientStates.Dirty, ClientStates.Trunk),
    )
  )

  io.toReplace.bits.lineAddr := replayLineAddr
  io.toReplace.bits.data     := replayReg

  // replay output
  io.toPipe.bits.nextCycleWb := underReplay && (metaCounter < Mux(
    state === mode_idle,
    io.innerIO.bits.counter,
    totalCounter,
  )) && !(isPrefetch(io.innerIO.bits.meta.cmd) || isWriteIntent(io.innerIO.bits.meta.cmd))

  io.toPipe.valid         := RegNext(io.toPipe.bits.nextCycleWb)
  io.toPipe.bits.regIdx   := replayMeta.regIdx
  io.toPipe.bits.sourceId := replayMeta.sourceId
  io.toPipe.bits.regData  := loadgen

}

class MSHRFile extends Module() {

  val io = IO(
    new Bundle {
      val pipelineReq = Flipped(DecoupledIO(new CachepipeMSHRFile))
      val addrMatch   = Output(Bool())
      val toL2Req     = DecoupledIO(new MSHRFileL2)              // TL A
      val fromRefill  = Flipped(DecoupledIO(new RefillMSHRFile)) // TL D/E

      val probeCheck  = new ProbeMSHRFile
      val probeRefill = ValidIO(new ProbeRefill)

      val toPipeline    = ValidIO(new MSHRPipeResp()) // read resp
      val toReplace     = DecoupledIO(new MSHRReplace())
      val replaceStatus = Input(ReplaceStatus())
    }
  )

  val replayReg = Module(new ReplayModule)

  val metaArray = RegInit(
    VecInit(
      Seq.fill(nMSHRs)(VecInit(Seq.fill(nMSHRMetas)(0.U((new ReqMetaBundle).getWidth.W))))
    )
  )

  val maskArray = RegInit(
    VecInit(
      Seq.fill(nMSHRs)(0.U(blockBytes.W))
    )
  )

  val dataArray = RegInit(
    VecInit(
      Seq.fill(nMSHRs)(0.U(blockBits.W))
    )
  )

  // general signal
  val lineAddrMatchList = Wire(Vec(nMSHRs, Bool()))
  val lineAddrMatch     = lineAddrMatchList.asUInt.orR
  val lineAddrMatchIdx  = OHToUInt(lineAddrMatchList)

  val stallReqList = Wire(Vec(nMSHRs, Bool()))
  val stallReq     = stallReqList.asUInt.orR

  val allocateArb = Module(new Arbiter(Bool(), nMSHRs))
  allocateArb.io.in.foreach(_.bits := DontCare)

  val reqArb = Module(new Arbiter(MSHRReqType(), 2))
//  val probeReq  = reqArb.io.in(0).fire
  val replayReq = reqArb.io.in(0).fire
  val allocReq  = reqArb.io.in(1).fire

//  reqArb.io.in(0).valid := io.probeCheck.valid
//  reqArb.io.in(0).bits  := MSHRReqType.probe

  reqArb.io.in(0).valid := io.fromRefill.valid && replayReg.io.innerIO.ready
  reqArb.io.in(0).bits  := MSHRReqType.replay
  io.fromRefill.ready   := reqArb.io.in(0).ready && replayReg.io.innerIO.ready

  reqArb.io.in(1).valid := io.pipelineReq.valid
  reqArb.io.in(1).bits  := Mux(io.pipelineReq.valid, MSHRReqType.alloc, MSHRReqType.invalid)
  io.pipelineReq.ready  := reqArb.io.in(1).ready
  io.addrMatch          := lineAddrMatch

  reqArb.io.out.ready := MuxLookup(reqArb.io.out.bits, false.B)(
    Seq(
//      MSHRReqType.probe  -> true.B,
      MSHRReqType.replay -> true.B,
      MSHRReqType.alloc  -> ((!lineAddrMatch && allocateArb.io.out.valid) || (lineAddrMatch && !stallReq)),
    )
  )
  replayReg.io.innerIO.valid := replayReq

  allocateArb.io.out.ready := allocReq && !lineAddrMatch

  // interface for probe
  val probeReq               = io.probeCheck.valid
  val probeLineAddrMatchList = Wire(Vec(nMSHRs, Bool()))
  val probeLineAddrMatch     = probeLineAddrMatchList.asUInt.orR
  val probeLineAddrMatchIdx  = OHToUInt(probeLineAddrMatchList)

  val probeStateList = Wire(Vec(nMSHRs, UInt(ProbeMSHRState.width.W)))
  val probeState     = probeStateList.reduce(_ | _)
  io.probeCheck.replaceFinish := io.replaceStatus === ReplaceStatus.replace_finish &&
    io.probeCheck.lineAddr === replayReg.io.toReplace.bits.lineAddr

  io.probeRefill.valid        := io.probeCheck.valid
  io.probeRefill.bits.entryId := probeLineAddrMatchIdx

  io.probeCheck.pass := probeReq && (probeState === ProbeMSHRState.hitGo | (probeState === ProbeMSHRState.hitBlockN && !io.fromRefill.bits.probeMatch))
  io.probeCheck.hit := probeReq && probeLineAddrMatch

  // interface for replay
  val writeCounterList  = Wire(Vec(nMSHRs, UInt(log2Up(nMSHRMetas).W)))
  val replayCounterList = Wire(Vec(nMSHRs, UInt(log2Up(nMSHRMetas).W)))

  val replayFinishRespList = Wire(Vec(nMSHRs, Bool()))

  // interface for allocate
  val allocateList = Wire(Vec(nMSHRs, Bool()))
  val allocateIdx  = OHToUInt(allocateList.asUInt)

  // interface for new entry sender
  val senderQueue = Module(new Queue(UInt(log2Up(nMSHRs).W), nMSHRs))
  // val senderReqList  = Wire(Vec(nMSHRs, Bool()))
  val senderRespList = Wire(Vec(nMSHRs, Bool()))
  // val senderIdx      = OHToUInt(senderReqList)

  val lineAddrList         = Wire(Vec(nMSHRs, UInt(lineAddrWidth.W)))
  val senderPermissionList = Wire(Vec(nMSHRs, UInt(TLPermissions.aWidth.W)))

  // connect mshr
  val mshrs = (0 until nMSHRs) map {
    i =>
      val mshr = Module(new MSHR(i))

      mshr.io.reqValid := MuxLookup(reqArb.io.out.bits, false.B)(
        Seq(
//          MSHRReqType.probe  -> true.B,
          MSHRReqType.replay -> (io.fromRefill.bits.entryId === i.asUInt),
          MSHRReqType.alloc  -> (allocateArb.io.in(i).fire || mshr.io.allocLineAddrMatch),
        )
      )
      mshr.io.req           := reqArb.io.out.bits
      mshr.io.allocLineAddr := io.pipelineReq.bits.lineAddr
      mshr.io.probeValid    := probeReq
      mshr.io.probeLineAddr := io.probeCheck.lineAddr
      mshr.io.reqCmd        := io.pipelineReq.bits.meta.cmd
      mshr.io.isUpgrade     := io.pipelineReq.bits.isUpgrade

      lineAddrMatchList(i)      := mshr.io.allocLineAddrMatch
      probeLineAddrMatchList(i) := mshr.io.probeLineAddrMatch

      // probe signal
      probeStateList(i)       := mshr.io.probeState
      mshr.io.probePermission := io.probeCheck.probePermission

      // replay & refill signal
      writeCounterList(i)  := mshr.io.writeCounter
      replayCounterList(i) := mshr.io.replayCounter

      mshr.io.replayFinish := replayFinishRespList(i)

      // allocate signal
      allocateArb.io.in(i).valid := mshr.io.isEmpty
      allocateList(i)            := allocateArb.io.in(i).fire

      stallReqList(i) := mshr.io.stallReq

      // sender signal
      lineAddrList(i) := mshr.io.senderLineAddr
      // senderReqList(i)        := mshr.io.isEmpty & allocateArb.io.in(i).fire
      senderPermissionList(i) := mshr.io.senderPermission
      mshr.io.senderResp      := senderRespList(i)
  }

  // Write Mask & Data Array
  def dataMergeGen(mask: UInt, data: UInt): UInt = {
    val byteData = data.asTypeOf(Vec(blockBytes, UInt(8.W)))
    var res      = Wire(Vec(blockBytes, UInt(8.W)))
    for (i <- 0 until blockBytes)
      res(i) := Mux(mask(i), byteData(i), 0.U)
    res.asUInt
  }

  val wrIdx        = Mux(lineAddrMatch, lineAddrMatchIdx, allocateIdx)
  val metaWriteIdx = writeCounterList(wrIdx)

  val dataArrayWriteEna = RegInit(false.B)
  val dataArrayWriteIdx = RegInit(0.U(nMSHRs.W))

  when(io.pipelineReq.fire && Mux(lineAddrMatch, !isPrefetch(io.pipelineReq.bits.meta.cmd), true.B)) {
    // read/write => update meta info
    metaArray(wrIdx)(metaWriteIdx) := io.pipelineReq.bits.meta.asUInt

    // write => update data array & mask array
    when(isWrite(io.pipelineReq.bits.meta.cmd)) {
      dataArrayWriteEna := true.B
      dataArrayWriteIdx := wrIdx
    }.otherwise {
      dataArrayWriteEna := false.B
    }
  }.elsewhen(io.replaceStatus === ReplaceStatus.replace_finish) {
    maskArray(replayReg.io.replayIdx) := 0.U
    dataArray(replayReg.io.replayIdx) := 0.U
    dataArrayWriteEna                 := false.B
  }.otherwise {
    dataArrayWriteEna := false.B
  }

  // receive miss addr data at s2
  when(dataArrayWriteEna) {
//    dataArrayWriteEna := false.B
    dataArray(dataArrayWriteIdx) := Mux(
      RegNext(io.pipelineReq.bits.isUpgrade),
      io.pipelineReq.bits.data,
      dataMergeGen(~io.pipelineReq.bits.mask, dataArray(dataArrayWriteIdx)) |
        dataMergeGen(io.pipelineReq.bits.mask, io.pipelineReq.bits.data),
    )
    maskArray(dataArrayWriteIdx) := io.pipelineReq.bits.mask | maskArray(dataArrayWriteIdx)
  }

  // sender queue
  senderQueue.io.enq.valid := allocateList.asUInt.orR
  senderQueue.io.enq.bits  := allocateIdx

  io.toL2Req.valid         := senderQueue.io.deq.valid && !dataArrayWriteEna
  senderQueue.io.deq.ready := io.toL2Req.ready && !dataArrayWriteEna
  io.toL2Req.bits.perm     := senderPermissionList(senderQueue.io.deq.bits)
  io.toL2Req.bits.entryId  := senderQueue.io.deq.bits
  io.toL2Req.bits.lineAddr := lineAddrList(senderQueue.io.deq.bits)

  senderRespList := Mux(
    io.toL2Req.ready,
    UIntToOH(senderQueue.io.deq.bits, nMSHRs),
    0.U,
  ).asBools

  // refill queue wakeup MSHR FSM & replay reg
  // replayReg.io.innerIO.valid         := io.fromRefill.valid
  // io.fromRefill.ready                := replayReg.io.innerIO.ready
  replayReg.io.innerIO.bits.perm     := io.fromRefill.bits.perm
  replayReg.io.innerIO.bits.lineAddr := lineAddrList(io.fromRefill.bits.entryId)

  replayReg.io.innerIO.bits.entryIdx := io.fromRefill.bits.entryId
  replayReg.io.innerIO.bits.meta := metaArray(replayReg.io.replayIdx)(replayReg.io.idxMeta).asTypeOf(new ReqMetaBundle)

  replayReg.io.innerIO.bits.mask := maskArray(replayReg.io.replayIdx)

  replayReg.io.innerIO.bits.data := Mux(
    io.fromRefill.fire,
    Mux(
      !io.fromRefill.bits.hasData,
      dataArray(replayReg.io.replayIdx),
      io.fromRefill.bits.data,
    ),
    dataArray(replayReg.io.replayIdx),
  )
  replayReg.io.innerIO.bits.counter := replayCounterList(replayReg.io.replayIdx)

  io.toPipeline <> replayReg.io.toPipe
  io.toReplace <> replayReg.io.toReplace
  replayReg.io.replaceStatus := io.replaceStatus

  replayFinishRespList := Mux(
    io.replaceStatus === ReplaceStatus.replace_finish,
    UIntToOH(replayReg.io.replayIdx),
    0.U,
  ).asBools

}

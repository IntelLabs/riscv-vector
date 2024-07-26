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
  val probeReq    = io.req === MSHRReqType.probe && io.lineAddrMatch
  val replayReq   = io.req === MSHRReqType.replay
  val allocateReq = io.req === MSHRReqType.alloc

  // info regs & wires
  val sentPermission = RegInit(TLPermissions.NtoB)
  val lineAddrReg    = RegEnable(io.reqLineAddr, 0.U, allocateReq && state === mode_idle)

  val metaCounter = RegInit(0.U(log2Up(nMSHRMetas).W))

  // match & output
  val lineAddrMatch = lineAddrReg === io.reqLineAddr && state =/= mode_idle
  io.lineAddrMatch := lineAddrMatch

  /////////////// allocate state flag
  // current entry is sent as read, but here is a write req
  val privErr = (state >= mode_resp_wait) && (sentPermission === TLPermissions.NtoB) && io.reqType.asBool
  // it is not allowed for write -> read -> write
  val readAfterWriteFlag = RegInit(false.B)
  val wrwErr             = readAfterWriteFlag && io.reqType.asBool

  val probeState = Mux(
    state >= mode_resp_wait,
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

  io.probeState := Mux(probeReq, probeState, ProbeMSHRState.miss)

  readAfterWriteFlag := MuxCase(
    readAfterWriteFlag,
    Seq(
      (state === mode_clear)                                                  -> false.B,
      (lineAddrMatch && sentPermission =/= TLPermissions.NtoB && !io.reqType) -> true.B,
    ),
  )

  // don't have enough space to store current inst
  val isFull = (metaCounter + 1.U) >= nMSHRMetas.asUInt

  val stallReq = lineAddrMatch && (!(state <= mode_resp_wait) || isFull || privErr || wrwErr)
  io.stallReq := stallReq
  io.isEmpty  := state === mode_idle

  /////////////// refill state flag & io
  io.metaCounter := Mux(
    sentPermission =/= TLPermissions.NtoB && allocateReq && io.reqType.asBool,
    metaCounter - 1.U,
    metaCounter,
  )

  // if already has write req, metaCounter won't change
  when(state === mode_clear) {
    metaCounter := 0.U
  }.elsewhen(allocateReq && !stallReq) {
    when(state === mode_idle) {
      metaCounter := 1.U
    }.elsewhen(lineAddrMatch) {
      when(sentPermission === TLPermissions.NtoB && io.reqType.asBool) {
        metaCounter := metaCounter + 1.U
      }.elsewhen(!io.reqType.asBool) {
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
        Mux(io.reqType.asBool, TLPermissions.NtoT, TLPermissions.NtoB),
      )
    }.elsewhen(lineAddrMatch) {
      sentPermission := Mux(
        sentPermission === TLPermissions.NtoB,
        TLPermissions.NtoT,
        sentPermission,
      )
    }
  }.elsewhen(probeReq) {
    sentPermission := Mux(
      io.probePermission === TLPermissions.toN && sentPermission === TLPermissions.BtoT,
      TLPermissions.NtoT,
      sentPermission,
    )
  }

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

  val initEnable     = io.innerIO.fire
  val mshrEntryIdx   = RegEnable(io.innerIO.bits.entryIdx, 0.U(log2Up(nMSHRs).W), initEnable)
  val totalCounter   = RegEnable(io.innerIO.bits.counter, 0.U(log2Up(nMSHRMetas).W), initEnable)
  val replayLineAddr = RegEnable(io.innerIO.bits.lineAddr, 0.U(lineAddrWidth.W), initEnable)
  val replayPerm     = RegEnable(io.innerIO.bits.perm, TLPermissions.NtoB, initEnable)

  val underReplay = initEnable || state === mode_replay
//  val replayMetaValid = RegEnable(metaCounter < totalCounter, underReplay)
  val replayMeta = RegEnable(io.innerIO.bits.meta, underReplay)

  val writeRecord = RegInit(false.B)

  writeRecord := MuxLookup(state, writeRecord)(
    Seq(
      mode_clear  -> false.B,
      mode_replay -> (writeRecord | replayMeta.rwType.asBool),
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
    ).genData(2)

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
      ((state === mode_replay) && replayMeta.rwType.asBool) ->
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
  io.replayIdx     := mshrEntryIdx

  // replace signal connect
  io.toReplace.valid := state === mode_send_replace

  io.toReplace.bits.state := MuxLookup(replayPerm, ClientStates.Branch)(
    Seq(
      TLPermissions.NtoB -> ClientStates.Branch,
      TLPermissions.BtoT -> ClientStates.Dirty,
      TLPermissions.NtoT -> Mux(writeRecord, ClientStates.Dirty, ClientStates.Trunk),
    )
  )

  io.toReplace.bits.lineAddr := replayLineAddr
  io.toReplace.bits.data     := replayReg

  // replay output
  io.toPipe.bits.nextCycleWb := underReplay && (metaCounter < Mux(
    state === mode_idle,
    io.innerIO.bits.counter,
    totalCounter,
  )) && !io.innerIO.bits.meta.rwType

  io.toPipe.valid         := RegNext(io.toPipe.bits.nextCycleWb)
  io.toPipe.bits.regIdx   := replayMeta.regIdx
  io.toPipe.bits.sourceId := replayMeta.sourceId
  io.toPipe.bits.regData  := loadgen

}

class MSHRFile extends Module() {

  val io = IO(
    new Bundle {
      val pipelineReq = Flipped(DecoupledIO(new CachepipeMSHRFile))
      val toL2Req     = DecoupledIO(new MSHRFileL2)              // TL A
      val fromRefill  = Flipped(DecoupledIO(new RefillMSHRFile)) // TL D/E

      val probeCheck  = new ProbeMSHRFile
      val probeRefill = ValidIO(new ProbeRefill)

      val toPipeline    = DecoupledIO(new MSHRPipeResp()) // read resp
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

  val reqArb    = Module(new Arbiter(MSHRReqType(), 3))
  val probeReq  = reqArb.io.in(0).fire
  val replayReq = reqArb.io.in(1).fire
  val allocReq  = reqArb.io.in(2).fire

  reqArb.io.in(0).valid := io.probeCheck.valid
  reqArb.io.in(0).bits  := MSHRReqType.probe

  reqArb.io.in(1).valid := io.fromRefill.valid && replayReg.io.innerIO.ready
  reqArb.io.in(1).bits  := MSHRReqType.replay
  io.fromRefill.ready   := reqArb.io.in(1).ready && replayReg.io.innerIO.ready

  reqArb.io.in(2).valid := io.pipelineReq.valid
  reqArb.io.in(2).bits  := Mux(io.pipelineReq.valid, MSHRReqType.alloc, MSHRReqType.invalid)
  io.pipelineReq.ready  := reqArb.io.in(2).ready

  reqArb.io.out.ready := MuxLookup(reqArb.io.out.bits, false.B)(
    Seq(
      MSHRReqType.probe  -> true.B,
      MSHRReqType.replay -> true.B,
      MSHRReqType.alloc  -> ((!lineAddrMatch && allocateArb.io.out.valid) || (lineAddrMatch && !stallReq)),
    )
  )
  replayReg.io.innerIO.valid := replayReq

  allocateArb.io.out.ready := allocReq && !lineAddrMatch

  // interface for probe
  val probeStateList = Wire(Vec(nMSHRs, UInt(ProbeMSHRState.width.W)))
  val probeState     = probeStateList.reduce(_ | _)
  io.probeCheck.replaceFinish := io.replaceStatus === ReplaceStatus.replace_finish

  io.probeRefill.valid        := io.probeCheck.valid
  io.probeRefill.bits.entryId := lineAddrMatchIdx

  io.probeCheck.pass := probeReq && (probeState === ProbeMSHRState.hitGo | (probeState === ProbeMSHRState.hitBlockN && !io.fromRefill.bits.probeMatch))
  io.probeCheck.hit := probeReq && lineAddrMatch

  // interface for replay
  val metaCounterList = Wire(Vec(nMSHRs, UInt(log2Up(nMSHRMetas).W)))

  val replayFinishRespList = Wire(Vec(nMSHRs, Bool()))

  // interface for allocate
  val allocateList = Wire(Vec(nMSHRs, Bool()))
  val allocateIdx  = OHToUInt(allocateList.asUInt)

  val reqLineAddr = Mux(io.probeCheck.valid, io.probeCheck.lineAddr, io.pipelineReq.bits.lineAddr)

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

      mshr.io.req := Mux(
        (allocReq && !allocateArb.io.in(
          i
        ).ready && !mshr.io.lineAddrMatch) || (replayReq && io.fromRefill.bits.entryId =/= i.asUInt),
        MSHRReqType.invalid,
        reqArb.io.out.bits,
      )
      mshr.io.reqLineAddr := reqLineAddr
      mshr.io.reqType     := io.pipelineReq.bits.meta.rwType
      mshr.io.isUpgrade   := io.pipelineReq.bits.isUpgrade

      lineAddrMatchList(i) := mshr.io.lineAddrMatch

      // probe signal
      probeStateList(i)       := mshr.io.probeState
      mshr.io.probePermission := io.probeCheck.probePermission

      // replay & refill signal
      metaCounterList(i) := mshr.io.metaCounter

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
  val metaWriteIdx = metaCounterList(wrIdx)
  when(io.pipelineReq.ready) {
    metaArray(wrIdx)(metaWriteIdx) := io.pipelineReq.bits.meta.asUInt
    maskArray(wrIdx)               := io.pipelineReq.bits.mask | maskArray(wrIdx)
    dataArray(wrIdx) := dataMergeGen(
      ~io.pipelineReq.bits.mask,
      dataArray(wrIdx),
    ) |
      dataMergeGen(
        Mux(io.pipelineReq.bits.isUpgrade, Fill(blockBytes, 1.U(1.W)), io.pipelineReq.bits.mask),
        io.pipelineReq.bits.data,
      )
  }.elsewhen(io.replaceStatus === ReplaceStatus.replace_finish) {
    maskArray(replayReg.io.replayIdx) := 0.U
    dataArray(replayReg.io.replayIdx) := 0.U
  }

  // sender queue
  senderQueue.io.enq.valid := allocateList.asUInt.orR
  senderQueue.io.enq.bits  := allocateIdx

  io.toL2Req.valid         := senderQueue.io.deq.valid
  senderQueue.io.deq.ready := io.toL2Req.ready
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
  replayReg.io.innerIO.bits.perm     := senderPermissionList(io.fromRefill.bits.entryId)
  replayReg.io.innerIO.bits.lineAddr := lineAddrList(io.fromRefill.bits.entryId)

  replayReg.io.innerIO.bits.entryIdx := io.fromRefill.bits.entryId
  replayReg.io.innerIO.bits.meta := metaArray(replayReg.io.replayIdx)(replayReg.io.idxMeta).asTypeOf(new ReqMetaBundle)

  replayReg.io.innerIO.bits.mask := maskArray(replayReg.io.replayIdx)

  replayReg.io.innerIO.bits.data := Mux(
    io.fromRefill.valid,
    Mux(
      senderPermissionList(io.fromRefill.bits.entryId) === TLPermissions.BtoT,
      dataArray(replayReg.io.replayIdx),
      io.fromRefill.bits.data,
    ),
    dataArray(replayReg.io.replayIdx),
  )
  replayReg.io.innerIO.bits.counter := metaCounterList(replayReg.io.replayIdx)

  io.toPipeline <> replayReg.io.toPipe
  io.toReplace <> replayReg.io.toReplace
  replayReg.io.replaceStatus := io.replaceStatus

  replayFinishRespList := Mux(
    io.replaceStatus === ReplaceStatus.replace_finish,
    UIntToOH(replayReg.io.replayIdx),
    0.U,
  ).asBools

}

/** For Test * */

//object MSHRFile extends App {
//
//  val firtoolOptions = Array(
//    "--lowering-options=" + List(
//      "disallowLocalVariables",
//      "disallowPackedArrays",
//      "locationInfoStyle=wrapInAtSquareBracket",
//    ).reduce(_ + "," + _)
//  )
//
//  ChiselStage.emitSystemVerilogFile(new MSHRFile, args, firtoolOptions ++ Array("--disable-all-randomization"))
//}

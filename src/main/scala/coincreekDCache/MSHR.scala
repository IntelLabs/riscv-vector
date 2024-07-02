package coincreekDCache

import chisel3._
import chisel3.util._
import util._
import freechips.rocketchip.tilelink._
import coincreekDCache.util._
import _root_.circt.stage.ChiselStage

class MSHR(id: Int) extends Module() {
  val io = IO(new MSHREntryIO)

  val mode_idle :: mode_req_enqueue :: mode_resp_wait :: mode_replay :: mode_clear :: Nil = Enum(5)
  private val state                                                                       = RegInit(mode_idle)

  // req priority
  val probeReq    = io.req(2)
  val replayReq   = !probeReq && io.req(1)
  val allocateReq = !io.req(2, 1).orR && io.req(0) // && state === mode_idle

  // info regs & wires
  val sentPermission = RegInit(0.U(TLPermissions.aWidth.W))
  val lineAddrReg    = RegEnable(io.reqLineAddr, 0.U, allocateReq && state === mode_idle)

  val metaCounter = RegInit(0.U(log2Up(mshrEntryMetaNum).W))

  // match & output
  val lineAddrMatch = lineAddrReg === io.reqLineAddr
  io.lineAddrMatch := lineAddrMatch

  /////////////// allocate state flag
  // current entry is sent as read, but here is a write req
  val privErr = (state === mode_resp_wait) && (sentPermission === TLPermissions.NtoB) && io.reqType.asBool
  // it is not allowed for write -> read -> write
  val readAfterWriteFlag = RegInit(false.B)
  val wrwErr             = readAfterWriteFlag && io.reqType.asBool
  val probeBlock         = probeReq && io.probePermission === TLPermissions.toB && sentPermission =/= TLPermissions.NtoB
  io.probeBlock := probeBlock && probeReq

  readAfterWriteFlag := MuxCase(
    readAfterWriteFlag,
    Seq(
      (state === mode_clear)                                                  -> false.B,
      (lineAddrMatch && sentPermission =/= TLPermissions.NtoB && !io.reqType) -> true.B
    )
  )

  // don't have enough space to store current inst
  val isFull = (metaCounter + 1.U) > mshrEntryMetaNum.asUInt

  val stallReq = lineAddrMatch && (!(state <= mode_resp_wait) || isFull || privErr || wrwErr)
  io.stallReq := stallReq
  io.isEmpty  := state === mode_idle

  /////////////// refill state flag & io
  io.metaCounter := metaCounter

  // if already has write req, metaCounter won't change
  when(allocateReq && !(io.isUpgrade || (io.reqType.asBool && sentPermission === TLPermissions.BtoT))) {
    when(
      lineAddrMatch && !stallReq && !io.maskConflict &&
        (!io.reqType.asBool || (io.reqType.asBool && !sentPermission =/= TLPermissions.NtoT))
    ) {
      metaCounter := metaCounter + 1.U
    }
  }.elsewhen(state === mode_clear) {
    metaCounter := 0.U
  }

  /////////////// sent info
  when(state === mode_clear) {
    sentPermission := TLPermissions.NtoB
  }.elsewhen(allocateReq && state === mode_idle) {
    sentPermission := Mux(
      io.isUpgrade,
      TLPermissions.BtoT,
      Mux(io.reqType.asBool, TLPermissions.NtoT, TLPermissions.NtoB)
    )
  }.elsewhen(lineAddrMatch) {
    when(probeReq) {
      sentPermission := MuxLookup(io.probePermission, sentPermission)(
        Seq(
          TLPermissions.toT ->
            Mux(
              sentPermission === TLPermissions.NtoB,
              TLPermissions.NtoT,
              sentPermission
            ),
          TLPermissions.toN ->
            Mux(sentPermission === TLPermissions.BtoT, TLPermissions.NtoT, sentPermission)
        )
      )
    }.elsewhen(allocateReq && !stallReq && !io.maskConflict) {
      sentPermission := Mux(
        sentPermission === TLPermissions.BtoT,
        sentPermission,
        Mux(io.reqType.asBool, TLPermissions.NtoT, sentPermission)
      )
    }
  }

  // enqueue the sender
  io.senderPermission := Mux(
    sentPermission === TLPermissions.NtoB && lineAddrMatch && !stallReq && !io.maskConflict,
    TLPermissions.NtoT,
    sentPermission
  )

  io.senderLineAddr := lineAddrReg

  // FSM
  state := MuxLookup(state, state)(
    Seq(
      mode_idle        -> Mux(allocateReq, mode_req_enqueue, state),
      mode_req_enqueue -> Mux(io.senderResp, mode_resp_wait, state),
      mode_resp_wait   -> Mux(lineAddrMatch && replayReq, mode_replay, state),
      mode_replay      -> Mux(io.replayFinish, mode_clear, state),
      mode_clear       -> mode_idle
    )
  )

}

class ReplayModule extends Module() {
  val io                = IO(new ReplayModuleIO())
  private val replayReg = RegInit(0.U(mshrDataWidth.W))

  val mode_idle :: mode_replay :: mode_wait_replace :: mode_clear :: Nil = Enum(4)
  private val state                                                      = RegInit(mode_idle)

  val metaCounter = RegInit(0.U(log2Up(mshrEntryMetaNum).W))

  val initEnable     = state === mode_idle && io.innerIO.valid
  val mshrEntryIdx   = RegEnable(io.innerIO.bits.entryIdx, 0.U(log2Up(mshrEntryNum).W), initEnable)
  val totalCounter   = RegEnable(io.innerIO.bits.counter, 0.U(log2Up(mshrEntryMetaNum).W), initEnable)
  val replayLineAddr = RegEnable(io.innerIO.bits.lineAddr, 0.U(lineAddrWidth.W), initEnable)
  val replayPerm     = RegEnable(io.innerIO.bits.perm, TLPermissions.NtoB, initEnable)

  val replayStall = RegInit(false.B)
  val writeRecord = RegInit(false.B)

  writeRecord := MuxLookup(state, writeRecord)(
    Seq(
      mode_clear  -> false.B,
      mode_replay -> (writeRecord | io.innerIO.bits.meta.rw_type.asBool)
    )
  )

  val loadgen =
    new LoadGen(
      io.innerIO.bits.meta.typ,
      io.innerIO.bits.meta.signed,
      io.innerIO.bits.meta.offset,
      replayReg,
      false.B,
      mshrDataWidth / 8
    )

  def maskedStoreGen(mask: UInt, originData: UInt, newData: UInt): UInt = {
    val originDataByte = originData.asTypeOf(Vec(mshrDataWidth / 8, UInt(8.W)))
    val newDataByte    = newData.asTypeOf(Vec(mshrDataWidth / 8, UInt(8.W)))
    val res            = Wire(Vec(mshrDataWidth / 8, UInt(8.W)))
    for (i <- 0 until mshrDataWidth / 8)
      res(i) := Mux(mask(i), newDataByte(i), originDataByte(i))
    res.asUInt
  }

  when(!replayStall) {
    metaCounter := MuxLookup(state, metaCounter)(
      Seq(
        mode_clear  -> 0.U,
        mode_replay -> (metaCounter + 1.U)
      )
    )

    replayReg := MuxCase(
      replayReg,
      Seq(
        initEnable -> io.innerIO.bits.data,
        ((state === mode_replay) && io.innerIO.bits.meta.rw_type.asBool) ->
          maskedStoreGen(io.innerIO.bits.mask, replayReg, io.innerIO.bits.data)
      )
    )

    state := MuxLookup(state, state)(
      Seq(
        mode_idle         -> Mux(io.innerIO.valid, mode_replay, state),
        mode_replay       -> Mux((metaCounter + 1.U) >= totalCounter, mode_wait_replace, state),
        mode_wait_replace -> Mux(io.replaceFinish, mode_clear, state),
        mode_clear        -> mode_idle
      )
    )

  }

  // inner connect
  io.innerIO.ready := state === mode_idle
  io.idxMeta       := metaCounter
  io.replayIdx     := mshrEntryIdx

  // replace signal connect
  val replaceSendFlag = RegInit(false.B)

  replaceSendFlag := Mux(
    state =/= mode_wait_replace,
    false.B,
    Mux(io.toReplace.ready, true.B, replaceSendFlag)
  )

  io.toReplace.valid := state === mode_wait_replace && !replaceSendFlag

  io.toReplace.bits.state := MuxLookup(replayPerm, ClientStates.Branch)(
    Seq(
      TLPermissions.NtoB -> ClientStates.Branch,
      TLPermissions.BtoT -> ClientStates.Dirty,
      TLPermissions.NtoT -> Mux(writeRecord, ClientStates.Dirty, ClientStates.Trunk)
    )
  )

  io.toReplace.bits.lineAddr := replayLineAddr
  io.toReplace.bits.data     := replayReg

  // replay output
  io.toPipe.valid        := (state === mode_replay) && !io.innerIO.bits.meta.rw_type
  replayStall            := io.toPipe.valid && !io.toPipe.ready
  io.toPipe.bits.regIdx  := io.innerIO.bits.meta.regIdx
  io.toPipe.bits.regData := loadgen.genData(typMax)

}

class MSHRFile extends Module() {

  val io = IO(
    new Bundle {
      val pipelineReq = Flipped(DecoupledIO(new CachepipeMSHRFile))
      val toL2Req     = DecoupledIO(new MSHRFileL2)              // TL A
      val fromRefill  = Flipped(DecoupledIO(new RefillMSHRFile)) // TL D/E
      val fromProbe   = new ProbeMSHRFile

      val toPipeline    = DecoupledIO(new MSHRPipeResp()) // read resp
      val toReplace     = DecoupledIO(new MSHRReplace())
      val replaceFinish = Input(Bool())
    }
  )

  val replayReg = Module(new ReplayModule)

  val metaArray = RegInit(
    VecInit(
      Seq.fill(mshrEntryNum)(VecInit(Seq.fill(mshrEntryMetaNum)(0.U((new ReqMetaBundle).getWidth.W))))
    )
  )

  val maskArray = RegInit(
    VecInit(
      Seq.fill(mshrEntryNum)(0.U(mshrMaskWidth.W))
    )
  )

  val dataArray = RegInit(
    VecInit(
      Seq.fill(mshrEntryNum)(0.U(mshrDataWidth.W))
    )
  )

// general signal
  val lineAddrMatchList    = Wire(Vec(mshrEntryNum, Bool()))
  val lineAddrMatch        = lineAddrMatchList.asUInt.orR
  val lineAddrMatchIdxList = Wire(Vec(mshrEntryNum, UInt(log2Up(mshrEntryNum).W)))
  val lineAddrMatchIdx     = lineAddrMatchIdxList.reduce(_ | _)

  // interface for probe
  val probeReq       = io.fromProbe.valid
  val probeBlockList = Wire(Vec(mshrEntryNum, Bool()))
  io.fromProbe.probeMatch := probeReq && lineAddrMatch
  io.fromProbe.probeBlock := probeReq && (probeBlockList.asUInt | lineAddrMatchList.asUInt).orR

  // interface for replay
  val replayReq = io.fromRefill.valid
  io.fromRefill.ready := replayReg.io.innerIO.ready && !probeReq

  val metaCounterList = Wire(Vec(mshrEntryNum, UInt(log2Up(mshrEntryMetaNum).W)))

  val replayFinishRespList = Wire(Vec(mshrEntryNum, Bool()))

  // interface for allocate
  val allocateReq = io.pipelineReq.valid

  val allocateArb = Module(new Arbiter(Bool(), mshrEntryNum))
  allocateArb.io.in.foreach(_.bits := DontCare)

  val metaWriteIdx = metaCounterList(lineAddrMatchIdx)

  val stallReqList = Wire(Vec(mshrEntryNum, Bool()))
  val stallReq     = stallReqList.asUInt.orR

  val maskConflict = allocateReq && lineAddrMatch && (io.pipelineReq.bits.mask & maskArray(lineAddrMatchIdx)).orR

  val reqLineAddr = MuxCase( // TODO del tagmatch req from refill queue
    io.pipelineReq.bits.lineAddr,
    Seq(
      probeReq -> io.fromProbe.lineAddr
    )
  )

  // interface for new entry sender
  val senderQueue    = Module(new Queue(UInt(log2Up(mshrEntryNum).W), mshrEntryNum))
  val senderReqList  = Wire(Vec(mshrEntryNum, Bool()))
  val senderRespList = Wire(Vec(mshrEntryNum, Bool()))
  val senderIdxList  = Wire(Vec(mshrEntryNum, UInt(mshrEntryNum.W)))

  val lineAddrList         = Wire(Vec(mshrEntryNum, UInt(lineAddrWidth.W)))
  val senderPermissionList = Wire(Vec(mshrEntryNum, UInt(TLPermissions.aWidth.W)))

  // connect mshr
  val mshrs = (0 until mshrEntryNum) map {
    i =>
      val mshr = Module(new MSHR(i))

      mshr.io.req         := Cat(io.fromProbe.valid, io.fromRefill.valid, allocateArb.io.in(i).ready)
      mshr.io.reqLineAddr := reqLineAddr
      mshr.io.reqType     := io.pipelineReq.bits.meta.rw_type
      mshr.io.isUpgrade   := io.pipelineReq.bits.isUpgrade

      lineAddrMatchList(i)    := mshr.io.lineAddrMatch
      lineAddrMatchIdxList(i) := Mux(mshr.io.lineAddrMatch, i.asUInt, 0.U)

      // probe signal
      probeBlockList(i)       := mshr.io.probeBlock
      mshr.io.probePermission := io.fromProbe.probePermission

      // replay & refill signal
      metaCounterList(i) := mshr.io.metaCounter

      mshr.io.replayFinish := replayFinishRespList(i)

      // allocate signal
      allocateArb.io.in(i).valid := mshr.io.isEmpty & !lineAddrMatch

      stallReqList(i)      := mshr.io.stallReq
      mshr.io.maskConflict := maskConflict

      // sender signal
      lineAddrList(i)         := mshr.io.senderLineAddr
      senderReqList(i)        := mshr.io.isEmpty & !lineAddrMatch & allocateArb.io.in(i).ready
      senderIdxList(i)        := Mux(senderReqList(i), i.asUInt, 0.U)
      senderPermissionList(i) := mshr.io.senderPermission
      mshr.io.senderResp      := senderRespList(i)
  }

  // Write Mask & Data Array
  def dataMergeGen(mask: UInt, data: UInt): UInt = {
    val byteData = data.asTypeOf(Vec(mshrDataWidth / 8, UInt(8.W)))
    var res      = Wire(Vec(mshrDataWidth / 8, UInt(8.W)))
    for (i <- 0 until (mshrDataWidth / 8))
      res(i) := Mux(mask(i), byteData(i), 0.U)
    res.asUInt
  }

  when(!probeReq && !replayReq && allocateReq && !stallReq && !maskConflict) {
    metaArray(lineAddrMatchIdx)(metaWriteIdx) := io.pipelineReq.bits.meta.asUInt
    maskArray(lineAddrMatchIdx)               := io.pipelineReq.bits.mask | maskArray(lineAddrMatchIdx)
    dataArray(lineAddrMatchIdx) := dataMergeGen(maskArray(lineAddrMatchIdx), dataArray(lineAddrMatchIdx)) |
      dataMergeGen(
        io.pipelineReq.bits.mask,
        io.pipelineReq.bits.data
      )
  }.elsewhen(io.replaceFinish) {
    maskArray(replayReg.io.replayIdx) := 0.U
  }

  // Resp To Cache Pipeline
  io.pipelineReq.ready     := !probeReq && !replayReq && !stallReq && !maskConflict
  allocateArb.io.out.ready := allocateReq

  // sender queue
  senderQueue.io.enq.valid := io.pipelineReq.valid && senderReqList.asUInt.orR
  senderQueue.io.enq.bits  := senderIdxList.reduce(_ | _)

  io.toL2Req.valid         := senderQueue.io.deq.valid
  senderQueue.io.deq.ready := io.toL2Req.ready
  io.toL2Req.bits.perm     := senderPermissionList(senderQueue.io.deq.bits)
  io.toL2Req.bits.entryId  := senderQueue.io.deq.bits
  io.toL2Req.bits.lineAddr := lineAddrList(senderQueue.io.deq.bits)

  senderRespList := Mux(
    io.toL2Req.ready,
    UIntToOH(senderQueue.io.deq.bits, mshrEntryNum),
    0.U
  ).asBools

  // refill queue wakeup MSHR FSM & replay reg
  replayReg.io.innerIO.valid         := io.fromRefill.valid
  io.fromRefill.ready                := replayReg.io.innerIO.ready
  replayReg.io.innerIO.bits.perm     := senderPermissionList(io.fromRefill.bits.entryId)
  replayReg.io.innerIO.bits.lineAddr := io.fromRefill.bits.lineAddr

  replayReg.io.innerIO.bits.entryIdx := io.fromRefill.bits.entryId
  replayReg.io.innerIO.bits.meta := metaArray(replayReg.io.replayIdx)(replayReg.io.idxMeta).asTypeOf(new ReqMetaBundle)

  replayReg.io.innerIO.bits.mask := maskArray(replayReg.io.replayIdx)

  replayReg.io.innerIO.bits.data := Mux(
    io.fromRefill.valid,
    Mux(
      senderPermissionList(io.fromRefill.bits.entryId) === TLPermissions.BtoT,
      dataArray(replayReg.io.replayIdx),
      io.fromRefill.bits.data
    ),
    dataArray(replayReg.io.replayIdx)
  )

  replayReg.io.innerIO.bits.entryIdx := senderIdxList.reduce(_ | _)
  replayReg.io.innerIO.bits.counter  := metaCounterList(lineAddrMatchIdx)

  io.toPipeline <> replayReg.io.toPipe
  io.toReplace <> replayReg.io.toReplace
  replayReg.io.replaceFinish := io.replaceFinish

  replayFinishRespList := Mux(
    io.replaceFinish,
    UIntToOH(replayReg.io.replayIdx),
    0.U
  ).asBools

}

/** For Test * */

object MSHRFile extends App {

  val firtoolOptions = Array(
    "--lowering-options=" + List(
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket"
    ).reduce(_ + "," + _)
  )

  ChiselStage.emitSystemVerilogFile(new MSHRFile, args, firtoolOptions)
}

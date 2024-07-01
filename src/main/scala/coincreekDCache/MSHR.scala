package coincreekDCache

import chisel3._
import chisel3.util._
import util._
import coincreekDCache.util._
import _root_.circt.stage.ChiselStage

class MSHR(id: Int) extends Module() {
  val io = IO(new MSHREntryIO)

  val mode_idle :: mode_req_enqueue :: mode_resp_wait :: mode_replay :: mode_clear :: Nil = Enum(5)
  private val state                                                                       = RegInit(mode_idle)

  // req priority
  val probeReq    = io.req(2)
  val replayReq   = !probeReq && io.req(1)
  val allocateReq = !io.req(2, 1).orR && io.req(0) && state === mode_idle

  // info regs & wires
  val sentPermission = RegInit(false.B) // 1 for write, 0 for read
  val tagReg         = RegEnable(io.reqTag, 0.U, allocateReq)

  val metaCounter = RegInit(0.U(log2Up(mshrEntryMetaNum).W))
  val dataCounter = RegInit(0.U(log2Up(mshrEntryDataNum).W))

  // match & output
  val tagMatch = tagReg === io.reqTag
  io.tagMatch := tagMatch

  /////////////// allocate state flag
  // current entry is sent as read, but here is a write req
  val privErr = tagMatch && (state === mode_resp_wait) && (!sentPermission) && io.reqType.asBool

  // don't have enough space to store current inst
  val isFull = tagMatch &&
    ((dataCounter + io.reqDataEntryNum) > mshrEntryDataNum.asUInt || (metaCounter + 1.U) > mshrEntryMetaNum.asUInt)

  val stallReq = (tagMatch && !(state <= mode_resp_wait)) || isFull || privErr
  io.stallReq := stallReq
  io.isEmpty  := state === mode_idle

  /////////////// refill state flag & io
  io.metaCounter := metaCounter
  io.dataCounter := dataCounter

  when(allocateReq || (tagMatch & !stallReq)) {
    metaCounter := metaCounter + 1.U
    when(io.reqType.asBool) {
      dataCounter := dataCounter + io.reqDataEntryNum
    }
  }

  /////////////// sent info
  sentPermission := Mux(
    state === mode_clear,
    false.B,
    Mux(allocateReq || (tagMatch && !stallReq), sentPermission | io.reqType, sentPermission),
  )

  // enqueue the sender
  io.senderPriv := sentPermission | Mux(tagMatch && !stallReq, io.reqType, false.B)
  io.senderTag  := tagReg

  // FSM
  state := MuxLookup(state, state)(
    Seq(
      mode_idle        -> Mux(allocateReq, mode_req_enqueue, state),
      mode_req_enqueue -> Mux(io.senderResp, mode_resp_wait, state),
      mode_resp_wait   -> Mux(tagMatch && replayReq, mode_replay, state),
      mode_replay      -> Mux(io.replayFinish, mode_clear, state),
      mode_clear       -> mode_idle,
    )
  )

}

class ReplayModule extends Module() {
  val io                = IO(new ReplayModuleIO())
  private val replayReg = RegInit(0.U(mshrDataBusWidth.W))

  val mode_idle :: mode_replay :: mode_wait_replace :: mode_clear :: Nil = Enum(4)
  private val state                                                      = RegInit(mode_idle)

  val metaCounter = RegInit(0.U(log2Up(mshrEntryMetaNum).W))
  val dataCounter = RegInit(0.U(log2Up(mshrEntryDataNum).W))

  val initEnable   = state === mode_idle && io.innerIO.valid
  val mshrEntryIdx = RegEnable(io.innerIO.bits.entryIdx, 0.U(log2Up(mshrEntryNum).W), initEnable)
  val totalCounter = RegEnable(io.innerIO.bits.counter, 0.U(log2Up(mshrEntryMetaNum).W), initEnable)
  val replayTag    = RegEnable(io.innerIO.bits.tag, 0.U(tagWidth.W), initEnable)
  val replayPriv   = RegEnable(io.innerIO.bits.priv, false.B, initEnable)

  val replayStall = RegInit(false.B)

  val loadgen =
    new LoadGen(
      io.innerIO.bits.meta.size,
      io.innerIO.bits.meta.signed,
      io.innerIO.bits.meta.addrIndex,
      replayReg,
      false.B,
      mshrDataBusWidth / 8,
    )

//  val load4Scalar = loadgen.genData(3)
//  val load4Vector = loadgen.genData(typMax)

  val storegen =
    new StoreGen(
      io.innerIO.bits.meta.size,
      io.innerIO.bits.meta.addrIndex,
      (new LoadGen(io.innerIO.bits.meta.size, false.B, dataCounter, io.innerIO.bits.data.asUInt, false.B, sizeMax))
        .genData(toInt(io.innerIO.bits.meta.size)),
      mshrDataBusWidth / 8,
    )

  val storeMask = storegen.mask
  val storeData = storegen.genData(toInt(io.innerIO.bits.meta.size))

  val mask4Reg: UInt = storeMask.asBools.foldLeft(0.U) {
    (res, mask) =>
      Cat(res, Fill(8, mask.asUInt))
  }

  when(!replayStall) {
    metaCounter := MuxLookup(state, metaCounter)(
      Seq(
        mode_clear  -> 0.U,
        mode_replay -> (metaCounter + 1.U),
      )
    )

    dataCounter := MuxLookup(state, dataCounter)(
      Seq(
        mode_clear -> 0.U,
        mode_replay -> Mux(
          io.innerIO.bits.meta.rwType.asBool,
          dataCounter + (1.U << io.innerIO.bits.meta.size),
          dataCounter,
        ),
      )
    )

    replayReg := MuxCase(
      replayReg,
      Seq(
        initEnable -> io.innerIO.bits.data.asTypeOf(replayReg),
        ((state === mode_replay) && io.innerIO.bits.meta.rwType.asBool) -> ((storeData & !mask4Reg) | (storeData & mask4Reg)),
      ),
    )

    state := MuxLookup(state, state)(
      Seq(
        mode_idle         -> Mux(io.innerIO.valid, mode_replay, state),
        mode_replay       -> Mux((metaCounter + 1.U) >= totalCounter, mode_wait_replace, state),
        mode_wait_replace -> Mux(io.replaceFinish, mode_clear, state),
        mode_clear        -> mode_idle,
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
    Mux(io.toReplace.ready, true.B, replaceSendFlag),
  )

  io.toReplace.valid     := state === mode_wait_replace && !replaceSendFlag
  io.toReplace.bits.priv := replayPriv
  io.toReplace.bits.tag  := replayTag
  io.toReplace.bits.data := replayReg

  // replay output
  io.toPipe.valid        := (state === mode_replay) && !io.innerIO.bits.meta.rwType
  replayStall            := io.toPipe.valid && !io.toPipe.ready
  io.toPipe.bits.regAddr := io.innerIO.bits.meta.regAddr
  io.toPipe.bits.regData := loadgen.genData(toInt(io.innerIO.bits.meta.size))

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

  val dataArray = RegInit(
    VecInit(
      Seq.fill(mshrEntryNum)(VecInit(Seq.fill(mshrEntryDataNum)(0.U(mshrEntryDataWidth.W))))
    )
  )

// general signal

  val tagMatchList    = Wire(Vec(mshrEntryNum, Bool()))
  val tagMatch        = tagMatchList.asUInt.orR
  val tagMatchIdxList = Wire(Vec(mshrEntryNum, UInt(log2Up(mshrEntryNum).W)))
  val tagMatchIdx     = tagMatchIdxList.reduce(_ | _)

  // interface for probe
  val probeReq = io.fromProbe.valid
  io.fromProbe.probeReady := probeReq && tagMatch

  // interface for replay
  val replayReq = io.fromRefill.valid
  io.fromRefill.ready := replayReg.io.innerIO.ready && !probeReq

  val metaCounterList = Wire(Vec(mshrEntryNum, UInt(log2Up(mshrEntryMetaNum).W)))
  val dataCounterList = Wire(Vec(mshrEntryNum, UInt(log2Up(mshrEntryDataNum).W)))

  val replayFinishRespList = Wire(Vec(mshrEntryNum, Bool()))

  // interface for allocate
  val allocateReq = io.pipelineReq.valid

  val allocateArb = Module(new Arbiter(Bool(), mshrEntryNum))
  allocateArb.io.in.foreach(_.bits := DontCare)

  val metaWriteIdx = metaCounterList(tagMatchIdx)
  val dataWriteIdx = dataCounterList(tagMatchIdx)

  val allocateDataEntryNum = Wire(UInt(3.W))

  allocateDataEntryNum :=
    Mux(io.pipelineReq.bits.meta.size < 3.U, 1.U, 1.U << (io.pipelineReq.bits.meta.size - 3.U)).asUInt

  val stallReqList = Wire(Vec(mshrEntryNum, Bool()))
  val stallReq     = stallReqList.asUInt.orR

  val reqTag = MuxCase(
    io.pipelineReq.bits.tag,
    Seq(
      probeReq                -> io.fromProbe.tag,
      (replayReq & !probeReq) -> io.fromRefill.bits.tag,
    ),
  )

  // interface for new entry sender
  val senderQueue    = Module(new Queue(UInt(log2Up(mshrEntryNum).W), mshrEntryNum))
  val senderReqList  = Wire(Vec(mshrEntryNum, Bool()))
  val senderRespList = Wire(Vec(mshrEntryNum, Bool()))
  val senderIdxList  = Wire(Vec(mshrEntryNum, UInt(mshrEntryDataNum.W)))

  val tagList        = Wire(Vec(mshrEntryNum, UInt(tagWidth.W)))
  val senderPrivList = Wire(Vec(mshrEntryNum, Bool()))

  // connect mshr
  val mshrs = (0 until mshrEntryNum) map {
    i =>
      val mshr = Module(new MSHR(i))

      mshr.io.req             := Cat(io.fromProbe.valid, io.fromRefill.valid, allocateArb.io.in(i).ready)
      mshr.io.reqTag          := reqTag
      mshr.io.reqType         := io.pipelineReq.bits.meta.rwType
      mshr.io.reqDataEntryNum := allocateDataEntryNum

      tagMatchList(i)    := mshr.io.tagMatch
      tagMatchIdxList(i) := Mux(mshr.io.tagMatch, i.asUInt, 0.U)

      // replay & refill signal
      metaCounterList(i) := mshr.io.metaCounter
      dataCounterList(i) := mshr.io.dataCounter

      mshr.io.replayFinish := replayFinishRespList(i)

      // allocate signal
      allocateArb.io.in(i).valid := mshr.io.isEmpty & !tagMatch

      stallReqList(i) := mshr.io.stallReq

      // sender signal
      tagList(i)         := mshr.io.senderTag
      senderReqList(i)   := mshr.io.isEmpty & !tagMatch & allocateArb.io.in(i).ready
      senderIdxList(i)   := Mux(senderReqList(i), i.asUInt, 0.U)
      senderPrivList(i)  := mshr.io.senderPriv
      mshr.io.senderResp := senderRespList(i)
  }

  // Write Mask & Data Array
  when(!probeReq && !replayReq && allocateReq && !stallReq) {
    metaArray(tagMatchIdx)(metaWriteIdx) := io.pipelineReq.bits.meta.asUInt
    val inData = io.pipelineReq.bits.data.asTypeOf(Vec(mshrEntryDataNum, UInt(mshrEntryDataWidth.W)))
    var j      = 0
    for (i <- toInt(dataWriteIdx) until mshrEntryDataNum)
      when(j.asUInt < allocateDataEntryNum) {
        dataArray(tagMatchIdx)(i) := inData(j)
        j = j + 1
      }
  }

  // Resp To Cache Pipeline
  io.pipelineReq.ready     := !probeReq && !replayReq && !stallReq
  allocateArb.io.out.ready := allocateReq

  // sender queue
  senderQueue.io.enq.valid := io.pipelineReq.valid && senderReqList.asUInt.orR
  senderQueue.io.enq.bits  := senderIdxList.reduce(_ | _)

  io.toL2Req.valid         := senderQueue.io.deq.valid
  senderQueue.io.deq.ready := io.toL2Req.ready
  io.toL2Req.bits.priv     := senderPrivList(senderQueue.io.deq.bits)
  io.toL2Req.bits.tag      := tagList(senderQueue.io.deq.bits)

  senderRespList := Mux(
    io.toL2Req.ready,
    UIntToOH(senderQueue.io.deq.bits, mshrEntryNum),
    0.U,
  ).asBools

  // refill queue wakeup MSHR FSM & replay reg
  replayReg.io.innerIO.valid     := io.fromRefill.valid
  io.fromRefill.ready            := replayReg.io.innerIO.ready
  replayReg.io.innerIO.bits.priv := senderPrivList(tagMatchIdx)
  replayReg.io.innerIO.bits.tag  := io.fromRefill.bits.tag

  replayReg.io.innerIO.bits.entryIdx := tagMatchIdx
  replayReg.io.innerIO.bits.meta := metaArray(replayReg.io.replayIdx)(replayReg.io.idxMeta).asTypeOf(new ReqMetaBundle)

  replayReg.io.innerIO.bits.data := Mux(
    io.fromRefill.valid,
    io.fromRefill.bits.data.asTypeOf(Vec(mshrEntryDataNum, UInt(mshrEntryDataWidth.W))),
    dataArray(replayReg.io.replayIdx),
  )

  replayReg.io.innerIO.bits.entryIdx := senderIdxList.reduce(_ | _)
  replayReg.io.innerIO.bits.counter  := metaCounterList(tagMatchIdx)

  io.toPipeline <> replayReg.io.toPipe
  io.toReplace <> replayReg.io.toReplace
  replayReg.io.replaceFinish := io.replaceFinish

  replayFinishRespList := Mux(
    io.replaceFinish,
    UIntToOH(replayReg.io.replayIdx),
    0.U,
  ).asBools

}

/** For Test * */

object MSHRFile extends App {

  val firtoolOptions = Array(
    "--lowering-options=" + List(
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket",
    ).reduce(_ + "," + _)
  )

  ChiselStage.emitSystemVerilogFile(new MSHRFile, args, firtoolOptions)
}

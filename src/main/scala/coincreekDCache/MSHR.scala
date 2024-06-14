package coincreekDCache

import chisel3._
import chisel3.util._
import util._

class MSHR(id: Int) extends Module() {
  val io = IO(new ioMSHR)

  private val mode_idle :: mode_req_enqueue :: mode_resp_wait :: mode_replay :: mode_clear :: Nil = Enum(5)
  val state                                                                                       = RegInit(mode_idle)

  // info regs & wires
  val sentPermission = Wire(UInt(1.W)) // 1 for write, 0 for read

  val isVector = RegInit(false.B)
  val typeList = RegInit(0.U(mshrEntryDataNum.W))
  val tagReg   = RegInit(0.U(tagWidth.W))

  val totalCounter = RegInit(0.U(log2Up(mshrEntryDataNum).W))

  // req priority
  val probeReq    = io.req(2)
  val replayReq   = !probeReq && io.req(1)
  val allocateReq = !io.req(2, 1).orR && io.req(0)

  // match & output
  val tagMatch = io.req.orR && (tagReg === io.reqTag)
  io.tagMatch := tagMatch

  /////////////// allocate state flag
  // current entry is sent as read, but here is a write req
  val privErr = tagMatch && (state > mode_req_enqueue) && (!sentPermission) && io.allocateType(0)
  // current entry is allocate as vector, but here is a scalar req
  val typeErr = tagMatch && (isVector =/= io.allocateType(1))
  // vector 9(mask+512data) & scalar write 2(mask+64data) & scalar read 1(mask)
  val reqDataNum = Mux(io.allocateType(1), 9.U, Mux(io.allocateType(0), 2.U, 1.U))
  // don't have enough space to store current inst
  val isFull = tagMatch && (totalCounter + reqDataNum) > mshrEntryDataNum.asUInt

  io.isFull  := isFull
  io.isEmpty := state === mode_idle
  io.privErr := privErr
  io.typeErr := typeErr

  /////////////// refill state flag & io
  io.counter  := totalCounter
  io.isVector := isVector
  io.typeList := typeList

  /////////////// probe state flag

  /////////////// sent info
  sentPermission := typeList.orR

  isVector := Mux(
    state === mode_clear,
    false.B,
    Mux(state === mode_idle && tagMatch, io.allocateType(1).asBool, isVector)
  )

  typeList := Mux(
    state === mode_clear,
    0.U,
    Mux(
      tagMatch && allocateReq && !isFull && state <= mode_req_enqueue,
      typeList | (Mux(io.allocateType(0), UIntToOH(totalCounter, mshrEntryDataNum), 0.U)),
      typeList
    )
  )

  tagReg := Mux(state === mode_idle, Mux(allocateReq, io.reqTag, 0.U), Mux(state === mode_clear, 0.U, tagReg))

  totalCounter := Mux(
    state === mode_clear,
    0.U,
    Mux(tagMatch && allocateReq && !isFull, totalCounter + reqDataNum, totalCounter)
  )

  // enqueue the sender
  io.senderPriv := sentPermission
  io.senderTag  := tagReg

  // FSM
  state := MuxLookup(state, state)(
    Seq(
      mode_idle        -> Mux(tagMatch && allocateReq, mode_req_enqueue, state),
      mode_req_enqueue -> Mux(io.senderResp, mode_resp_wait, state),
      mode_resp_wait   -> Mux(tagMatch && replayReq, mode_replay, state),
      mode_replay      -> Mux(io.replayFinish, mode_clear, state),
      mode_clear       -> mode_idle
    )
  )

}

// TODO: Finish replay logic && define how to access mshr entry mem(reg)
class replayReg extends Module() {
  val io = IO(new ioReplayReg())

  val replayReg = RegInit(0.U(mshrDataBusWidth.W))

  private val mode_idle :: mode_replay :: mode_wait_replace :: mode_clear :: Nil = Enum(4)
  val state                                                                      = RegInit(mode_idle)

  val replayCounter = RegInit(0.U(log2Up(mshrEntryDataNum).W))
  val totalCounter  = RegInit(0.U(log2Up(mshrEntryDataNum).W))

  val mshrEntryIdx = RegInit(0.U(log2Up(mshrEntryNum).W))
  val typeList     = RegInit(0.U(mshrEntryDataNum.W))
  val replayTag    = RegInit(0.U(tagWidth.W))
  val isVector     = RegInit(false.B)

  val reqDataNum = Mux(isVector, 9.U, Mux(typeList(replayCounter), 2.U, 1.U))

  replayCounter := MuxLookup(state, replayCounter)(
    Seq(
      mode_idle   -> 0.U,
      mode_replay -> (replayCounter + reqDataNum)
    )
  )

  mshrEntryIdx := Mux(state === mode_idle && io.innerIO.valid, io.innerIO.bits.entryIdx, mshrEntryIdx)
  totalCounter := Mux(state === mode_idle && io.innerIO.valid, io.innerIO.bits.counter, totalCounter)
  typeList     := Mux(state === mode_idle && io.innerIO.valid, io.innerIO.bits.typeList, typeList)
  isVector     := Mux(state === mode_idle && io.innerIO.valid, io.innerIO.bits.isVector, isVector)
  replayTag    := Mux(state === mode_idle && io.innerIO.valid, io.innerIO.bits.tag, replayTag)

//  val mask = Wire(UInt(mshrMaskBusWidth.W))
//  val data = Wire(UInt(mshrDataBusWidth.W))
//
//  mask := io.innerIO.bits.mask
//  mask := io.innerIO.bits.

  replayReg := Mux(
    state === mode_clear,
    0.U,
    Mux(
      (state === mode_idle) && io.innerIO.valid,
      io.innerIO.bits.data,
      replayReg
    )
  )

  state := MuxLookup(state, state)(
    Seq(
      mode_idle         -> Mux(io.innerIO.valid, mode_replay, state),
      mode_replay       -> Mux((replayCounter + reqDataNum) >= totalCounter, mode_wait_replace, state),
      mode_wait_replace -> Mux(io.replaceFinish, mode_clear, state),
      mode_clear        -> mode_idle
    )
  )

  // inner output
  io.innerIO.ready := state === mode_idle
  io.finishIdx     := mshrEntryIdx

  // replace signal connect
  val replaceSendFlag = RegInit(false.B)

  replaceSendFlag := Mux(
    state =/= mode_wait_replace,
    false.B,
    Mux(io.toReplace.ready, true.B, replaceSendFlag)
  )

  io.toReplace.valid     := state === mode_wait_replace && !replaceSendFlag
  io.toReplace.bits.tag  := io.innerIO.bits.tag
  io.toReplace.bits.data := replayReg

  // replay data connect
  io.replayFinish := (replayCounter + reqDataNum) >= totalCounter
}

class MSHRFile extends Module() {

  val io = IO(
    new Bundle {
      val pipelineReq = Flipped(DecoupledIO(new ioCachepipeMSHRFile))
      val toL2Req     = DecoupledIO(new ioMSHRL2)
      val fromRefill  = Flipped(DecoupledIO(new ioRefillMSHR))
      val fromProbe   = Flipped(DecoupledIO(new ioProbeMSHR)) // use ready as match signal
    }
  )

  val replayReg = Module(new replayReg)

  val array4MaskAndDatas = VecInit(
    Seq.fill(mshrEntryNum)(VecInit(Seq.fill(mshrEntryDataNum)(0.U(mshrEntryDataWidth.W))))
  )

  // general signal
  val reqTag = MuxCase(
    io.pipelineReq.bits.allocateTag,
    Seq(
      probeReq  -> io.fromProbe.bits.tag,
      replayReq -> io.fromRefill.bits
    )
  )

  val tagMatchList    = Wire(Vec(mshrEntryNum, Bool()))
  val tagMatchIdxList = Wire(Vec(mshrEntryNum, UInt(log2Up(mshrEntryNum).W)))
  val tagMatchIdx     = tagMatchIdxList.reduce(_ | _)

  // interface for probe
  val probeReq = io.fromProbe.valid
  io.fromProbe.ready := probeReq && tagMatchList.asUInt.orR

  // interface for replay
  val replayReq = io.fromRefill.valid
  io.fromRefill.ready := replayReg.io.innerIO.ready && !probeReq

  val typeListList = Wire(Vec(mshrEntryNum, UInt(mshrEntryDataNum.W)))
  val counterList  = Wire(Vec(mshrEntryNum, UInt(log2Up(mshrEntryDataNum).W)))
  val isVectorList = Wire(Vec(mshrEntryNum, Bool()))

  val replayFinishRespList = Wire(Vec(mshrEntryNum, Bool()))

  // interface for allocate
  val allocateReq = io.pipelineReq.valid
  io.pipelineReq.ready := !probeReq && !replayReq && tagMatchList.asUInt.orR

  val allocateArb = Module(new Arbiter(UInt(), mshrEntryNum))
  allocateArb.io.in.foreach(_.bits := DontCare)

  val arrayWriteIdx  = counterList(tagMatchIdx)
  val entryStallList = Wire(Vec(mshrEntryNum, Bool()))
  val needStall      = (tagMatchList.asUInt & entryStallList.asUInt).orR

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

      mshr.io.req    := Cat(io.fromProbe.valid, io.fromRefill.valid, allocateArb.io.in(i).ready)
      mshr.io.reqTag := reqTag

      tagMatchList(i)    := mshr.io.tagMatch
      tagMatchIdxList(i) := Mux(mshr.io.tagMatch, i.asUInt, 0.U)

      // replay & refill signal
      typeListList(i) := mshr.io.typeList
      counterList(i)  := mshr.io.counter
      isVectorList(i) := mshr.io.isVector

      mshr.io.replayFinish := replayFinishRespList(i)

      // allocate signal
      allocateArb.io.in(i).valid := mshr.io.isEmpty

      entryStallList(i) := mshr.io.isFull | mshr.io.privErr | mshr.io.typeErr

      // sender signal
      tagList(i)         := mshr.io.senderTag
      senderReqList(i)   := mshr.io.tagMatch && mshr.io.isEmpty
      senderIdxList(i)   := Mux(senderReqList(i), i.asUInt, 0.U)
      senderPrivList(i)  := mshr.io.senderPriv
      mshr.io.senderResp := senderRespList(i)
  }

  // Write Mask & Data Array
  when(!probeReq && !replayReq && allocateReq && !needStall) {
    val data2Vec = io.pipelineReq.bits.allocateData.asTypeOf(Vec(mshrEntryDataNum, UInt(mshrEntryDataWidth.W)))
    for (i <- 0 until mshrEntryDataNum)
      when(io.pipelineReq.bits.allocateType(1)) {
        array4MaskAndDatas(tagMatchIdx)(i) := data2Vec(i)
      }.elsewhen(io.pipelineReq.bits.allocateType(0)) {
        when(i.asUInt === arrayWriteIdx) {
          array4MaskAndDatas(tagMatchIdx)(i) := io.pipelineReq.bits.allocateMask
        }.elsewhen(i.asUInt === arrayWriteIdx + 1.U) {
          array4MaskAndDatas(tagMatchIdx)(i) := data2Vec(0)
        }
      }.otherwise {
        when(i.asUInt === arrayWriteIdx) {
          array4MaskAndDatas(tagMatchIdx)(i) := io.pipelineReq.bits.allocateMask
        }
      }
  }

  // sender queue
  senderQueue.io.enq.valid := io.pipelineReq.valid && senderReqList.asUInt.orR
  senderQueue.io.enq.bits  := senderIdxList.reduce(_ | _)

  senderQueue.io.deq.ready := io.toL2Req.ready
  io.toL2Req.bits.priv     := senderPrivList(senderQueue.io.deq.bits)
  io.toL2Req.bits.tag      := tagList(senderQueue.io.deq.bits)
  senderRespList           := UIntToOH(senderQueue.io.deq.bits, mshrEntryNum)

  // refill queue wakeup MSHR FSM & replay reg
  replayReg.io.innerIO.bits.tag  := io.fromRefill.bits.tag
  replayReg.io.innerIO.bits.data := array4MaskAndDatas(tagMatchIdx)

  replayReg.io.innerIO.bits.typeList := typeListList(tagMatchIdx)
  replayReg.io.innerIO.bits.counter  := counterList(tagMatchIdx)
  replayReg.io.innerIO.bits.isVector := isVectorList(tagMatchIdx)

  replayFinishRespList := UIntToOH(replayReg.io.finishIdx).asTypeOf(replayFinishRespList)

  // probe req

}

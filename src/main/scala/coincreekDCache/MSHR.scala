package coincreekDCache

import chisel3._
import chisel3.util._
import util._

class ioMSHR extends Bundle() {
  // mshr entry search & add new request inst
  val allocateReq  = Input(Bool())
  val allocateType = Input(UInt(mshrTypeTag.W))
  val tag          = Input(UInt(tagWidth.W))

  val tagMatch = Output(Bool())                           // mshr allocate match
  val isEmpty  = Output(Bool())
  val isFull   = Output(Bool())                           // mshr allocate full -> need stall outside
  val typeErr  = Output(Bool())
  val privErr  = Output(Bool())
  val wrLine   = Output(UInt(log2Up(mshrEntryDataNum).W)) // new inst data write to which line of current mshr entry

  // mshr sender port
  val senderResp = Input(Bool())  // permitted to send this entry now
  val senderPriv = Output(Bool()) // 1 for write, 0 for read
  val senderTag  = Output(UInt(tagWidth.W))
}

class MSHR(id: Int) extends Module() {
  val io = IO(new ioMSHR)

  val mode_idle :: mode_req_enqueue :: mode_resp_wait :: mode_replay :: mode_refill :: Nil = Enum(5)
  val state                                                                                = RegInit(mode_idle)

  // info regs & wires
  val sendedPermission = Wire(UInt(1.W)) // 1 for write, 0 for read

  val isVector = RegInit(false.B)
  val typeList = RegInit(0.U(mshrEntryDataNum.W))
  val tagReg   = RegInit(0.U(tagWidth.W))

  val totalCounter = RegInit(0.U(log2Up(mshrEntryDataNum).W))

  // match & output
  val tagMatch = io.allocateReq && (tagReg === io.tag)
  val privErr  = tagMatch && (state > mode_req_enqueue) && (!sendedPermission) && io.allocateType(0)
  val typeErr  = tagMatch && (isVector =/= io.allocateType(1))
  // vector 9(mask+512data) & scalar write 2(mask+64data) & scalar read 1(mask)
  val reqDataNum = Mux(io.allocateType(1), 9.U, Mux(io.allocateType(0), 2.U, 1.U))
  val isFull     = io.allocateReq && (totalCounter + reqDataNum) > mshrEntryDataNum.asUInt
  io.tagMatch := tagMatch
  io.isFull   := isFull
  io.isEmpty  := state === mode_idle
  io.privErr  := privErr
  io.typeErr  := typeErr

  io.wrLine := Mux(io.allocateReq, totalCounter, 0.U)

  // info regs & wires update
  sendedPermission := typeList.orR

  isVector := Mux(
    state === mode_refill,
    false.B,
    Mux(state === mode_idle && tagMatch, io.allocateType(1).asBool, isVector)
  )

  typeList := Mux(
    state === mode_refill,
    0.U,
    Mux(
      tagMatch && !isFull && state <= mode_req_enqueue,
      typeList | (Mux(io.allocateType(0), UIntToOH(totalCounter, mshrEntryDataNum), 0.U)),
      typeList
    )
  )

  tagReg       := Mux(state === mode_idle, Mux(tagMatch, io.tag, 0.U), tagReg)
  totalCounter := Mux(state === mode_refill, 0.U, Mux(tagMatch && !isFull, totalCounter + 1.U, totalCounter))

  // enqueue the sender
  io.senderPriv := sendedPermission
  io.senderTag  := tagReg

  // FSM
  state := MuxLookup(state, state)(
    Seq(
      mode_idle        -> Mux(tagMatch, mode_req_enqueue, state),
      mode_req_enqueue -> Mux(io.senderResp, mode_resp_wait, state)
//      mode_resp_wait   -> Mux()
    )
  )

}

class ioCachepipeMSHRFile extends Bundle() {
  val allocateTag  = UInt(tagWidth.W)
  val allocateType = UInt(mshrTypeTag.W)
  val allocateMask = UInt(mshrMaskBusWidth.W)
  val allocateData = UInt(mshrDataBusWidth.W)
}

class ioMSHRL2 extends Bundle() {
  val priv = Bool()
  val tag  = UInt(tagWidth.W)
}

class MSHRFile extends Module() {

  val io = IO(
    new Bundle {
      val pipelineReq = Flipped(Decoupled(new ioCachepipeMSHRFile))
      val toL2Req     = Decoupled(new ioMSHRL2)
    }
  )

  val array4MaskAndDatas = VecInit(
    Seq.fill(mshrEntryNum)(VecInit(Seq.fill(mshrEntryDataNum)(0.U(mshrEntryDataWidth.W))))
  )

  val senderQueue    = Module(new Queue(UInt(log2Up(mshrEntryNum).W), mshrEntryNum))
  val senderReqList  = Wire(Vec(mshrEntryNum, Bool()))
  val senderRespList = Wire(Vec(mshrEntryNum, Bool()))
  val senderIdxList  = Wire(Vec(mshrEntryNum, UInt(mshrEntryDataNum.W)))

  val allocateArb = Module(new Arbiter(UInt(), mshrEntryNum))
  allocateArb.io.in.foreach(_.bits := DontCare)

  val tagMatchList      = Wire(Vec(mshrEntryNum, Bool()))
  val entryFullList     = Wire(Vec(mshrEntryNum, Bool()))
  val entryStallList    = Wire(Vec(mshrEntryNum, Bool()))
  val entryChooseList   = Wire(Vec(mshrEntryNum, Bool()))
  val arrayWriteIdxList = Wire(Vec(mshrEntryNum, UInt(mshrEntryDataNum.W)))

  val needStall     = (tagMatchList.asUInt & entryFullList.asUInt).orR
  val choosedEntry  = OHToUInt(entryChooseList.asUInt, log2Up(mshrEntryNum))
  val arrayWriteIdx = arrayWriteIdxList.reduce(_ | _)

  val tagList        = Wire(Vec(mshrEntryNum, UInt(tagWidth.W)))
  val senderPrivList = Wire(Vec(mshrEntryNum, Bool()))

  val mshrs = (0 until mshrEntryNum) map {
    i =>
      val mshr = Module(new MSHR(i))

      allocateArb.io.in(i).valid := mshr.io.isEmpty
      mshr.io.allocateReq        := allocateArb.io.in(i).ready
      entryChooseList(i)         := Mux(allocateArb.io.in(i).ready, true.B, false.B)

      tagMatchList(i)      := mshr.io.tagMatch
      entryFullList(i)     := mshr.io.isFull | mshr.io.privErr | mshr.io.typeErr
      arrayWriteIdxList(i) := mshr.io.wrLine

      tagList(i)         := mshr.io.senderTag
      senderReqList(i)   := mshr.io.tagMatch && mshr.io.isEmpty
      senderIdxList(i)   := Mux(senderReqList(i), i.asUInt, 0.U)
      senderPrivList(i)  := mshr.io.senderPriv
      mshr.io.senderResp := senderRespList(i)
  }

  // Write Mask & Data SRAM
  when(io.pipelineReq.valid && (!needStall)) {
    val data2Vec = io.pipelineReq.bits.allocateData.asTypeOf(Vec(mshrEntryDataNum, UInt(mshrEntryDataWidth.W)))
    for (i <- 0 until mshrEntryDataNum)
      when(io.pipelineReq.bits.allocateType(1)) {
        array4MaskAndDatas(choosedEntry)(i) := data2Vec(i)
      }.elsewhen(io.pipelineReq.bits.allocateType(0)) {
        when(i.asUInt === arrayWriteIdx) {
          array4MaskAndDatas(choosedEntry)(i) := io.pipelineReq.bits.allocateMask
        }.elsewhen(i.asUInt === arrayWriteIdx + 1.U) {
          array4MaskAndDatas(choosedEntry)(i) := data2Vec(0)
        }
      }.otherwise {
        when(i.asUInt === arrayWriteIdx) {
          array4MaskAndDatas(choosedEntry)(i) := io.pipelineReq.bits.allocateMask
        }
      }
  }

  // sender queue
  senderQueue.io.enq.valid := io.pipelineReq.valid && senderReqList.asUInt.orR
  senderQueue.io.enq.bits  := senderIdxList.reduce(_ | _)

  senderQueue.io.deq.ready := io.toL2Req.valid
  io.toL2Req.bits.priv     := senderPrivList(senderQueue.io.deq.bits)
  io.toL2Req.bits.tag      := tagList(senderQueue.io.deq.bits)
  senderRespList           := UIntToOH(senderQueue.io.deq.bits, mshrEntryNum)

}

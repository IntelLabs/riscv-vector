package coincreekDCache

import chisel3._
import chisel3.util._
import util._

/////// MSHR Entry IO
class ioMSHR extends Bundle() {
  val req    = Input(UInt(mshrReqType.W))
  val reqTag = Input(UInt(tagWidth.W))

  val tagMatch = Output(Bool()) // mshr inner tag match

  // replay & refill signals
  val replayFinish = Input(Bool())
  val counter      = Output(UInt(log2Up(mshrEntryDataNum).W))
  val typeList     = Output(UInt(mshrEntryDataNum.W))
  val isVector     = Output(Bool())

  // mshr entry search & add new request inst
  val allocateType = Input(UInt(mshrTypeTag.W))

  // state flag for allocate
  val isEmpty = Output(Bool())
  val isFull  = Output(Bool()) // mshr allocate full -> need stall outside
  val typeErr = Output(Bool())
  val privErr = Output(Bool())

  // mshr sender port
  val senderResp = Input(Bool())  // permitted to send this entry now
  val senderPriv = Output(Bool()) // 1 for write, 0 for read
  val senderTag  = Output(UInt(tagWidth.W))

}

/////// replay reg IOs
class ioMSHRReplay extends Bundle() {
  val regAddr = UInt(regAddrWidth.W)
  val regData = UInt(regDataWidth.W)
}

class ioMSHRReplace extends Bundle() {
  val tag  = UInt(tagWidth.W)
  val data = UInt(regDataWidth.W)
}

class ioMSHRInner extends Bundle() {
  val tag  = UInt(tagWidth.W)
  val data = Vec(mshrEntryNum, UInt(mshrEntryDataWidth.W))

  val entryIdx = UInt(log2Up(mshrEntryNum).W)
  val counter  = UInt(log2Up(mshrEntryDataNum).W)
  val typeList = UInt(mshrEntryDataNum.W)
  val isVector = Bool()
}

class ioReplayReg extends Bundle() {
  val toReplay = DecoupledIO(new ioMSHRReplay())

  val toReplace     = DecoupledIO(new ioMSHRReplace())
  val replaceFinish = Input(Bool())

  val innerIO = Flipped(DecoupledIO(new ioMSHRInner()))

  val replayFinish = Output(Bool())
  val finishIdx    = Output(UInt(log2Up(mshrEntryNum).W))
}

/////// MSHR file IO

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

class ioRefillMSHR extends Bundle() {
  val tag  = UInt(tagWidth.W)
  val data = UInt(mshrDataBusWidth.W)
}

class ioProbeMSHR extends Bundle() {
  val tag = UInt(tagWidth.W)
}

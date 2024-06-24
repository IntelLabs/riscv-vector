package coincreekDCache

import chisel3._
import chisel3.util._
import util._

/////// MSHR Entry IO
class MSHREntryIO extends Bundle() {
  val req             = Input(UInt(mshrReqType.W))
  val reqType         = Input(UInt(mshrType.W))
  val reqTag          = Input(UInt(tagWidth.W))
  val reqDataEntryNum = Input(UInt(log2Up(mshrEntryDataNum).W))

  val tagMatch = Output(Bool()) // mshr inner tag match

  // replay & refill signals
  val replayFinish = Input(Bool())
  val metaCounter  = Output(UInt(log2Up(mshrEntryMetaNum).W))
  val dataCounter  = Output(UInt(log2Up(mshrEntryDataNum).W))

  // state flag for allocate
  val isEmpty  = Output(Bool())
  val stallReq = Output(Bool())

  // mshr sender port
  val senderResp = Input(Bool())  // permitted to send this entry now
  val senderPriv = Output(Bool()) // 1 for write, 0 for read
  val senderTag  = Output(UInt(tagWidth.W))

}

/////// replay reg IOs
class MSHRPipeReplay extends Bundle() {
  val regAddr = UInt(regAddrWidth.W)
  val regData = UInt(regDataWidth.W)
}

class MSHRReplace extends Bundle() {
  val tag  = UInt(tagWidth.W)
  val data = UInt(regDataWidth.W)
}

class MSHRInner extends Bundle() {
  val tag = UInt(tagWidth.W)

  val meta = new ReqMetaBundle()
  val data = Vec(mshrEntryDataNum, UInt(mshrEntryDataWidth.W))

  val entryIdx = UInt(log2Up(mshrEntryNum).W)
  val counter  = UInt(log2Up(mshrEntryMetaNum).W)
}

class ReplayModuleIO extends Bundle() {
  val toPipe = DecoupledIO(new MSHRPipeReplay())

  val toReplace     = DecoupledIO(new MSHRReplace())
  val replaceFinish = Input(Bool())

  val innerIO = Flipped(DecoupledIO(new MSHRInner()))

  val idxMeta   = Output(UInt(log2Up(mshrEntryMetaNum).W))
  val replayIdx = Output(UInt(log2Up(mshrEntryNum).W))
}

/////// MSHR file IO
class ReqMetaBundle extends Bundle() {
  val rw_type   = UInt(1.W)
  val regAddr   = UInt(regAddrWidth.W)
  val typ       = UInt(typWidth.W)
  val signed    = Bool()
  val addrIndex = UInt(dataIndexWidth.W)
}

class CachepipeMSHRFile extends Bundle() {
  val tag  = UInt(tagWidth.W)
  val meta = new ReqMetaBundle
  val data = UInt(mshrDataBusWidth.W)
}

class MSHRFileL2 extends Bundle() {
  val priv = Bool()
  val tag  = UInt(tagWidth.W)
}

class RefillMSHRFile extends Bundle() {
  val tag  = UInt(tagWidth.W)
  val data = UInt(mshrDataBusWidth.W)
}

class ProbeMSHRFile extends Bundle() {
  val tag = UInt(tagWidth.W)
}

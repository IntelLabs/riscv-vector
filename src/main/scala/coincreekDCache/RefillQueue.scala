package coincreekDCache

import chisel3._
import chisel3.util._
import util._
import freechips.rocketchip.tilelink._
import coincreekDCache.util._
import _root_.circt.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters

class L2TLD extends Bundle() {
  val opcode = UInt(3.W)
  val source = UInt(log2Up(mshrEntryNum).W)
  val data   = UInt(beatBits.W)
}

class TLDInterface extends Module {
  val io = IO(new Bundle {
    val fromL2   = Flipped(DecoupledIO(new L2TLD))
    val toRefill = DecoupledIO(new L2Refill())
  })

  val data = RegInit(VecInit(Seq.fill(refillCycles)(0.U(beatBits.W))))

  val counter   = RegInit(0.U(log2Up(refillCycles).W))
  val writeFlag = RegInit(false.B)

  val inValid = io.fromL2.valid && io.toRefill.ready && !writeFlag && io.fromL2.bits.opcode === TLMessages.GrantData
  when(inValid) {
    counter       := Mux(counter + 1.U >= refillCycles.asUInt, 0.U, counter + 1.U)
    data(counter) := io.fromL2.bits.data
  }

  writeFlag := Mux(writeFlag, false.B, Mux(counter === refillCycles.asUInt - 1.U, true.B, writeFlag))

  io.fromL2.ready          := io.toRefill.ready && !writeFlag
  io.toRefill.valid        := writeFlag | io.fromL2.bits.opcode === TLMessages.Grant
  io.toRefill.bits.hasData := Mux(io.fromL2.bits.opcode === TLMessages.Grant, false.B, true.B)
  io.toRefill.bits.data    := data.asUInt
  io.toRefill.bits.entryId := Mux(
    io.fromL2.bits.opcode === TLMessages.Grant,
    io.fromL2.bits.source,
    RegEnable(io.fromL2.bits.source, counter === refillCycles.asUInt - 1.U),
  )
}

class RefillQueue extends Module {
  val io = IO(new Bundle {
    val fromL2 = Flipped(DecoupledIO(new L2Refill))
    val toCore = DecoupledIO(new RefillMSHRFile)
  })

  val dataQueue   = Module(new Queue(new L2Refill, refillDataQueueNum))
  val noDataQueue = Module(new Queue(UInt(log2Up(mshrEntryNum).W), refillNoDataQueueNum))

  dataQueue.io.enq.valid   := io.fromL2.valid && io.fromL2.bits.hasData
  dataQueue.io.enq.bits    := io.fromL2.bits
  noDataQueue.io.enq.valid := io.fromL2.valid && !io.fromL2.bits.hasData
  noDataQueue.io.enq.bits  := io.fromL2.bits.entryId
  io.fromL2.ready          := dataQueue.io.enq.ready && noDataQueue.io.enq.ready

  val priv = RegInit(false.B) // false for data, true for no data

  io.toCore.valid := noDataQueue.io.deq.valid | dataQueue.io.deq.valid
  io.toCore.bits.entryId := Mux(
    priv && noDataQueue.io.deq.valid,
    noDataQueue.io.deq.bits,
    dataQueue.io.deq.bits.entryId,
  )
  io.toCore.bits.data := dataQueue.io.deq.bits.data

  dataQueue.io.deq.ready := io.toCore.ready && dataQueue.io.deq.valid && (!priv || (priv && !noDataQueue.io.deq.valid))
  noDataQueue.io.deq.ready := io.toCore.ready && noDataQueue.io.deq.valid && (priv || (!priv && dataQueue.io.deq.valid))

}

//class RefillTest
////    implicit edge: TLEdgeIn
//    extends Module {
//  val io = IO(new Bundle {
//    val fromL2 = Flipped(DecoupledIO(new L2TLD))
//    val toCore = DecoupledIO(new RefillMSHRFile)
//  })
//
//  val interface = Module(new TLDInterface())
//  val refill    = Module(new RefillQueue)
//
//  interface.io.fromL2 <> io.fromL2
//  io.toCore <> refill.io.toCore
//  refill.io.fromL2 <> interface.io.toRefill
//}
//
//object RefillTest extends App {
//
//  val firtoolOptions = Array(
//    "--lowering-options=" + List(
//      "disallowLocalVariables",
//      "disallowPackedArrays",
//      "locationInfoStyle=wrapInAtSquareBracket",
//    ).reduce(_ + "," + _)
//  )
//
//  ChiselStage.emitSystemVerilogFile(
//    new RefillTest,
//    args,
//    firtoolOptions ++ Array("--disable-all-randomization"),
//  )
//}

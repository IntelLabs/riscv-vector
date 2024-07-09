package coincreekDCache

import chisel3._
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

  io.fromL2.ready             := io.toRefill.ready && !writeFlag
  io.toRefill.bits.probeMatch := DontCare
  io.toRefill.valid           := writeFlag | io.fromL2.bits.opcode === TLMessages.Grant
  io.toRefill.bits.hasData    := Mux(io.fromL2.bits.opcode === TLMessages.Grant, false.B, true.B)
  io.toRefill.bits.data       := data.asUInt
  io.toRefill.bits.entryId := Mux(
    io.fromL2.bits.opcode === TLMessages.Grant,
    io.fromL2.bits.source,
    RegEnable(io.fromL2.bits.source, counter === refillCycles.asUInt - 1.U),
  )
}

class RefillQueue extends Module {
  val io = IO(new Bundle {
    val fromL2    = Flipped(DecoupledIO(new L2Refill))
    val fromProbe = Flipped(ValidIO(new ProbeRefill))
    val toCore    = DecoupledIO(new RefillMSHRFile)
  })

  val dataIdxQueue = Module(new SearchableQueue(UInt(log2Up(mshrEntryNum).W), refillDataQueueNum))
  val dataQueue    = Module(new Queue(UInt(mshrDataWidth.W), refillDataQueueNum))

  val noDataQueue = Module(new SearchableQueue(UInt(log2Up(mshrEntryNum).W), refillNoDataQueueNum))

//  dataQueue.equals()

  dataIdxQueue.io.enq.valid := io.fromL2.valid && io.fromL2.bits.hasData
  dataIdxQueue.io.enq.bits  := io.fromL2.bits.entryId
  dataQueue.io.enq.valid    := io.fromL2.valid && io.fromL2.bits.hasData
  dataQueue.io.enq.bits     := io.fromL2.bits.data

  noDataQueue.io.enq.valid := io.fromL2.valid && !io.fromL2.bits.hasData
  noDataQueue.io.enq.bits  := io.fromL2.bits.entryId

  io.fromL2.ready := dataQueue.io.enq.ready && noDataQueue.io.enq.ready

  val priv = RegInit(false.B) // false for data, true for no data

  io.toCore.valid := noDataQueue.io.deq.valid | dataQueue.io.deq.valid
  io.toCore.bits.entryId := Mux(
    priv && noDataQueue.io.deq.valid,
    noDataQueue.io.deq.bits,
    dataIdxQueue.io.deq.bits,
  )
  io.toCore.bits.data := dataQueue.io.deq.bits

  dataIdxQueue.io.searchIdx := io.fromProbe.bits.entryId
  noDataQueue.io.searchIdx  := io.fromProbe.bits.entryId
  io.toCore.bits.probeMatch := io.fromProbe.valid && (dataIdxQueue.io.idxMatch || noDataQueue.io.idxMatch)

  dataQueue.io.deq.ready := io.toCore.ready && dataQueue.io.deq.valid && (!priv || (priv && !noDataQueue.io.deq.valid))
  dataIdxQueue.io.deq.ready := io.toCore.ready && dataQueue.io.deq.valid && (!priv || (priv && !noDataQueue.io.deq.valid))
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

class SearchableQueue[T <: Data](val gen: T, val entries: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)

    val searchIdx = Input(gen)
    val idxMatch  = Output(Bool())
  })

  val queue      = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen))))
  val enq_ptr    = Counter(entries)
  val deq_ptr    = Counter(entries)
  val maybe_full = RegInit(false.B)
  val ptr_match  = enq_ptr.value === deq_ptr.value
  val empty      = ptr_match && !maybe_full
  val full       = ptr_match && maybe_full
  val do_enq     = WireDefault(io.enq.fire)
  val do_deq     = WireDefault(io.deq.fire)

  when(do_enq) {
    queue(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }
  when(do_deq) {
    deq_ptr.inc()
  }
  when(do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  io.deq.valid := !empty
  io.enq.ready := !full

  io.deq.bits := queue(deq_ptr.value)

  def search(gen: T): Bool =
    queue.foldLeft(false.B)((m, n) => m | n === gen)
  io.idxMatch := search(io.searchIdx)
}

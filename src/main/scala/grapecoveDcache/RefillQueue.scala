package grapecoveDCache

import chisel3._
import util._
import freechips.rocketchip.tilelink._
import _root_.circt.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters

class RefillQueueWrapper(
    implicit edge: TLEdgeOut,
    p:             Parameters,
) extends Module {
  val io = IO(new Bundle {
    val memGrant   = Flipped(Decoupled(new TLBundleD(edge.bundle)))
    val memFinish  = Decoupled(new TLBundleE(edge.bundle))
    val probeCheck = Flipped(ValidIO(new ProbeRefill))
    val refillResp = DecoupledIO(new RefillMSHRFile)
  })

  val refillQueue = Module(new RefillQueue)

  val s_receive_grant :: s_send_refill :: Nil = Enum(2)

  val state = RegInit(s_receive_grant)

  val dataReg    = RegInit(VecInit(Seq.fill(refillCycles)(0.U(beatBits.W))))
  val hasDataReg = RegEnable(io.memGrant.bits.opcode =/= TLMessages.Grant, io.memGrant.fire)
  val sourceReg  = RegEnable(io.memGrant.bits.source, io.memGrant.fire)

  val allBeatDone = edge.last(io.memGrant) && io.memGrant.fire
  val counter     = RegInit(0.U(log2Up(refillCycles).W))

  switch(state) {
    is(s_receive_grant) {
      when(allBeatDone) {
        state := s_send_refill
      }
    }
    is(s_send_refill) {
      when(refillQueue.io.memRefill.fire) {
        state := s_receive_grant
      }

    }
  }

  when(io.memGrant.fire && io.memGrant.bits.opcode === TLMessages.GrantData) {
    counter          := Mux(counter + 1.U >= refillCycles.asUInt, 0.U, counter + 1.U)
    dataReg(counter) := io.memGrant.bits.data
  }

  refillQueue.io.memRefill.valid           := (state === s_send_refill)
  refillQueue.io.memRefill.bits.hasData    := hasDataReg
  refillQueue.io.memRefill.bits.data       := dataReg.asUInt
  refillQueue.io.memRefill.bits.entryId    := sourceReg
  refillQueue.io.memRefill.bits.probeMatch := DontCare

  refillQueue.io.probeCheck <> io.probeCheck
  io.refillResp <> refillQueue.io.refillResp

  val grantAckQueue = Module(new Queue(new TLBundleE(edge.bundle), 1))

  grantAckQueue.io.enq.valid := ((state === s_receive_grant) & allBeatDone)
  grantAckQueue.io.enq.bits  := edge.GrantAck(io.memGrant.bits)

  io.memFinish <> grantAckQueue.io.deq

  io.memGrant.ready := (state === s_receive_grant) && grantAckQueue.io.enq.ready

  assert(
    !(io.memGrant.fire &&
      io.memGrant.bits.opcode =/= TLMessages.GrantData &&
      io.memGrant.bits.opcode =/= TLMessages.Grant)
  )
}

class RefillQueue extends Module {
  val io = IO(new Bundle {
    val memRefill  = Flipped(DecoupledIO(new RefillMSHRFile))
    val probeCheck = Flipped(ValidIO(new ProbeRefill))
    val refillResp = DecoupledIO(new RefillMSHRFile)
  })

  val dataIdxQueue = Module(new SearchableQueue(UInt(log2Up(nMSHRs).W), nRefillQDataEntries))
  val dataQueue    = Module(new Queue(UInt(blockBits.W), nRefillQDataEntries))

  val permQueue = Module(new SearchableQueue(UInt(log2Up(nMSHRs).W), nRefillQPermEntries))

  // enq
  dataIdxQueue.io.enq.valid := io.memRefill.valid && io.memRefill.bits.hasData
  dataIdxQueue.io.enq.bits  := io.memRefill.bits.entryId

  dataQueue.io.enq.valid := io.memRefill.valid && io.memRefill.bits.hasData
  dataQueue.io.enq.bits  := io.memRefill.bits.data

  permQueue.io.enq.valid := io.memRefill.valid && !io.memRefill.bits.hasData
  permQueue.io.enq.bits  := io.memRefill.bits.entryId

  io.memRefill.ready := dataQueue.io.enq.ready && permQueue.io.enq.ready

  // deq arb
  val queueArb = Module(new Arbiter(UInt(log2Up(nMSHRs).W), 2))
  queueArb.io.in(0) <> permQueue.io.deq
  queueArb.io.in(1) <> dataIdxQueue.io.deq
  queueArb.io.out.ready := io.refillResp.ready

  permQueue.io.deq.ready    := queueArb.io.in(0).ready
  dataIdxQueue.io.deq.ready := queueArb.io.in(1).ready
  dataQueue.io.deq.ready    := queueArb.io.in(1).ready

  io.refillResp.valid        := queueArb.io.out.valid
  io.refillResp.bits.entryId := queueArb.io.out.bits
  io.refillResp.bits.data    := dataQueue.io.deq.bits

  // probe search
  dataIdxQueue.io.searchIdx     := io.probeCheck.bits.entryId
  permQueue.io.searchIdx        := io.probeCheck.bits.entryId
  io.refillResp.bits.probeMatch := io.probeCheck.valid && (dataIdxQueue.io.idxMatch || permQueue.io.idxMatch)
}

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

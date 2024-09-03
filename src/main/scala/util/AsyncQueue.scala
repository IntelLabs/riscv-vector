// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

case class AsyncQueueParams(
  depth:  Int     = 8,
  sync:   Int     = 3,
  safe:   Boolean = true,
// If safe is true, then effort is made to resynchronize the crossing indices when either side is reset.
// This makes it safe/possible to reset one side of the crossing (but not the other) when the queue is empty.
  narrow: Boolean = false)
// If narrow is true then the read mux is moved to the source side of the crossing.
// This reduces the number of level shifters in the case where the clock crossing is also a voltage crossing,
// at the expense of a combinational path from the sink to the source and back to the sink.
{
  require (depth > 0 && isPow2(depth))
  require (sync >= 2)

  val bits = log2Ceil(depth)
  val wires = if (narrow) 1 else depth
}

object AsyncQueueParams {
  // When there is only one entry, we don't need narrow.
  def singleton(sync: Int = 3, safe: Boolean = true) = AsyncQueueParams(1, sync, safe, false)
}

class AsyncBundleSafety extends Bundle {
  val ridx_valid     = Input (Bool())
  val widx_valid     = Output(Bool())
  val source_reset_n = Output(Bool())
  val sink_reset_n   = Input (Bool())
}

class AsyncBundle[T <: Data](private val gen: T, val params: AsyncQueueParams = AsyncQueueParams()) extends Bundle {
  // Data-path synchronization
  val mem   = Output(Vec(params.wires, gen))
  val ridx  = Input (UInt((params.bits+1).W))
  val widx  = Output(UInt((params.bits+1).W))
  val index = params.narrow.option(Input(UInt(params.bits.W)))

  // Signals used to self-stabilize a safe AsyncQueue
  val safe = params.safe.option(new AsyncBundleSafety)
}

object GrayCounter {
  def apply(bits: Int, increment: Bool = true.B, clear: Bool = false.B, name: String = "binary"): UInt = {
    val incremented = Wire(UInt(bits.W))
    val binary = RegNext(next=incremented, init=0.U).suggestName(name)
    incremented := Mux(clear, 0.U, binary + increment.asUInt)
    incremented ^ (incremented >> 1)
  }
}

class AsyncValidSync(sync: Int, desc: String) extends RawModule {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })
  val clock = IO(Input(Clock()))
  val reset = IO(Input(AsyncReset()))
  withClockAndReset(clock, reset){
    io.out := AsyncResetSynchronizerShiftReg(io.in, sync, Some(desc))
  }
}

class AsyncQueueSource[T <: Data](gen: T, params: AsyncQueueParams = AsyncQueueParams()) extends Module {
  val io = IO(new Bundle {
    // These come from the source domain
    val enq = Flipped(Decoupled(gen))
    // These cross to the sink clock domain
    val async = new AsyncBundle(gen, params)
  })

  val bits = params.bits
  val sink_ready = WireInit(true.B)
  val mem = Reg(Vec(params.depth, gen)) // This does NOT need to be reset at all.
  val widx = withReset(reset.asAsyncReset)(GrayCounter(bits+1, io.enq.fire, !sink_ready, "widx_bin"))
  val ridx = AsyncResetSynchronizerShiftReg(io.async.ridx, params.sync, Some("ridx_gray"))
  val ready = sink_ready && widx =/= (ridx ^ (params.depth | params.depth >> 1).U)

  val index = if (bits == 0) 0.U else io.async.widx(bits-1, 0) ^ (io.async.widx(bits, bits) << (bits-1))
  when (io.enq.fire) { mem(index) := io.enq.bits }

  val ready_reg = withReset(reset.asAsyncReset)(RegNext(next=ready, init=false.B).suggestName("ready_reg"))
  io.enq.ready := ready_reg && sink_ready

  val widx_reg = withReset(reset.asAsyncReset)(RegNext(next=widx, init=0.U).suggestName("widx_gray"))
  io.async.widx := widx_reg

  io.async.index match {
    case Some(index) => io.async.mem(0) := mem(index)
    case None => io.async.mem := mem
  }

  io.async.safe.foreach { sio =>
    val source_valid_0 = Module(new AsyncValidSync(params.sync, "source_valid_0"))
    val source_valid_1 = Module(new AsyncValidSync(params.sync, "source_valid_1"))

    val sink_extend  = Module(new AsyncValidSync(params.sync, "sink_extend"))
    val sink_valid   = Module(new AsyncValidSync(params.sync, "sink_valid"))
    source_valid_0.reset := (reset.asBool || !sio.sink_reset_n).asAsyncReset
    source_valid_1.reset := (reset.asBool || !sio.sink_reset_n).asAsyncReset
    sink_extend   .reset := (reset.asBool || !sio.sink_reset_n).asAsyncReset
    sink_valid    .reset := reset.asAsyncReset

    source_valid_0.clock := clock
    source_valid_1.clock := clock
    sink_extend   .clock := clock
    sink_valid    .clock := clock

    source_valid_0.io.in := true.B
    source_valid_1.io.in := source_valid_0.io.out
    sio.widx_valid := source_valid_1.io.out
    sink_extend.io.in := sio.ridx_valid
    sink_valid.io.in := sink_extend.io.out
    sink_ready := sink_valid.io.out
    sio.source_reset_n := !reset.asBool

    // Assert that if there is stuff in the queue, then reset cannot happen
    //  Impossible to write because dequeue can occur on the receiving side,
    //  then reset allowed to happen, but write side cannot know that dequeue
    //  occurred.
    // TODO: write some sort of sanity check assertion for users
    // that denote don't reset when there is activity
    //    assert (!(reset || !sio.sink_reset_n) || !io.enq.valid, "Enqueue while sink is reset and AsyncQueueSource is unprotected")
    //    assert (!reset_rise || prev_idx_match.asBool, "Sink reset while AsyncQueueSource not empty")
  }
}

class AsyncQueueSink[T <: Data](gen: T, params: AsyncQueueParams = AsyncQueueParams()) extends Module {
  val io = IO(new Bundle {
    // These come from the sink domain
    val deq = Decoupled(gen)
    // These cross to the source clock domain
    val async = Flipped(new AsyncBundle(gen, params))
  })

  val bits = params.bits
  val source_ready = WireInit(true.B)
  val ridx = withReset(reset.asAsyncReset)(GrayCounter(bits+1, io.deq.fire, !source_ready, "ridx_bin"))
  val widx = AsyncResetSynchronizerShiftReg(io.async.widx, params.sync, Some("widx_gray"))
  val valid = source_ready && ridx =/= widx

  // The mux is safe because timing analysis ensures ridx has reached the register
  // On an ASIC, changes to the unread location cannot affect the selected value
  // On an FPGA, only one input changes at a time => mem updates don't cause glitches
  // The register only latches when the selected valued is not being written
  val index = if (bits == 0) 0.U else ridx(bits-1, 0) ^ (ridx(bits, bits) << (bits-1))
  io.async.index.foreach { _ := index }
  // This register does not NEED to be reset, as its contents will not
  // be considered unless the asynchronously reset deq valid register is set.
  // It is possible that bits latches when the source domain is reset / has power cut
  // This is safe, because isolation gates brought mem low before the zeroed widx reached us
  val deq_bits_nxt = io.async.mem(if (params.narrow) 0.U else index)
  io.deq.bits := ClockCrossingReg(deq_bits_nxt, en = valid, doInit = false, name = Some("deq_bits_reg"))

  val valid_reg = withReset(reset.asAsyncReset)(RegNext(next=valid, init=false.B).suggestName("valid_reg"))
  io.deq.valid := valid_reg && source_ready

  val ridx_reg = withReset(reset.asAsyncReset)(RegNext(next=ridx, init=0.U).suggestName("ridx_gray"))
  io.async.ridx := ridx_reg

  io.async.safe.foreach { sio =>
    val sink_valid_0    = Module(new AsyncValidSync(params.sync, "sink_valid_0"))
    val sink_valid_1    = Module(new AsyncValidSync(params.sync, "sink_valid_1"))

    val source_extend = Module(new AsyncValidSync(params.sync, "source_extend"))
    val source_valid  = Module(new AsyncValidSync(params.sync, "source_valid"))
    sink_valid_0 .reset := (reset.asBool || !sio.source_reset_n).asAsyncReset
    sink_valid_1 .reset := (reset.asBool || !sio.source_reset_n).asAsyncReset
    source_extend.reset := (reset.asBool || !sio.source_reset_n).asAsyncReset
    source_valid .reset := reset.asAsyncReset

    sink_valid_0 .clock := clock
    sink_valid_1 .clock := clock
    source_extend.clock := clock
    source_valid .clock := clock

    sink_valid_0.io.in := true.B
    sink_valid_1.io.in := sink_valid_0.io.out
    sio.ridx_valid := sink_valid_1.io.out
    source_extend.io.in := sio.widx_valid
    source_valid.io.in := source_extend.io.out
    source_ready := source_valid.io.out
    sio.sink_reset_n := !reset.asBool

    // TODO: write some sort of sanity check assertion for users
    // that denote don't reset when there is activity
    // 
    // val reset_and_extend = !source_ready || !sio.source_reset_n || reset.asBool
    // val reset_and_extend_prev = RegNext(reset_and_extend, true.B)
    // val reset_rise = !reset_and_extend_prev && reset_and_extend
    // val prev_idx_match = AsyncResetReg(updateData=(io.async.widx===io.async.ridx), resetData=0)
    // assert (!reset_rise || prev_idx_match.asBool, "Source reset while AsyncQueueSink not empty")
  }
}

object FromAsyncBundle
{
  // Sometimes it makes sense for the sink to have different sync than the source
  def apply[T <: Data](x: AsyncBundle[T]): DecoupledIO[T] = apply(x, x.params.sync)
  def apply[T <: Data](x: AsyncBundle[T], sync: Int): DecoupledIO[T] = {
    val sink = Module(new AsyncQueueSink(chiselTypeOf(x.mem(0)), x.params.copy(sync = sync)))
    sink.io.async <> x
    sink.io.deq
  }
}

object ToAsyncBundle
{
  def apply[T <: Data](x: ReadyValidIO[T], params: AsyncQueueParams = AsyncQueueParams()): AsyncBundle[T] = {
    val source = Module(new AsyncQueueSource(chiselTypeOf(x.bits), params))
    source.io.enq <> x
    source.io.async
  }
}

class AsyncQueue[T <: Data](gen: T, params: AsyncQueueParams = AsyncQueueParams()) extends Crossing[T] {
  val io = IO(new CrossingIO(gen))
  val source = withClockAndReset(io.enq_clock, io.enq_reset) { Module(new AsyncQueueSource(gen, params)) }
  val sink   = withClockAndReset(io.deq_clock, io.deq_reset) { Module(new AsyncQueueSink  (gen, params)) }

  source.io.enq <> io.enq
  io.deq <> sink.io.deq
  sink.io.async <> source.io.async
}

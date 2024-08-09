package grapecoveDCache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import _root_.circt.stage.ChiselStage
import grapecoveDCache._

class IOMSHR(id: Int)(
    implicit edge: TLEdgeOut
) extends Module {
  val io = IO(new Bundle {
    val req = Input(new MainPipeReq)

    val isEmpty  = Output(Bool())
    val reqValid = Input(Bool())

    val addrMatch = Output(Bool())

    val replayFinish = Input(Bool())

    val reqReg = Output(new MainPipeReq)

  })

  val mode_idle :: mode_busy :: Nil = Enum(2)
  val state                         = RegInit(mode_idle)

  val reqReg = RegEnable(io.req, 0.U.asTypeOf(new MainPipeReq), io.reqValid)
  io.reqReg := reqReg

  state := MuxLookup(state, state)(
    Seq(
      mode_idle -> Mux(io.reqValid, mode_busy, state),
      mode_busy -> Mux(io.replayFinish, mode_idle, state),
    )
  )
  io.isEmpty   := state === mode_idle
  io.addrMatch := state === mode_busy && io.req.paddr === reqReg.paddr
}

class IOMSHRFile(
    implicit edge: TLEdgeOut
) extends Module {
  val io = IO(new Bundle {
    val req       = Flipped(DecoupledIO(new MainPipeReq))
    val addrMatch = Output(Bool())
    val resp      = DecoupledIO(new DataExchangeResp())

    val l2Req      = DecoupledIO(new TLBundleA(edge.bundle))
    val fromRefill = Flipped(DecoupledIO(new RefillMSHRFile))
  })

  val mode_idle :: mode_replay :: Nil = Enum(2)
  val state                           = RegInit(mode_idle)

  val senderQueue = Module(new MyQueue(UInt(log2Up(nMMIOs).W), nMMIOs))

  val allocArb = Module(new Arbiter(Bool(), nMMIOs))
  allocArb.io.in.foreach(_.bits := DontCare)
  allocArb.io.out.ready := senderQueue.io.enq.ready && io.req.valid

  val addrMatchList    = Wire(Vec(nMMIOs, Bool()))
  val replayFinishList = Wire(Vec(nMMIOs, Bool()))
  val reqList          = Wire(Vec(nMMIOs, new MainPipeReq))
  val allocList        = Wire(Vec(nMMIOs, Bool()))

  val iomshrs = (0 until nMMIOs) map {
    i =>
      val iomshr = Module(new IOMSHR(i)(edge))

      allocArb.io.in(i).valid := iomshr.io.isEmpty

      allocList(i)       := allocArb.io.in(i).fire
      iomshr.io.reqValid := allocArb.io.in(i).fire
      iomshr.io.req      := io.req.bits

      reqList(i) := iomshr.io.reqReg

      addrMatchList(i)       := iomshr.io.addrMatch
      iomshr.io.replayFinish := replayFinishList(i)

  }

  // to pipe req
  io.req.ready := allocArb.io.out.valid
  io.addrMatch := addrMatchList.asUInt.orR

  // set sender enq/deq info
  senderQueue.io.enq.valid := senderQueue.io.enq.ready && io.req.valid
  senderQueue.io.enq.bits  := OHToUInt(allocList.asUInt)

  val a_source = senderQueue.io.deq.bits + firstMMIO.U
  val a_addr   = reqList(senderQueue.io.deq.bits).paddr
  val a_size   = reqList(senderQueue.io.deq.bits).size
  val a_data   = reqList(senderQueue.io.deq.bits).wdata
  val a_mask   = reqList(senderQueue.io.deq.bits).wmask
  val a_cmd    = reqList(senderQueue.io.deq.bits).cmd

  val get = edge.Get(a_source, a_addr, a_size)._2
  val put = edge.Put(a_source, a_addr, a_size, a_data)._2
  val atomic = MuxLookup(a_cmd, 0.U.asTypeOf(new TLBundleA(edge.bundle)))(
    Seq(
      M_XA_SWAP -> edge.Logical(a_source, a_addr, a_size, a_data, TLAtomics.SWAP)._2,
      M_XA_XOR  -> edge.Logical(a_source, a_addr, a_size, a_data, TLAtomics.XOR)._2,
      M_XA_OR   -> edge.Logical(a_source, a_addr, a_size, a_data, TLAtomics.OR)._2,
      M_XA_AND  -> edge.Logical(a_source, a_addr, a_size, a_data, TLAtomics.AND)._2,
      M_XA_ADD  -> edge.Arithmetic(a_source, a_addr, a_size, a_data, TLAtomics.ADD)._2,
      M_XA_MIN  -> edge.Arithmetic(a_source, a_addr, a_size, a_data, TLAtomics.MIN)._2,
      M_XA_MAX  -> edge.Arithmetic(a_source, a_addr, a_size, a_data, TLAtomics.MAX)._2,
      M_XA_MINU -> edge.Arithmetic(a_source, a_addr, a_size, a_data, TLAtomics.MINU)._2,
      M_XA_MAXU -> edge.Arithmetic(a_source, a_addr, a_size, a_data, TLAtomics.MAXU)._2,
    )
  )
  val bypassStorePartial = edge.Put(a_source, a_addr, a_size, a_data, a_mask)._2

  io.l2Req.valid           := senderQueue.io.deq.valid
  senderQueue.io.deq.ready := io.l2Req.ready
  io.l2Req.bits := Mux(
    reqList(senderQueue.io.deq.bits).noAlloc && a_cmd === M_PWR,
    bypassStorePartial,
    Mux(isAMO(a_cmd), atomic, Mux(isRead(a_cmd), get, put)),
  )

  // refill req
  val respIOMSHRIdx =
    RegEnable(
      io.fromRefill.bits.entryId - firstMMIO.U,
      0.U,
      state === mode_idle && io.fromRefill.valid && io.fromRefill.bits.hasData,
    )
  val refillData =
    RegEnable(io.fromRefill.bits.data, 0.U, state === mode_idle && io.fromRefill.valid && io.fromRefill.bits.hasData)

  val loadgen = new LoadGen(
    reqList(respIOMSHRIdx).size,
    reqList(respIOMSHRIdx).signed,
    reqList(respIOMSHRIdx).paddr,
    refillData,
    false.B,
    blockBytes,
  )

  io.resp.valid        := state === mode_replay
  io.resp.bits.hasData := true.B
  io.resp.bits.source  := reqList(respIOMSHRIdx).source
  io.resp.bits.dest    := reqList(respIOMSHRIdx).dest
  io.resp.bits.status  := CacheRespStatus.refill
  io.resp.bits.data    := loadgen.data

  io.fromRefill.ready := state === mode_idle

  replayFinishList := Mux(
    io.resp.fire,
    UIntToOH(respIOMSHRIdx),
    Mux(io.fromRefill.fire && !io.fromRefill.bits.hasData, UIntToOH(io.fromRefill.bits.entryId - firstMMIO.U), 0.U),
  ).asTypeOf(replayFinishList)

  state := MuxLookup(state, state)(
    Seq(
      mode_idle   -> Mux(io.fromRefill.valid && io.fromRefill.bits.hasData, mode_replay, state),
      mode_replay -> Mux(io.resp.fire, mode_idle, state),
    )
  )

}

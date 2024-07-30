//package grapecoveDcache
//
//import chisel3._
//import chisel3.util._
//import freechips.rocketchip.tilelink._
//import _root_.circt.stage.ChiselStage
//import grapecoveDCache._
//
//class IOMSHR(id: Int)(
//    implicit edge: TLEdgeOut
//) extends Module {
//  val io = IO(new Bundle {
//    val req = Flipped(DecoupledIO(new MainPipeReq))
//
//    val isEmpty      = Output(Bool())
//    val reqValid     = Input(Bool())
//    val isRefilling  = Input(Bool())
//    val replayFinish = Input(Bool())
//
//    val reqReg = Output(new MainPipeReq)
//
//  })
//
//  val mode_idle :: mode_resp_wait :: mode_replay :: Nil = Enum(3)
//  val state                                             = RegInit(mode_idle)
//
//  val reqReg = RegEnable(io.req, 0.U, io.reqValid)
//  io.reqReg := reqReg
//
//  state := MuxLookup(state, state)(
//    Seq(
//      mode_idle      -> Mux(io.reqValid, mode_resp_wait, state),
//      mode_resp_wait -> Mux(io.isRefilling, mode_replay, state),
//      mode_replay    -> Mux(io.replayFinish, mode_idle, state),
//    )
//  )
//  io.isEmpty := state === mode_idle
//}
//
//class IOMSHRFile(
//    implicit edge: TLEdgeOut
//) extends Module {
//  val io = IO(new Bundle {
//    val req = Flipped(DecoupledIO(new MainPipeReq))
//    val resp = DecoupledIO(new DataExchangeResp())
//
//    val l2Req     = DecoupledIO(new TLBundleA(edge.bundle))
//    val fromReill = Flipped(DecoupledIO(new L2Refill))
//  })
//
//  val mode_idle :: mode_replay :: Nil = Enum(2)
//  val state                           = RegInit(mode_idle)
//
//  val senderQueue = Module(new Queue(UInt(log2Up(nMMIOs).W), nMMIOs))
//
//  val allocArb = Module(new Arbiter(Bool(), nMMIOs))
//  allocArb.io.in.foreach(_.bits := DontCare)
//  allocArb.io.out.ready := !senderQueue.full && io.req.valid
//
//  val refillList = Wire(Vec(nMMIOs, Bool()))
//  val reqList    = Wire(Vec(nMMIOs, new MainPipeReq))
//
//  val iomshrs = (0 until nMMIOs) map {
//    i =>
//      val iomshr = Module(new IOMSHR(i)(edge))
//
//      allocArb.io.in(i).valid := iomshr.io.isEmpty
//
//      iomshr.io.reqValid := allocArb.io.in(i).ready
//      iomshr.io.req      := io.req
//
//      reqList(i) := iomshr.io.reqReg
//
//  }
//
//  // to pipe req
//  io.req.ready := allocArb.io.out.valid
//
//  // set sender deq info
//  val a_source = senderQueue.io.deq.bits + nMSHRs
//  val a_addr   = reqList(senderQueue.io.deq.bits).paddr
//  val a_size   = reqList(senderQueue.io.deq.bits).size
//  val a_data   = reqList(senderQueue.io.deq.bits).wdata
//  val a_cmd    = reqList(senderQueue.io.deq.bits).cmd
//
//  val get = edge.Get(a_source, a_addr, a_size)._2
//  val put = edge.Put(a_source, a_addr, a_size, a_data)._2
//  val atomic = MuxLookup(a_cmd, 0.U.asTypeOf(new TLBundleA(edge.bundle)))(
//    Seq(
//      M_XA_SWAP -> edge.Logical(a_source, a_addr, a_size, a_data, TLAtomics.SWAP)._2,
//      M_XA_XOR  -> edge.Logical(a_source, a_addr, a_size, a_data, TLAtomics.XOR)._2,
//      M_XA_OR   -> edge.Logical(a_source, a_addr, a_size, a_data, TLAtomics.OR)._2,
//      M_XA_AND  -> edge.Logical(a_source, a_addr, a_size, a_data, TLAtomics.AND)._2,
//      M_XA_ADD  -> edge.Arithmetic(a_source, a_addr, a_size, a_data, TLAtomics.ADD)._2,
//      M_XA_MIN  -> edge.Arithmetic(a_source, a_addr, a_size, a_data, TLAtomics.MIN)._2,
//      M_XA_MAX  -> edge.Arithmetic(a_source, a_addr, a_size, a_data, TLAtomics.MAX)._2,
//      M_XA_MINU -> edge.Arithmetic(a_source, a_addr, a_size, a_data, TLAtomics.MINU)._2,
//      M_XA_MAXU -> edge.Arithmetic(a_source, a_addr, a_size, a_data, TLAtomics.MAXU)._2,
//    )
//  )
//  val bypass = edge.AcquireBlock(a_source, a_addr, a_size, TLPermissions.NtoB)._2
//
//  io.l2Req.valid           := senderQueue.io.deq.valid
//  senderQueue.io.deq.ready := io.l2Req.ready
//  io.l2Req.bits := Mux(
//    reqList(senderQueue.io.deq.bits).noAlloc,
//    bypass,
//    Mux(isAMO(a_cmd), atomic, Mux(isRead(a_cmd), get, put)),
//  )
//
//  // refill req
//
//  state := MuxLookup(state, state )(
//    Seq(
//      mode_idle -> Mux(io.fromReill.valid, mode_replay, state),
//      mode_replay -> Mux(io.,, state)
//    )
//  )
//
//}

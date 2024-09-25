// package grapecoveDCache

// import chisel3._
// import chisel3.util._
// import freechips.rocketchip.rocket._
// import freechips.rocketchip.rocket.HellaCacheExceptions
// import org.chipsalliance.cde.config.{Parameters, Field}
// import freechips.rocketchip.diplomacy._
// import freechips.rocketchip.tilelink._
// import freechips.rocketchip.tile._
// import freechips.rocketchip.system._
// import org.chipsalliance.cde.config.Config
// import freechips.rocketchip.subsystem._
// import freechips.rocketchip.util._

// class MemBlockReq extends Bundle {
//   val source = UInt(MasterSource.width.W)
//   val addr   = UInt(paddrWidth.W)
//   val cmd    = UInt(M_SZ.W)
//   // 64 -> 2, 512 -> 3
//   val size    = UInt(log2Up(log2Up(dataBytes)).W)
//   val signed  = Bool()
//   val wdata   = UInt(dataWidth.W)
//   val wmask   = UInt(dataBytes.W)
//   val noAlloc = Bool()
//   val dest    = UInt(destWidth.W) // reg addr or lsq idx
//   val phys    = Bool()            // physical address
//   val dprv    = UInt(PRV.SZ.W)
//   val dv      = Bool()
// }

// class MemBlock()(
//     implicit p: Parameters
// ) extends BaseDCache with MemoryOpConstants {
//   val cache                = LazyModule(new GPCDCache()(Parameters.empty))
//   override val node        = cache.node
//   override lazy val module = new MemBlockImp(this)
// }

// class MemBlockImp(outer: MemBlock) extends BaseDCacheImp(outer) {

//   val io = IO(new Bundle {
//     // val ptw           = new TLBPTWIO()
//     val req           = Flipped(Decoupled(new MemBlockReq))
//     val resp          = Valid(new DataExchangeResp())
//     val s1_kill       = Input(Bool())
//     val s2_kill       = Input(Bool())
//     val s2_xcpt       = Output(new HellaCacheExceptions)
//     val s2_gpa        = Output(UInt(vaddrWidth.W))
//     val s2_gpa_is_pte = Output(Bool())
//   })

//   // * s0 stage Begin
//   // * check TLB & get TLB resp
//   val nTLBSets = 1
//   val nTLBWays = 32
//   val dtlb = Module(new GPCTLB(
//     false,               // is instruction?
//     log2Ceil(dataBytes), // log max size
//     GPCTLBConfig(nTLBSets, nTLBWays),
//   ))
//   dtlb.io.ptw                  := DontCare
//   dtlb.io.req.valid            := io.req.valid
//   dtlb.io.req.bits.passthrough := io.req.bits.phys
//   dtlb.io.req.bits.vaddr       := io.req.bits.addr
//   dtlb.io.req.bits.cmd         := io.req.bits.cmd
//   dtlb.io.req.bits.size        := io.req.bits.size
//   dtlb.io.req.bits.prv         := io.req.bits.dprv
//   dtlb.io.req.bits.v           := io.req.bits.dv

//   when(!dtlb.io.req.ready && !io.req.bits.phys) {
//     io.req.ready := false.B
//   }

//   dtlb.io.sfence.valid     := io.req.valid && io.req.bits.cmd === M_SFENCE
//   dtlb.io.sfence.bits.rs1  := io.req.bits.size(0)
//   dtlb.io.sfence.bits.rs2  := io.req.bits.size(1)
//   dtlb.io.sfence.bits.asid := io.req.bits.wdata
//   dtlb.io.sfence.bits.hv   := io.req.bits.cmd === M_HFENCEV
//   dtlb.io.sfence.bits.hg   := io.req.bits.cmd === M_HFENCEG

//   val s0_tlbResp = dtlb.io.resp
//   val s0_nack    = dtlb.io.resp.miss
//   val s0_xcpt    = Wire(new HellaCacheExceptions)
//   s0_xcpt := dtlb.io.resp
//   val s0_xcptValid = s0_xcpt.asUInt.orR

//   // * s0 stage End

//   // * s1 stage Begin

//   val s1_cahceValid = RegNext(dtlb.io.req.fire & !s0_nack && !s0_xcptValid)
//   val s1_req        = RegEnable(io.req.bits, io.req.valid)
//   val s1_tlbResp    = RegNext(s0_tlbResp)
//   val s1_xcptValid  = RegNext(dtlb.io.req.fire && !dtlb.io.resp.miss)

//   val cacheReq = Wire(new DataExchangeReq)
//   cacheReq.source    := s1_req.source
//   cacheReq.paddr     := s1_tlbResp.paddr
//   cacheReq.cmd       := s1_req.cmd
//   cacheReq.size      := s1_req.size
//   cacheReq.signed    := s1_req.signed
//   cacheReq.wdata     := s1_req.wdata
//   cacheReq.wmask     := s1_req.wmask
//   cacheReq.noAlloc   := s1_req.noAlloc
//   cacheReq.dest      := s1_req.dest
//   cacheReq.isRefill  := false.B
//   cacheReq.refillWay := 0.U
//   cacheReq.refillCoh := 0.U

//   val cache = outer.cache.module

//   cache.io.req.valid := s1_cahceValid
//   cache.io.req.bits  := cacheReq

//   // * s1 stage End

//   // * s2 stage Begin
//   // * get Cache Resp
//   val s2_xcptValid = RegNext(s1_xcptValid)
//   val s2_tlbResp   = RegNext(s1_tlbResp)

//   io.s2_xcpt := Mux(s2_xcptValid, s2_tlbResp, 0.U.asTypeOf(new HellaCacheExceptions))
//   io.resp    := cache.io.resp
//   // * s2 stage End

// }

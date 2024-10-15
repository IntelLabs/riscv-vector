package gpc.core

import chisel3._
import chisel3.util._
import chisel3.withClock
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property
import scala.collection.mutable.ArrayBuffer
import freechips.rocketchip.rocket._
import utility._
import VectorParam._
import gpc.tile._

case class GpcCoreParams(
  // Enable verificaton
  enUVM: Boolean = false,

  bootFreqHz: BigInt = 0,
  useVM: Boolean = true,
  useUser: Boolean = false,
  useSupervisor: Boolean = false,
  useHypervisor: Boolean = false,
  useDebug: Boolean = true,
  useAtomics: Boolean = true,
  useAtomicsOnlyForIO: Boolean = false,
  useCompressed: Boolean = true,
  useRVE: Boolean = false,
  useConditionalZero: Boolean = false,
  nLocalInterrupts: Int = 0,
  useNMI: Boolean = false,
  nBreakpoints: Int = 1,
  useBPWatch: Boolean = false,
  mcontextWidth: Int = 0,
  scontextWidth: Int = 0,
  nPMPs: Int = 8,
  nPerfCounters: Int = 0,
  haveBasicCounters: Boolean = true,
  haveCFlush: Boolean = false,
  misaWritable: Boolean = true,
  nL2TLBEntries: Int = 0,
  nL2TLBWays: Int = 1,
  nPTECacheEntries: Int = 8,
  mtvecInit: Option[BigInt] = Some(BigInt(0)),
  mtvecWritable: Boolean = true,
  fastLoadWord: Boolean = true,
  fastLoadByte: Boolean = false,
  branchPredictionModeCSR: Boolean = false,
  clockGate: Boolean = false,
  mvendorid: Int = 0, // 0 means non-commercial implementation
  mimpid: Int = 0x20181004, // release date in BCD
  mulDiv: Option[MulDivParams] = Some(MulDivParams()),
  fpu: Option[FPUParams] = Some(FPUParams()),
  debugROB: Boolean = false, // if enabled, uses a C++ debug ROB to generate trace-with-wdata
  haveCease: Boolean = false, // non-standard CEASE instruction
  haveSimTimeout: Boolean = true // add plusarg for simulation timeout
) extends CoreParams {
  val lgPauseCycles = 5
  val haveFSDirty = false
  val pmpGranularity: Int = if (useHypervisor) 4096 else 4
  val fetchWidth: Int = if (useCompressed) 4 else 2
  //  fetchWidth doubled, but coreInstBytes halved, for RVC:
  val decodeWidth: Int = 2
  val retireWidth: Int = 2
  val instBits: Int = if (useCompressed) 16 else 32
  val lrscCycles: Int = 80 // worst case is 14 mispredicted branches + slop
  val traceHasWdata: Boolean = false // ooo wb, so no wdata in trace
  override val customIsaExt = Option.when(haveCease)("xrocket") // CEASE instruction
  override def minFLen: Int = fpu.map(_.minFLen).getOrElse(32)
  override def customCSRs(implicit p: Parameters) = new GpcCustomCSRs
  // Enable verification
  override val useVerif: Boolean = enUVM
}

trait HasGpcCoreParameters extends HasCoreParameters {
  lazy val gpcParams: GpcCoreParams = tileParams.core.asInstanceOf[GpcCoreParams].copy(useDebug = true)

  val fastLoadWord = gpcParams.fastLoadWord
  val fastLoadByte = gpcParams.fastLoadByte

  val mulDivParams = gpcParams.mulDiv.getOrElse(MulDivParams()) // TODO ask andrew about this

  val aluFn = new ALUFN

  require(!fastLoadByte || fastLoadWord)
  require(!gpcParams.haveFSDirty, "rocket doesn't support setting fs dirty from outside, please disable haveFSDirty")
}

class GpcCustomCSRs(implicit p: Parameters) extends CustomCSRs with HasGpcCoreParameters {
  override def bpmCSR = {
    gpcParams.branchPredictionModeCSR.option(CustomCSR(bpmCSRId, BigInt(1), Some(BigInt(0))))
  }

  private def haveDCache = tileParams.dcache.get.scratch.isEmpty

  override def chickenCSR = {
    val mask = BigInt(
      tileParams.dcache.get.clockGate.toInt << 0 |
      gpcParams.clockGate.toInt << 1 |
      gpcParams.clockGate.toInt << 2 |
      1 << 3 | // disableSpeculativeICacheRefill
      haveDCache.toInt << 9 | // suppressCorruptOnGrantData
      tileParams.icache.get.prefetch.toInt << 17
    )
    Some(CustomCSR(chickenCSRId, mask, Some(mask)))
  }

  def disableICachePrefetch = getOrElse(chickenCSR, _.value(17), true.B)

  def marchid = CustomCSR.constant(CSRs.marchid, BigInt(1))

  def mvendorid = CustomCSR.constant(CSRs.mvendorid, BigInt(gpcParams.mvendorid))

  // mimpid encodes a release version in the form of a BCD-encoded datestamp.
  def mimpid = CustomCSR.constant(CSRs.mimpid, BigInt(gpcParams.mimpid))

  override def decls = super.decls :+ marchid :+ mvendorid :+ mimpid
}

class CoreInterrupts(val hasBeu: Boolean)(implicit p: Parameters) extends TileInterrupts()(p) {
  val buserror = Option.when(hasBeu)(Bool())
}

trait HasGpcCoreIO extends HasGpcCoreParameters {
  implicit val p: Parameters
  def nTotalRoCCCSRs: Int
  val io = IO(new CoreBundle()(p) {
    val hartid = Input(UInt(hartIdLen.W))
    val reset_vector = Input(UInt(resetVectorLen.W))
    val interrupts = Input(new CoreInterrupts(tileParams.asInstanceOf[GpcTileParams].beuAddr.isDefined))
    val imem  = new FrontendIOGpc
    val dmem = new HellaCacheIO
    val ptw = Flipped(new DatapathPTWIO())
    val fpu = Flipped(new FPUCoreIO())
    val rocc = Flipped(new RoCCCoreIO(nTotalRoCCCSRs))
    val trace = Output(new TraceBundle)
    val bpwatch = Output(Vec(coreParams.nBreakpoints, new BPWatch(coreParams.retireWidth)))
    val cease = Output(Bool())
    val wfi = Output(Bool())
    val traceStall = Input(Bool())
    val vissue = Output(new VIssue)
    val villegal = Flipped(new VIllegal)
    val vcomplete = Flipped(new VComplete)
    val vwb_ready = Flipped(new VWritebackReady)
    val verif = coreParams.useVerif.option(new VerOutIO)
  })
}


class Gpc(tile: GpcTile)(implicit p: Parameters) extends CoreModule()(p)
    with HasGpcCoreParameters
    with HasGpcCoreIO 
    with HasCircularQueuePtrHelper {
  def nTotalRoCCCSRs = tile.roccCSRs.flatten.size

  val clock_en_reg = RegInit(true.B)
  val long_latency_stall = Reg(Bool())
  val id_reg_pause = RegInit(fasle.B)
  val imem_might_request_reg = Reg(Bool())
  val clock_en = WireDefault(true.B)
  val gated_clock =
    if (!gpcParams.clockGate) clock
    else ClockGate(clock, clock_en, "gpc_clock_gate")

  class GpcImpl { // entering gated-clock domain

  /** Temp: delete most perfEventes, define some wrong values */
  // performance counters //FIXME - 
  def pipelineIDToWB[T <: Data](x: T): T =
    RegEnable(RegEnable(RegEnable(x, !ctrl_killd(0)), ex_reg_valids(0)), m1_reg_valids(0))
  val perfEvents = new EventSets(Seq(
    new EventSet((mask, hits) => Mux(m2_xcpt(0), mask(0), m2_reg_valids(0) && pipelineIDToWB((mask & hits).orR)), Seq(
      ("exception", () => false.B),
     )    
  )))

  val pipelinedMul = usingMulDiv && mulDivParams.mulUnroll == xLen
  val decode_table = {
    (if (usingMulDiv) new MDecode(pipelinedMul, aluFn) +: (xLen > 32).option(new M64Decode(pipelinedMul, aluFn)).toSeq else Nil) ++:
    (if (usingAtomics) new ADecode(aluFn) +: (xLen > 32).option(new A64Decode(aluFn)).toSeq else Nil) ++:
    (if (fLen >= 32)    new FDecode(aluFn) +: (xLen > 32).option(new F64Decode(aluFn)).toSeq else Nil) ++:
    (if (fLen >= 64)    new DDecode(aluFn) +: (xLen > 32).option(new D64Decode(aluFn)).toSeq else Nil) ++:
    (if (minFLen == 16) new HDecode(aluFn) +: (xLen > 32).option(new H64Decode(aluFn)).toSeq ++: (fLen >= 64).option(new HDDecode(aluFn)).toSeq else Nil) ++:
    // (usingRoCC.option(new RoCCDecode(aluFn))) ++:
    (if (xLen == 32) new I32Decode(aluFn) else new I64Decode(aluFn)) +:
    (usingVM.option(new SVMDecode(aluFn))) ++:
    (usingSupervisor.option(new SDecode(aluFn))) ++:
    // (usingHypervisor.option(new HypervisorDecode(aluFn))) ++:
    // ((usingHypervisor && (xLen == 64)).option(new Hypervisor64Decode(aluFn))) ++:
    (usingDebug.option(new DebugDecode(aluFn))) ++:
    // (usingNMI.option(new NMIDecode(aluFn))) ++:
    // (usingConditionalZero.option(new ConditionalZeroDecode(aluFn))) ++:
    Seq(new FenceIDecode(tile.dcache.flushOnFenceI, aluFn)) ++:
    // coreParams.haveCFlush.option(new CFlushDecode(tile.dcache.canSupportCFlushLine, aluFn)) ++:
    // gpcParams.haveCease.option(new CeaseDecode(aluFn)) ++:
    Seq(new IDecode(aluFn))
  } flatMap(_.table)

  val decodeWidthGpc = 2

  val id_swap = Wire(Bool())

  val ex_reg_uops = Reg(Vec(decodeWidthGpc, new SUOp))
  val ex_reg_valids = Reg(Vec(decodeWidthGpc, Bool()))
  val ex_reg_swap = RegNext(id_swap)
  val ex_reg_rsdata = Reg(Vec(2, Vec(2, UInt(xLen.W))))
  val ex_reg_flush_pipe = Reg(Bool())
  val ex_reg_mem_size = Reg(UInt())
  val ex_reg_cause = Reg(Vec(2, UInt(64.W)))
  val ex_reg_next_pc = Reg(UInt(vaddrBitsExtended.W))
  val ex_reg_wphit = Reg(Vec(nBreakpoints, Bool()))
  val ex_reg_sfence = Reg(Bool())

  val m1_reg_uops = Reg(Vec(decodeWidthGpc, new SUOp))
  val m1_reg_valids = Reg(Vec(decodeWidthGpc, Bool()))
  val m1_reg_swap = RegNext(ex_reg_swap)
  val m1_reg_rsdata = Reg(Vec(2, Vec(2, UInt(xLen.W))))
  val m1_reg_wdata = Reg(Vec(2, UInt(xLen.W)))
  val m1_reg_flush_pipe = Reg(Bool())
  val m1_reg_mem_size = Reg(UInt())
  val m1_reg_cause = Reg(Vec(2, UInt(64.W)))
  val m1_reg_next_pc = Reg(UInt(vaddrBitsExtended.W))
  val m1_reg_wphit = Reg(Vec(nBreakpoints, Bool()))
  val m1_reg_load = Reg(Bool())
  val m1_reg_store = Reg(Bool())
  val m1_reg_sfence = Reg(Bool())

  val m2_reg_uops = Reg(Vec(decodeWidthGpc, new SUOp))
  val m2_reg_valids = Reg(Vec(decodeWidthGpc, Bool()))
  val m2_reg_swap = RegNext(m1_reg_swap)
  val m2_reg_rsdata = Reg(Vec(2, Vec(2, UInt(xLen.W))))
  val m2_reg_wdata = Reg(Vec(2, UInt(xLen.W)))
  val m2_reg_flush_pipe = Reg(Bool())
  val m2_reg_xcpt = Reg(Vec(2, Bool()))
  val m2_reg_mem_size = Reg(UInt())
  val m2_reg_cause = Reg(Vec(2, UInt(64.W)))
  val m2_reg_wphit = Reg(Vec(nBreakpoints, Bool()))
  val m2_reg_sfence = Reg(Bool())

  val wb_reg_uops = Reg(Vec(decodeWidthGpc, new SUOp))
  val wb_reg_valids = Reg(Vec(decodeWidthGpc, Bool()))
  val wb_reg_swap = RegNext(m2_reg_swap)
  val wb_reg_wdata = Reg(Vec(2, UInt(xLen.W)))
  val wb_reg_vissue = Reg(new VIssue)

  val take_pc_ex_p1 = Wire(Bool())
  val take_pc_m2_p0 = Wire(Bool())
  val take_pc_m2_p1 = Wire(Bool())
  val take_pc_m2 = take_pc_m2_p0 || take_pc_m2_p1
  val take_pc_all = take_pc_ex_p1 || take_pc_m2
  val vxcpt_flush = Wire(Bool())

  /** ID stage:
   *    Decode, RF read, Dual-issue scheduling
   */
  require(decodeWidthGpc == 2)
  assert(!(!io.imem.resp(0).valid && io.imem.resp(1).valid), "Assertion error: main pipe input valid(0)=false, valid(1)=true")
  val csr = Module(new CSRFile(perfEvents))
  
  val id_uops_origin = WireDefault(VecInit.fill(decodeWidthGpc)(0.U.asTypeOf(new SUOp)))
  val id_valids_origin = Seq(io.imem.resp(0).valid, io.imem.resp(1).valid)
  io.imem.resp.map(_.bits) zip id_uops_origin map { case (resp, uop) =>
    uop.ctrl.decode(resp.inst, decode_table)
    uop.inst := resp.inst
    uop.pc := resp.pc
    uop.rvc := resp.rvc
    uop.replay := resp.replay
    uop.btb_resp := resp.btb
  }
  // Vector decoding
  id_uops_origin.foreach { uop =>
    val (opcode, func3) = (uop.inst(6, 0), uop.inst(14, 12))
    val vec_arith_config = opcode === "b1010111".U
    uop.vec_arith := vec_arith_config && func3 =/= 7.U
    uop.vec_config := vec_arith_config && func3 === 7.U
    val vec_func3_0567 = func3 === 0.U || func3 === 5.U || func3 === 6.U || func3 === 7.U
    uop.vec_load := opcode === "b0000111".U && vec_func3_0567
    uop.vec_store := opcode === "b0100111".U && vec_func3_0567
    uop.vec := uop.vec_arith || uop.vec_load || uop.vec_store
    uop.pipe0 := uop.ctrl.pipe0 || uop.vec
    uop.pipe1 := uop.ctrl.pipe1 && !uop.vec
    val vec_mem = uop.vec_load || uop.vec_store
    uop.vec_config_case := Cat(!uop.inst(31), uop.inst(31, 30) === 3.U, uop.inst(31, 25) === "b1000000".U)
    val vec_need_rs1 = vec_mem || uop.vec_arith && func3(2) || uop.vec_config && !uop.vec_config_case(1)
    uop.rxs1 := uop.ctrl.rxs1 || vec_need_rs1
    val vec_need_rs2 = uop.inst(27, 26) === 2.U || uop.vec_config && uop.vec_config_case(0)
    uop.rxs2 := uop.ctrl.rxs2 || vec_need_rs2
    val vfmvfs = uop.vec_arith && uop.inst(31, 26) === "b010111".U && uop.inst(14, 12) === 5.U &&
                 uop.inst(25) === true.B && uop.inst(19, 15) === 0.U
    uop.wfd := uop.ctrl.wfd || vfmvfs
  }

  def hazard_targets_gen(uop: SUOp) = Seq((uop.rxs1 && uop.rs1 =/= 0.U, uop.rs1),
                                          (uop.rxs2 && uop.rs2 =/= 0.U, uop.rs2),
                                          (uop.ctrl.wxd  && uop.rd  =/= 0.U, uop.rd))
  // ---- Initial issue scheduling: swap ----
  //  Do not swap: (1) Interrupt goes to pipe-0, does not swap. Replay of pipe-0 does not swap.
  //               (2) uop0 only pipe0
  //         Swap: (1) uop0 only pipe1
  //               (2) uop0 pipe0 and pipe1, uop1 only pipe0
  id_swap := Mux(csr.io.interrupt || id_uops_origin(0).replay || id_uops_origin(0).onlyPipe0, 
                    false.B,
                    id_uops_origin(0).onlyPipe1 || 
                    id_uops_origin(0).ctrl.pipe0 && id_uops_origin(0).ctrl.pipe1 && id_uops_origin(1).onlyPipe0)
  val id_uops = Mux(!id_swap, id_uops_origin, VecInit(id_uops_origin.reverse))
  val id_valids = WireDefault(Mux(!id_swap, VecInit(id_valids_origin), VecInit(id_valids_origin.reverse)))
  
  // ---- Final issue scheduling: 
  //        check hazards and generate "ready"s for frontend ----
  val id_ctrls = id_uops.map(_.ctrl)
  val hazard_targets = id_uops map {hazard_targets_gen(_)}
  val fp_hazard_targets = Seq((io.fpu.dec.ren1, id_uops(1).rs1),
                              (io.fpu.dec.ren2, id_uops(1).rs2),
                              (io.fpu.dec.ren3, id_uops(1).rs3),
                              (io.fpu.dec.wen, id_uops(1).rd))
  /** Check RAW/WAW data hazard for:
   *    ID <--> EX     */
  val ex_ctrls = ex_reg_uops.map(_.ctrl)
  //----  Uop 0 ----
  val data_hazard_ex_uop0_p0 = ex_ctrls(0).wxd && checkHazards(hazard_targets(0), _ === ex_reg_uops(0).rd)
  val data_hazard_ex_uop0_p1 = ex_ctrls(1).wxd && checkHazards(hazard_targets(0), _ === ex_reg_uops(1).rd)
  val ex_cannot_bypass_p0 = ex_ctrls(0).csr =/= CSR.N || ex_ctrls(0).mem
  val ex_cannot_bypass_p1 = ex_ctrls(1).div || ex_ctrls(1).fp || ex_ctrls(1).mul
  val ex_can_bypass_uop0_p0 = ex_ctrls(0).mem && id_ctrls(0).alu
  val ex_can_bypass_uop0_p1 = ex_ctrls(1).mul && id_ctrls(0).alu
  // Using "||" is sufficient, because results of two pipes on the same stage cannot have WAW.
  val id_ex_hazard_uop0 = ex_reg_valids(0) && data_hazard_ex_uop0_p0 && ex_cannot_bypass_p0 && !ex_can_bypass_uop0_p0 || 
                          ex_reg_valids(1) && data_hazard_ex_uop0_p1 && ex_cannot_bypass_p1 && !ex_can_bypass_uop0_p1
  //----  Uop 1 ----
  val data_hazard_ex_uop1_p0 = ex_ctrls(0).wxd && checkHazards(hazard_targets(1), _ === ex_reg_uops(0).rd)
  val data_hazard_ex_uop1_p1 = ex_ctrls(1).wxd && checkHazards(hazard_targets(1), _ === ex_reg_uops(1).rd)
  val ex_can_bypass_uop1_p0 = ex_ctrls(0).mem && id_ctrls(1).alu
  val ex_can_bypass_uop1_p1 = ex_ctrls(1).mul && id_ctrls(1).alu
  val fp_data_hazard_ex = id_ctrls(1).fp && ex_ctrls(1).wfd && checkHazards(fp_hazard_targets, _ === ex_reg_uops(1).rd)
  val id_ex_hazard_uop1 = ex_reg_valids(0) && data_hazard_ex_uop1_p0 && ex_cannot_bypass_p0 && !ex_can_bypass_uop1_p0 || 
                          ex_reg_valids(1) && (data_hazard_ex_uop1_p1 && ex_cannot_bypass_p1 && !ex_can_bypass_uop1_p1 || fp_data_hazard_ex)

  /** Check RAW/WAW data hazard for:
   *    ID <--> M1     */
  val m1_ctrls = m1_reg_uops.map(_.ctrl)
  //----  Uop 0 ----
  val data_hazard_m1_uop0_p0 = m1_ctrls(0).wxd && checkHazards(hazard_targets(0), _ === m1_reg_uops(0).rd)
  val data_hazard_m1_uop0_p1 = m1_ctrls(1).wxd && checkHazards(hazard_targets(0), _ === m1_reg_uops(1).rd)
  val m1_cannot_bypass_p0 = m1_ctrls(0).csr =/= CSR.N || m1_ctrls(0).mem
  val m1_cannot_bypass_p1 = m1_ctrls(1).div || m1_ctrls(1).fp || m1_ctrls(1).mul
  val m1_can_bypass_uop0_p0 = m1_ctrls(0).mem && id_ctrls(0).alu
  val m1_can_bypass_uop0_p1 = m1_ctrls(1).mul && id_ctrls(0).alu
  val id_m1_hazard_uop0 = m1_reg_valids(0) && data_hazard_m1_uop0_p0 && m1_cannot_bypass_p0 && !m1_can_bypass_uop0_p0 || 
                          m1_reg_valids(1) && data_hazard_m1_uop0_p1 && m1_cannot_bypass_p1 && !m1_can_bypass_uop0_p1
  //----  Uop 1 ----
  val data_hazard_m1_uop1_p0 = m1_ctrls(0).wxd && checkHazards(hazard_targets(1), _ === m1_reg_uops(0).rd)
  val data_hazard_m1_uop1_p1 = m1_ctrls(1).wxd && checkHazards(hazard_targets(1), _ === m1_reg_uops(1).rd)
  val m1_can_bypass_uop1_p0 = m1_ctrls(0).mem && id_ctrls(1).alu
  val m1_can_bypass_uop1_p1 = m1_ctrls(1).mul && id_ctrls(1).alu
  val fp_data_hazard_m1 = id_ctrls(1).fp && m1_ctrls(1).wfd && checkHazards(fp_hazard_targets, _ === m1_reg_uops(1).rd)
  val id_m1_hazard_uop1 = m1_reg_valids(0) && data_hazard_m1_uop1_p0 && m1_cannot_bypass_p0 && !m1_can_bypass_uop1_p0 || 
                          m1_reg_valids(1) && (data_hazard_m1_uop1_p1 && m1_cannot_bypass_p1 && !m1_can_bypass_uop1_p1 || fp_data_hazard_m1)

  /** Check RAW/WAW data hazard for:
   *    ID <--> M2     */
  val m2_ctrls = m2_reg_uops.map(_.ctrl)
  //----  Uop 0 ----
  val data_hazard_m2_uop0_p0 = m2_ctrls(0).wxd && checkHazards(hazard_targets(0), _ === m2_reg_uops(0).rd)
  val data_hazard_m2_uop0_p1 = m2_ctrls(1).wxd && checkHazards(hazard_targets(0), _ === m2_reg_uops(1).rd)
  val m2_cannot_bypass_p0 = false.B
  val m2_cannot_bypass_p1 = m2_ctrls(1).div || m2_ctrls(1).fp
  val id_m2_hazard_uop0 = m2_reg_valids(0) && data_hazard_m2_uop0_p0 && m2_cannot_bypass_p0 || 
                          m2_reg_valids(1) && data_hazard_m2_uop0_p1 && m2_cannot_bypass_p1
  //----  Uop 1 ----
  val data_hazard_m2_uop1_p0 = m2_ctrls(0).wxd && checkHazards(hazard_targets(1), _ === m2_reg_uops(0).rd)
  val data_hazard_m2_uop1_p1 = m2_ctrls(1).wxd && checkHazards(hazard_targets(1), _ === m2_reg_uops(1).rd)
  val fp_data_hazard_m2 = id_ctrls(1).fp && m2_ctrls(1).wfd && checkHazards(fp_hazard_targets, _ === m2_reg_uops(1).rd)
  val id_m2_hazard_uop1 = m2_reg_valids(0) && data_hazard_m2_uop1_p0 && m2_cannot_bypass_p0 || 
                          m2_reg_valids(1) && (data_hazard_m2_uop1_p1 && m2_cannot_bypass_p1 || fp_data_hazard_m2)

  val id_stall = Wire(Vec(decodeWidthGpc, Bool()))
  val readys_swapped = WireDefault(VecInit(id_stall.map(!_)))
  when (!id_swap && !id_uops(1).ctrl.pipe1) { readys_swapped(1) := false.B }
  when (id_swap && !id_uops(0).ctrl.pipe0) { readys_swapped(0) := false.B }

  val readys_in = WireDefault(Mux(!id_swap, readys_swapped, VecInit(readys_swapped.reverse)))
  io.imem.resp zip readys_in map { case (imem, ready) => imem.ready := ready }
  // There must be no RAW/WAW between two uops
  val hazard_targets_origin = id_uops_origin map {hazard_targets_gen(_)}
  val dataHazard_between_origin_uops = checkHazards(hazard_targets_origin(1), _ === id_uops_origin(0).rd) && id_uops_origin(0).ctrl.wxd
  when (!id_swap && (!readys_swapped(0) || dataHazard_between_origin_uops)) {
    readys_in(1) := false.B
    id_valids(1) := false.B
  }
  when (id_swap && (!readys_swapped(1) || dataHazard_between_origin_uops)) {
    readys_in(1) := false.B
    id_valids(0) := false.B
  }
  
  //FIXME - the BreakpointUnit should support two issue paths
  val bpu = Module(new BreakpointUnit(nBreakpoints))
  bpu.io.status := csr.io.status
  bpu.io.bp := csr.io.bp
  bpu.io.pc := id_uops(0).pc //FIXME - 
  bpu.io.ea := m1_reg_wdata(0) //FIXME - 
  bpu.io.mcontext := csr.io.mcontext
  bpu.io.scontext := csr.io.scontext

  val io_imem_resp = io.imem.resp.map(_.bits)
  val id_imem_resp = Mux(!id_swap, VecInit(io_imem_resp), VecInit(io_imem_resp.reverse))
  val frontend_xcpt_swapped = id_imem_resp.map(_.xcpt)
  val id_illegal_insn = Wire(Vec(decodeWidthGpc, Bool()))
  val id_check_xcpt = (0 until 2) map { i =>
    checkExceptions(
      {if (i == 0) List((csr.io.interrupt, csr.io.interrupt_cause)) else Nil} ++: List(
      (bpu.io.debug_if,  CSR.debugTriggerCause.U),
      (bpu.io.xcpt_if,   Causes.breakpoint.U),
      (frontend_xcpt_swapped(i).pf.inst, Causes.fetch_page_fault.U),
      (frontend_xcpt_swapped(i).ae.inst, Causes.fetch_access.U),
      (id_illegal_insn(i),  Causes.illegal_instruction.U)))
  }
  val id_xcpt = id_check_xcpt.map(_._1)
  val id_cause = id_check_xcpt.map(_._2)
  //
  // ---- End of Final issue scheduling ----

  val id_csr_en = id_ctrls(0).csr.isOneOf(CSR.S, CSR.C, CSR.W)
  val id_system_insn = id_ctrls(0).csr === CSR.I
  val id_csr_ren = id_ctrls(0).csr.isOneOf(CSR.S, CSR.C) && id_uops(0).rs1 === 0.U
  val id_csr = Mux(id_system_insn && id_ctrls(0).mem, CSR.N, Mux(id_csr_ren, CSR.R, id_ctrls(0).csr))
  val id_csr_flush = id_system_insn || (id_csr_en && !id_csr_ren && csr.io.decode(0).write_flush)

  id_illegal_insn(0) := !id_ctrls(0).legal ||
    id_csr_en && (csr.io.decode(0).read_illegal || !id_csr_ren && csr.io.decode(0).write_illegal) ||
    !id_uops(0).rvc && (id_system_insn && csr.io.decode(0).system_illegal)
  id_illegal_insn(1) := !id_ctrls(1).legal ||
    id_ctrls(1).fp && (csr.io.decode(1).fp_illegal || io.fpu.illegal_rm) ||
    id_ctrls(1).dp && !csr.io.status.isa('d'-'a')

  val id_ren = id_ctrls.map(c => Seq(c.rxs1, c.rxs2))
  val id_raddr = id_uops.map(uop => Seq(uop.rs1, uop.rs2))
  val rf = new RegFile(31, xLen)
  val id_rs = id_raddr.map(raddr_onePipe => raddr_onePipe.map(rf.read _))

  // stall decode for fences (now, for AMO.rl; later, for AMO.aq and FENCE)
  val id_reg_fence = RegInit(false.B)
  val id_amo_aq = id_uops(0).inst(26)
  val id_amo_rl = id_uops(0).inst(25)
  val id_fence_pred = id_uops(0).inst(27,24)
  val id_fence_succ = id_uops(0).inst(23,20)
  val id_fence_next = id_ctrls(0).fence || id_ctrls(0).amo && id_amo_aq
  val id_mem_busy = !io.dmem.ordered || io.dmem.req.valid
  when (!id_mem_busy) { id_reg_fence := false.B }
  val id_do_fence = WireDefault(id_mem_busy && (id_ctrls(0).amo && id_amo_rl || id_ctrls(0).fence_i || id_reg_fence && id_ctrls(0).mem))

  val ctrl_killd = Wire(Vec(decodeWidthGpc, Bool()))
  ctrl_killd(0) := !id_valids(0) || !readys_swapped(0) || id_uops(0).replay || take_pc_all || id_stall(0) || csr.io.interrupt || vxcpt_flush
  ctrl_killd(1) := !id_valids(1) || !readys_swapped(1) || id_uops(1).replay || take_pc_all || id_stall(1) || csr.io.interrupt || vxcpt_flush

  /** EX stage
   */
  //---- RAW bypass: for R, find the nearest valid W. If W is not ready,
  //                 discard RAW bypass of this stage.
  val bypass_sources_all = Seq(
    (m1_reg_uops(0).rd, m1_reg_wdata(0), m1_reg_uops(0).ctrl.wxd, m1_reg_valids(0), m1_reg_uops(0).wdata_ready),
    (m1_reg_uops(1).rd, m1_reg_wdata(1), m1_reg_uops(1).ctrl.wxd, m1_reg_valids(1), m1_reg_uops(1).wdata_ready),
    (m2_reg_uops(0).rd, m2_reg_wdata(0), m2_reg_uops(0).ctrl.wxd, m2_reg_valids(0), m2_reg_uops(0).wdata_ready),
    (m2_reg_uops(1).rd, m2_reg_wdata(1), m2_reg_uops(1).ctrl.wxd, m2_reg_valids(1), m2_reg_uops(1).wdata_ready),
    (wb_reg_uops(0).rd, wb_reg_wdata(0), wb_reg_uops(0).ctrl.wxd, wb_reg_valids(0), wb_reg_uops(0).wdata_ready),
    (wb_reg_uops(1).rd, wb_reg_wdata(1), wb_reg_uops(1).ctrl.wxd, wb_reg_valids(1), wb_reg_uops(1).wdata_ready)
  )
  val bypass_sinks_ex = Seq(
    (ex_reg_uops(0).rs1, ex_reg_uops(0).rxs1),
    (ex_reg_uops(0).rs2, ex_reg_uops(0).rxs2),
    (ex_reg_uops(1).rs1, ex_reg_uops(1).rxs1),
    (ex_reg_uops(1).rs2, ex_reg_uops(1).rxs2)
  )
  val bypass_swaps_all = Seq(m1_reg_swap, m2_reg_swap, wb_reg_swap)
  // Select the youngest hit bit from two bits, output is one-hot 2-bit UInt
  def hit_sel_2[T <: Data](hits: Seq[Bool], swap: Seq[Bool]): UInt = {
    require(hits.size == 2 && swap.size == 1)
    Mux(!swap(0), Mux(hits(1) && hits(0), 1.U(2.W), hits.asUInt), Mux(hits(1) && hits(0), 2.U(2.W), hits.asUInt))
  }
  // Select the youngest hit bit from six bits, output is one-hot 6-bit UInt
  def hit_sel_6[T <: Data](hits: Seq[Bool], swap: Seq[Bool]): UInt = {
    require(hits.size == 6 && swap.size == 3)
    val hit_oneHot = Wire(Vec(3, UInt(2.W)))
    val (t0, t1, t2) = (hit_sel_2(hits.take(2), swap.take(1)), hit_sel_2(hits.take(4).drop(2), swap.take(2).drop(1)), hit_sel_2(hits.drop(4), swap.drop(2)))
    hit_oneHot(0) := t0
    hit_oneHot(1) := Mux(t0 =/= 0.U, 0.U, t1)
    hit_oneHot(2) := Mux(t0 =/= 0.U || t1 =/= 0.U, 0.U, t2)
    hit_oneHot.asUInt
  }
  def bypass_check(sink: (UInt, Bool), srcs: Seq[(UInt, UInt, Bool, Bool, Bool)], 
                   swap: Seq[Bool], hit_sel: (Seq[Bool], Seq[Bool]) => UInt): (Bool, Bool, UInt) = {
    val hits = srcs.map(src => src._1 === sink._1 && src._3 && src._4 && sink._2)
    val need_bypass = hits.reduce(_ || _)
    val hits_OH = hit_sel(hits, swap)
    val wdata_sel = Mux1H(hits_OH, srcs.map(_._2))
    val wdata_ready_sel = Mux1H(hits_OH, srcs.map(_._5))
    val rs_is_zero = sink._1 === 0.U && sink._2
    val hit_final = Mux(rs_is_zero, true.B, wdata_ready_sel)
    val wdata_sel_final = Mux(rs_is_zero, 0.U, wdata_sel)
    (need_bypass, hit_final, wdata_sel_final)
    // need_bypass: this operand need bypass
    // hit_final: whether the bypass path is hit
    // wdata_sel_final: the bypass data
  }
  val ex_p0_rs_byps = (0 to 1) map { i =>
    bypass_check(bypass_sinks_ex(i), bypass_sources_all, bypass_swaps_all, hit_sel_6) }
  val ex_p1_rs_byps = (2 to 3) map { i =>
    bypass_check(bypass_sinks_ex(i), bypass_sources_all, bypass_swaps_all, hit_sel_6) }
  val ex_p0_rs = (0 until 2) map { i =>
    Mux(ex_p0_rs_byps(i)._2, ex_p0_rs_byps(i)._3, ex_reg_rsdata(0)(i)) }
  val ex_p1_rs = (0 until 2) map { i =>
    Mux(ex_p1_rs_byps(i)._2, ex_p1_rs_byps(i)._3, ex_reg_rsdata(1)(i)) }
  val ex_imm = (0 until 2) map { i => ImmGen(ex_reg_uops(i).ctrl.sel_imm, ex_reg_uops(i).inst) }

  val ex_wdata_ready = Wire(Vec(2, Bool()))
  val ex_rs_ready = Wire(Vec(2, Vec(2, Bool())))
  ex_rs_ready(0) := VecInit.tabulate(2) { i => !ex_p0_rs_byps(i)._1 || ex_p0_rs_byps(i)._2 }
  ex_rs_ready(1) := VecInit.tabulate(2) { i => !ex_p1_rs_byps(i)._1 || ex_p1_rs_byps(i)._2 }
  ex_wdata_ready(0) := ex_rs_ready(0).reduce(_ && _) && ex_reg_uops(0).ctrl.alu
  ex_wdata_ready(1) := ex_rs_ready(1).reduce(_ && _) && ex_reg_uops(1).ctrl.alu
  
  //---- Pipe 0 of EX stage ----
  val ex_p0_op1 = MuxLookup(ex_reg_uops(0).ctrl.sel_alu1, 0.S)(Seq(
    A1_RS1 -> ex_p0_rs(0).asSInt,
    A1_PC -> ex_reg_uops(0).pc.asSInt))
  val ex_p0_op2 = MuxLookup(ex_reg_uops(0).ctrl.sel_alu2, 0.S)(Seq(
    A2_RS2 -> ex_p0_rs(1).asSInt,
    A2_IMM -> ex_imm(0),
    A2_SIZE -> Mux(ex_reg_uops(0).rvc, 2.S, 4.S)))
  val alu_p0 = Module(new ALU)
  alu_p0.io.dw := ex_reg_uops(0).ctrl.alu_dw
  alu_p0.io.fn := ex_reg_uops(0).ctrl.alu_fn
  alu_p0.io.in2 := ex_p0_op2.asUInt
  alu_p0.io.in1 := ex_p0_op1.asUInt
  
  //---- Pipe 1 of EX stage ----
  val ex_p1_op1 = MuxLookup(ex_reg_uops(1).ctrl.sel_alu1, 0.S)(Seq(
    A1_RS1 -> ex_p1_rs(0).asSInt,
    A1_PC -> ex_reg_uops(1).pc.asSInt))
  val ex_p1_op2 = MuxLookup(ex_reg_uops(1).ctrl.sel_alu2, 0.S)(Seq(
    A2_RS2 -> ex_p1_rs(1).asSInt,
    A2_IMM -> ex_imm(1),
    A2_SIZE -> Mux(ex_reg_uops(1).rvc, 2.S, 4.S)))
  val alu_p1 = Module(new ALU)
  alu_p1.io.dw := ex_reg_uops(1).ctrl.alu_dw
  alu_p1.io.fn := ex_reg_uops(1).ctrl.alu_fn
  alu_p1.io.in2 := ex_p1_op2.asUInt
  alu_p1.io.in1 := ex_p1_op1.asUInt

  //---- Branch/JAL(R) of EX stage ----
  val ex_br_ctrl = ex_reg_uops(1).ctrl
  val ex_jalx = ex_br_ctrl.jal || ex_br_ctrl.jalr
  val ex_cfi = ex_br_ctrl.branch || ex_br_ctrl.jal || ex_br_ctrl.jalr
  val ex_br_taken = alu_p1.io.cmp_out
  val ex_npc_predict = RegEnable(id_imem_resp(1).next_pc, !ctrl_killd(1))
  val ex_br_target = ex_reg_uops(1).pc.asSInt +
    Mux(ex_br_ctrl.branch, ImmGen(IMM_SB, ex_reg_uops(1).inst),
    Mux(ex_reg_uops(1).rvc, 2.S, 4.S))
  val ex_nl_pc = (ex_reg_uops(1).pc.asSInt + Mux(ex_reg_uops(1).rvc, 2.S, 4.S)).asUInt
  val ex_npc = (Mux(ex_jalx || ex_reg_sfence, encodeVirtualAddress(alu_p1.io.out, alu_p1.io.out).asSInt, 
                    ex_br_target) & (-2).S).asUInt
  val ex_wrong_npc = ex_wdata_ready(1) && ex_cfi &&
    Mux(ex_br_ctrl.branch, ex_br_taken =/= ex_reg_uops(1).btb_resp.bht.taken, ex_npc =/= ex_npc_predict)
  val ex_npc_misaligned = !csr.io.status.isa('c'-'a') && ex_npc(1) && !ex_reg_sfence
  val ex_cfi_taken = (ex_br_ctrl.branch && ex_br_taken) || ex_br_ctrl.jalr || ex_br_ctrl.jal
  val ex_direction_misprediction = ex_br_ctrl.branch && ex_br_taken =/= ex_reg_uops(1).btb_resp.bht.taken
  val ex_misprediction = ex_wrong_npc
  take_pc_ex_p1 := ex_reg_valids(1) && !ex_reg_uops(1).xcpt_noIntrp && (ex_misprediction || ex_reg_sfence)
 
  // multiplier and divider
  val div = Module(new MulDiv(if (pipelinedMul) mulDivParams.copy(mulUnroll = 0) else mulDivParams, width = xLen, aluFn = aluFn))
  div.io.req.valid := ex_reg_valids(1) && ex_reg_uops(1).ctrl.div
  div.io.req.bits.dw := ex_reg_uops(1).ctrl.alu_dw
  div.io.req.bits.fn := ex_reg_uops(1).ctrl.alu_fn
  div.io.req.bits.in1 := ex_p1_rs(0)
  div.io.req.bits.in2 := ex_p1_rs(1)
  div.io.req.bits.tag := ex_reg_uops(1).rd
  val mul = pipelinedMul.option {
    val m = Module(new PipelinedMultiplier(xLen, 2, aluFn = aluFn))
    m.io.req.valid := ex_reg_valids(1) && ex_reg_uops(1).ctrl.mul
    m.io.req.bits := div.io.req.bits
    m
  }

  val id_pc_valids = Seq(!ctrl_killd(0) || csr.io.interrupt || id_uops(0).replay,
                         !ctrl_killd(1) || id_uops(1).replay)
  for (i <- 0 until 2) {
    when (!ctrl_killd(i)) {
      ex_reg_uops(i) := id_uops(i)
      ex_reg_rsdata(i) := id_rs(i)
      ex_reg_next_pc := id_imem_resp(1).next_pc
      if (i == 0) {
        ex_reg_uops(i).ctrl.csr := id_csr
        when (id_ctrls(0).fence && id_fence_succ === 0.U) { id_reg_pause := true.B }
        when (id_fence_next) { id_reg_fence := true.B }
        ex_reg_flush_pipe := id_ctrls(0).fence_i || id_csr_flush
        ex_reg_mem_size := id_uops(0).inst(13, 12)
        when (id_ctrls(0).mem_cmd.isOneOf(M_SFENCE, M_FLUSH_ALL)) {
          ex_reg_mem_size := Cat(id_uops(0).rs2 =/= 0.U, id_uops(0).rs1 =/= 0.U)
        }
        if (tile.dcache.flushOnFenceI) {
          when (id_ctrls(0).fence_i) {
            ex_reg_mem_size := 0.U
          }
        }
      }
      when (id_xcpt(i)) {
        ex_reg_uops(i).ctrl.alu_fn := aluFn.FN_ADD
        ex_reg_uops(i).ctrl.alu_dw := DW_XPR
        ex_reg_uops(i).ctrl.sel_alu1 := A1_RS1 // badaddr := instruction
        ex_reg_uops(i).ctrl.sel_alu2 := A2_ZERO
        when (bpu.io.xcpt_if || frontend_xcpt_swapped(i).pf.inst ||
              frontend_xcpt_swapped(i).ae.inst) { // badaddr := PC  //FIXME - bpu.io.xcpt_if
          ex_reg_uops(i).ctrl.sel_alu1 := A1_PC
          ex_reg_uops(i).ctrl.sel_alu2 := A2_ZERO
        }
      }
    }
    when (id_pc_valids(i)) {
      ex_reg_cause(i) := id_cause(i)
      ex_reg_uops(i).inst := id_uops(i).inst
      ex_reg_uops(i).pc := id_uops(i).pc
      ex_reg_uops(i).btb_resp := id_uops(i).btb_resp
      if (i == 0) {
        ex_reg_sfence := usingVM.B && id_uops(0).ctrl.mem && id_uops(0).ctrl.mem_cmd === M_SFENCE
        ex_reg_wphit := bpu.io.bpwatch.map { bpw => bpw.ivalid(0) }
      }
    }
    // on pipeline flushes, cause mem_npc to hold the sequential npc, which
    // will drive the m2-stage npc mux
    when (ex_reg_valids(0) && ex_reg_flush_pipe) {
      ex_reg_sfence := false.B
    }
    ex_reg_valids(i) := !ctrl_killd(i)
    ex_reg_uops(i).replay := !take_pc_all && id_valids(i) && id_uops(i).replay
    ex_reg_uops(i).xcpt_noIntrp := !ctrl_killd(i) && id_xcpt(i)
    ex_reg_uops(i).interrupt := !take_pc_all && id_valids(i) && csr.io.interrupt
  }

  val ex_replay_intrp = Seq(ex_reg_uops(0).replay || ex_reg_uops(0).interrupt,
                            ex_reg_uops(1).replay)
  // replay inst in ex stage?
  val replay_ex_structural = Seq(ex_reg_uops(0).ctrl.mem && !io.dmem.req.ready,
                                 ex_reg_uops(1).ctrl.div && !div.io.req.ready)
  for (i <- 0 until 2) {
    ex_reg_uops(i).replay := ex_reg_uops(i).replay || ex_reg_valids(i) && replay_ex_structural(i)
  }
  val ctrl_killx_p0 = take_pc_m2 || ex_reg_uops(0).replay || !ex_reg_valids(0)
  val ctrl_killx_p1 = take_pc_m2 || ex_reg_uops(1).replay || !ex_reg_valids(1)
  val ctrl_killx = Wire(Vec(2, Bool()))
  ctrl_killx(0) := Mux(!ex_reg_swap, ctrl_killx_p0, ctrl_killx_p0 || ctrl_killx_p1 || take_pc_ex_p1) || vxcpt_flush
  ctrl_killx(1) := Mux(ex_reg_swap, ctrl_killx_p1, ctrl_killx_p0 || ctrl_killx_p1) || vxcpt_flush
  
  val ex_check_xcpt = (0 until 2) map { i =>
    checkExceptions(List(
      ({if (i==0) ex_reg_uops(i).interrupt else false.B} || ex_reg_uops(i).xcpt_noIntrp, ex_reg_cause(i)),
      (ex_reg_valids(0) && ex_npc_misaligned, Causes.misaligned_fetch.U)
  ))}
  val ex_xcpt = ex_check_xcpt.map(_._1)
  val ex_cause = ex_check_xcpt.map(_._2)

  //TODO - sfence handling

  /** M1 stage
   */
  // Bypass
  val bypass_sinks_m1 = Seq(
    (m1_reg_uops(0).rs1, m1_reg_uops(0).rxs1),
    (m1_reg_uops(0).rs2, m1_reg_uops(0).rxs2),
    (m1_reg_uops(1).rs1, m1_reg_uops(1).rxs1),
    (m1_reg_uops(1).rs2, m1_reg_uops(1).rxs2)
  )
  val m1_p0_rs_byps = (0 to 1) map { i =>
    bypass_check(bypass_sinks_m1(i), bypass_sources_all.drop(4), bypass_swaps_all.drop(2), hit_sel_2) }
  val m1_p1_rs_byps = (2 to 3) map { i =>
    bypass_check(bypass_sinks_m1(i), bypass_sources_all.drop(4), bypass_swaps_all.drop(2), hit_sel_2) }
  val m1_p0_rs = (0 until 2) map { i =>
    Mux(m1_p0_rs_byps(i)._2, m1_p0_rs_byps(i)._3, m1_reg_rsdata(0)(i)) }
  val m1_p1_rs = (0 until 2) map { i =>
    Mux(m1_p1_rs_byps(i)._2, m1_p1_rs_byps(i)._3, m1_reg_rsdata(1)(i)) }

  val m1_rs_ready = Wire(Vec(2, Vec(2, Bool())))
  m1_rs_ready(0) := VecInit.tabulate(2) { i => m1_reg_uops(0).rs_ready(i) || !m1_p0_rs_byps(i)._1 || m1_p0_rs_byps(i)._2 }
  m1_rs_ready(1) := VecInit.tabulate(2) { i => m1_reg_uops(1).rs_ready(i) || !m1_p1_rs_byps(i)._1 || m1_p1_rs_byps(i)._2 }

  val ex_pc_valids = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    ex_pc_valids(i) := !ctrl_killx(i) || ex_replay_intrp(i)
    when (ex_pc_valids(i)) {
      m1_reg_uops(i) := ex_reg_uops(i)
      m1_reg_uops(i).rs_ready := ex_rs_ready(i)
      m1_reg_uops(i).wdata_ready := ex_wdata_ready(i)
      m1_reg_cause(i) := ex_cause(i)
      if (i == 0) {
        m1_reg_flush_pipe := ex_reg_flush_pipe
        m1_reg_sfence := ex_reg_sfence
        m1_reg_mem_size := ex_reg_mem_size
        m1_reg_wphit := ex_reg_wphit
        m1_reg_load := ex_reg_uops(0).ctrl.mem && isRead(ex_reg_uops(0).ctrl.mem_cmd) 
        m1_reg_store := ex_reg_uops(0).ctrl.mem && isWrite(ex_reg_uops(0).ctrl.mem_cmd) 
      }
    }
    when (!ctrl_killx(i)) {
      m1_reg_rsdata(i) := { if (i == 0) ex_p0_rs else ex_p1_rs }
      m1_reg_next_pc := ex_reg_next_pc
    }
    when (!ctrl_killx(i) && ex_wdata_ready(i)) {
      // Todo: add branch instrn for wdata
      m1_reg_wdata(i) := { if (i == 0) alu_p0.io.out else Mux(ex_jalx, ex_nl_pc, alu_p1.io.out) }
    }
    m1_reg_valids(i) := !ctrl_killx(i)
    m1_reg_uops(i).replay := !take_pc_m2 && ex_reg_valids(i) && ex_reg_uops(i).replay
    m1_reg_uops(i).xcpt_noIntrp := !ctrl_killx(i) && ex_xcpt(i)
    m1_reg_uops(i).interrupt := !take_pc_m2 && ex_reg_uops(i).interrupt
  }

  val m1_breakpoint = (m1_reg_load && bpu.io.xcpt_ld) || (m1_reg_store && bpu.io.xcpt_st)
  val m1_debug_breakpoint = (m1_reg_load && bpu.io.debug_ld) || (m1_reg_store && bpu.io.debug_st)
  val (m1_ldst_xcpt, m1_ldst_cause) = checkExceptions(List(
    (m1_debug_breakpoint, CSR.debugTriggerCause.U),
    (m1_breakpoint,       Causes.breakpoint.U)))

  val m1_replay_intrp = Seq(m1_reg_uops(0).replay || m1_reg_uops(0).interrupt,
                            m1_reg_uops(1).replay)
  val m1_xcpt_cause = (0 until 2) map { i =>
    checkExceptions(List(
      ({if (i==0) m1_reg_uops(i).interrupt else false.B} || m1_reg_uops(i).xcpt_noIntrp, m1_reg_cause(i)),
      (m1_reg_valids(0) && m1_ldst_xcpt, m1_ldst_cause)
  ))}
  val m1_xcpt = m1_xcpt_cause.map(_._1)
  val m1_cause = m1_xcpt_cause.map(_._2)

  val dcache_kill_m1 = m1_reg_valids(0) && m1_reg_uops(0).ctrl.wxd && io.dmem.replay_next // structural hazard on writeback port
  val fpu_kill_m1 = m1_reg_valids(1) && m1_reg_uops(1).ctrl.fp && io.fpu.nack_mem
  val replay_m1 = Seq(dcache_kill_m1 || m1_reg_uops(0).replay, fpu_kill_m1 || m1_reg_uops(1).replay)
  val ctrl_killm1_p0 = dcache_kill_m1 || take_pc_m2 || !m1_reg_valids(0)
  val ctrl_killm1_p1 = fpu_kill_m1 || take_pc_m2 || !m1_reg_valids(1)
  val ctrl_killm1 = Wire(Vec(2, Bool()))
  ctrl_killm1(0) := Mux(!m1_reg_swap, ctrl_killm1_p0, ctrl_killm1_p0 || ctrl_killm1_p1) || vxcpt_flush
  ctrl_killm1(1) := Mux(m1_reg_swap, ctrl_killm1_p1, ctrl_killm1_p0 || ctrl_killm1_p1 || m1_reg_flush_pipe) || vxcpt_flush

  /** M2 stage
   */
  // Bypass
  val bypass_sinks_m2 = Seq(
    (m2_reg_uops(0).rs1, m2_reg_uops(0).rxs1),
    (m2_reg_uops(0).rs2, m2_reg_uops(0).rxs2),
    (m2_reg_uops(1).rs1, m2_reg_uops(1).rxs1),
    (m2_reg_uops(1).rs2, m2_reg_uops(1).rxs2)
  )
  val m2_p0_rs_byps = (0 to 1) map { i =>
    bypass_check(bypass_sinks_m2(i), bypass_sources_all.drop(4), bypass_swaps_all.drop(2), hit_sel_2) }
  val m2_p1_rs_byps = (2 to 3) map { i =>
    bypass_check(bypass_sinks_m2(i), bypass_sources_all.drop(4), bypass_swaps_all.drop(2), hit_sel_2) }
  val m2_p0_rs = (0 until 2) map { i =>
    Mux(m2_p0_rs_byps(i)._2, m2_p0_rs_byps(i)._3, m2_reg_rsdata(0)(i)) }
  val m2_p1_rs = (0 until 2) map { i =>
    Mux(m2_p1_rs_byps(i)._2, m2_p1_rs_byps(i)._3, m2_reg_rsdata(1)(i)) }
  val m2_imm = (0 until 2) map { i => ImmGen(m2_reg_uops(i).ctrl.sel_imm, m2_reg_uops(i).inst) }

  val m2_rs_ready = Wire(Vec(2, Vec(2, Bool())))
  m2_rs_ready(0) := VecInit.tabulate(2) { i => m2_reg_uops(0).rs_ready(i) || !m2_p0_rs_byps(i)._1 || m2_p0_rs_byps(i)._2 }
  m2_rs_ready(1) := VecInit.tabulate(2) { i => m2_reg_uops(1).rs_ready(i) || !m2_p1_rs_byps(i)._1 || m2_p1_rs_byps(i)._2 }
  // To handle load-use on miss: if need bypass but not hit at all stages, replay at M2 stage.
  val replay_m2_load_use = m2_rs_ready map {_.asUInt =/= 3.U}
  
  //---- Pipe 0 of M2 stage ----
  val m2_p0_op1 = MuxLookup(m2_reg_uops(0).ctrl.sel_alu1, 0.S)(Seq(
    A1_RS1 -> m2_p0_rs(0).asSInt,
    A1_PC -> m2_reg_uops(0).pc.asSInt))
  val m2_p0_op2 = MuxLookup(m2_reg_uops(0).ctrl.sel_alu2, 0.S)(Seq(
    A2_RS2 -> m2_p0_rs(1).asSInt,
    A2_IMM -> m2_imm(0),
    A2_SIZE -> Mux(m2_reg_uops(0).rvc, 2.S, 4.S)))
  val aluM2_p0 = Module(new ALU)
  aluM2_p0.io.dw := m2_reg_uops(0).ctrl.alu_dw
  aluM2_p0.io.fn := m2_reg_uops(0).ctrl.alu_fn
  aluM2_p0.io.in2 := m2_p0_op2.asUInt
  aluM2_p0.io.in1 := m2_p0_op1.asUInt

  //---- Pipe 1 of M2 stage ----
  val m2_p1_op1 = MuxLookup(m2_reg_uops(1).ctrl.sel_alu1, 0.S)(Seq(
    A1_RS1 -> m2_p1_rs(0).asSInt,
    A1_PC -> m2_reg_uops(1).pc.asSInt))
  val m2_p1_op2 = MuxLookup(m2_reg_uops(1).ctrl.sel_alu2, 0.S)(Seq(
    A2_RS2 -> m2_p1_rs(1).asSInt,
    A2_IMM -> m2_imm(1),
    A2_SIZE -> Mux(m2_reg_uops(1).rvc, 2.S, 4.S)))
  val aluM2_p1 = Module(new ALU)
  aluM2_p1.io.dw := m2_reg_uops(1).ctrl.alu_dw
  aluM2_p1.io.fn := m2_reg_uops(1).ctrl.alu_fn
  aluM2_p1.io.in2 := m2_p1_op2.asUInt
  aluM2_p1.io.in1 := m2_p1_op1.asUInt

  //---- Branch/JAL(R) of M2 stage ----
  val m2_br_ctrl = m2_reg_uops(1).ctrl
  val m2_jalx = m2_br_ctrl.jal || m2_br_ctrl.jalr
  val m2_cfi = m2_br_ctrl.branch || m2_br_ctrl.jal || m2_br_ctrl.jalr
  val m2_br_taken = aluM2_p1.io.cmp_out
  val m2_npc_predict = RegEnable(m1_reg_next_pc, !ctrl_killm1(1))
  val m2_br_target = m2_reg_uops(1).pc.asSInt +
    Mux(m2_br_ctrl.branch, ImmGen(IMM_SB, m2_reg_uops(1).inst),
    Mux(m2_reg_uops(1).rvc, 2.S, 4.S))
  val m2_nl_pc = (m2_reg_uops(1).pc.asSInt + Mux(m2_reg_uops(1).rvc, 2.S, 4.S)).asUInt
  val m2_npc = (Mux(m2_jalx, encodeVirtualAddress(aluM2_p1.io.out, aluM2_p1.io.out).asSInt,
                    m2_br_target) & (-2).S).asUInt
  val m2_wrong_npc = m2_cfi &&
    Mux(m2_br_ctrl.branch, m2_br_taken =/= m2_reg_uops(1).btb_resp.bht.taken, m2_npc =/= m2_npc_predict)
  val m2_npc_misaligned = !csr.io.status.isa('c'-'a') && m2_npc(1)
  val m2_cfi_taken = (m2_br_ctrl.branch && m2_br_taken) || m2_br_ctrl.jalr || m2_br_ctrl.jal
  val m2_direction_misprediction = m2_br_ctrl.branch && m2_br_taken =/= m2_reg_uops(1).btb_resp.bht.taken
  val m2_misprediction = m2_wrong_npc
  
  val m2_alu_wdata_p1 = Mux(m2_jalx, m2_nl_pc, aluM2_p1.io.out)

  val m2_pc_valids = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    m2_pc_valids(i) := !ctrl_killm1(i) || m1_replay_intrp(i)
    when (m2_pc_valids(i)) {
      m2_reg_uops(i) := m1_reg_uops(i)
      m2_reg_uops(i).rs_ready := m1_rs_ready(i)
      m2_reg_uops(i).wdata_ready := m1_reg_uops(i).wdata_ready
      m2_reg_cause(i) := m1_reg_cause(i)
      if (i == 0) {
        m2_reg_sfence := m1_reg_sfence
        m2_reg_mem_size := m1_reg_mem_size
        m2_reg_wphit := m1_reg_wphit | bpu.io.bpwatch.map { bpw => (bpw.rvalid(0) && m1_reg_load) || (bpw.wvalid(0) && m1_reg_store) }
      }
    }
    when (!ctrl_killm1(i)) {
      m2_reg_rsdata(i) := { if (i == 0) m1_p0_rs else m1_p1_rs }
      m2_reg_wdata(i) := { if (i == 0) m1_reg_wdata(0) else
        Mux(!m2_reg_xcpt(1) && m2_reg_uops(1).ctrl.fp && m2_reg_uops(1).ctrl.wxd,
            io.fpu.toint_data, m1_reg_wdata(1)) }
    }
    m2_reg_valids(i) := !ctrl_killm1(i)
    m2_reg_uops(i).replay := !take_pc_m2 && replay_m1(i)
    m2_reg_xcpt(i) := m1_xcpt(i) && !take_pc_m2
  }
  m2_reg_flush_pipe := !ctrl_killm1(0) && m1_reg_flush_pipe

  val vxcpt_cause = Reg(new VLdstXcpt)
  val vxcpt_cause_case = Mux1H(Seq(
    vxcpt_cause.ma.ld -> Causes.misaligned_load.U,
    vxcpt_cause.ma.st -> Causes.misaligned_store.U,
    vxcpt_cause.pf.ld -> Causes.load_page_fault.U,
    vxcpt_cause.pf.st -> Causes.store_page_fault.U,
    vxcpt_cause.ae.ld -> Causes.load_access.U,
    vxcpt_cause.ae.st -> Causes.store_access.U,
  ))
  val m2_xcpt_cause_p0 = checkExceptions(List(
    (vxcpt_flush,  vxcpt_cause_case),
    (m2_reg_xcpt(0),  m2_reg_cause(0)),
    (m2_reg_valids(0) && m2_reg_uops(0).ctrl.mem && io.dmem.s2_xcpt.pf.st, Causes.store_page_fault.U),
    (m2_reg_valids(0) && m2_reg_uops(0).ctrl.mem && io.dmem.s2_xcpt.pf.ld, Causes.load_page_fault.U),
    (m2_reg_valids(0) && m2_reg_uops(0).ctrl.mem && io.dmem.s2_xcpt.ae.st, Causes.store_access.U),
    (m2_reg_valids(0) && m2_reg_uops(0).ctrl.mem && io.dmem.s2_xcpt.ae.ld, Causes.load_access.U),
    (m2_reg_valids(0) && m2_reg_uops(0).ctrl.mem && io.dmem.s2_xcpt.ma.st, Causes.misaligned_store.U),
    (m2_reg_valids(0) && m2_reg_uops(0).ctrl.mem && io.dmem.s2_xcpt.ma.ld, Causes.misaligned_load.U)
  ))
  val m2_xcpt = Seq(m2_xcpt_cause_p0._1, m2_reg_xcpt(1))
  val m2_cause = Seq(m2_xcpt_cause_p0._2, m2_reg_cause(1))

  val m2_dcache_miss = m2_reg_uops(0).ctrl.mem && !io.dmem.resp.valid
  val m2_wdata_ready = WireDefault(VecInit(!m2_dcache_miss, true.B))

  val replay_m2_csr = m2_reg_valids(0) && csr.io.rw_stall
  val replay_m2 = Seq(io.dmem.s2_nack || replay_m2_csr || replay_m2_load_use(0) || m2_reg_uops(0).replay,
                      replay_m2_load_use(1) ||  m2_reg_uops(1).replay)
  take_pc_m2_p0 := replay_m2(0) || m2_xcpt(0) || csr.io.eret || m2_reg_flush_pipe
  val take_pc_m2_cfi = m2_reg_valids(1) && m2_misprediction //TODO - sfence adding
  val take_pc_m2_p1_others = replay_m2(1) || m2_xcpt(1)
  take_pc_m2_p1 := take_pc_m2_cfi || take_pc_m2_p1_others

  // M2 write arbitration
  val dmem_resp_xpu = !io.dmem.resp.bits.tag(0).asBool
  val dmem_resp_fpu =  io.dmem.resp.bits.tag(0).asBool
  val dmem_resp_waddr = io.dmem.resp.bits.tag(5, 1)
  val dmem_resp_valid = io.dmem.resp.valid && io.dmem.resp.bits.has_data
  val dmem_resp_replay = dmem_resp_valid && io.dmem.resp.bits.replay
  val ll_wen_mem_p0 = dmem_resp_replay && dmem_resp_xpu
  val ll_waddr_mem_p0 = dmem_resp_waddr
  val m2_wxd_p0 = m2_reg_valids(0) && m2_reg_uops(0).ctrl.wxd
  io.vwb_ready.wb_int_ready := !m2_wxd_p0
  when (ll_wen_mem_p0) { io.vwb_ready.wb_int_ready := false.B }
  val ll_wen_fire_v_int = io.vwb_ready.wb_int_ready && io.vcomplete.wen_int
  val ll_wen_p0 = ll_wen_mem_p0 || ll_wen_fire_v_int
  val ll_waddr_p0 = Mux(ll_wen_mem_p0, dmem_resp_waddr, io.vcomplete.wdata_reg_idx)

  val m2_wxd_p1 = m2_reg_valids(1) && m2_reg_uops(1).ctrl.wxd
  div.io.resp.ready := !m2_wxd_p1
  val ll_wdata_p1 = div.io.resp.bits.data
  val ll_waddr_p1 = div.io.resp.bits.tag
  val ll_wen_p1 = div.io.resp.fire

  val m2_wdata_p0 = Mux(ll_wen_mem_p0, io.dmem.resp.bits.data(xLen-1, 0),
                    Mux(ll_wen_fire_v_int, io.vcomplete.wdata,
                    Mux(m2_reg_uops(0).ctrl.csr =/= CSR.N, csr.io.rw.rdata,
                        aluM2_p0.io.out)))
  val m2_waddr_p0 = Mux(ll_wen_p0, ll_waddr_p0, m2_reg_uops(0).rd)
  val m2_wdata_p1 = Mux(ll_wen_p1, ll_wdata_p1,
                    Mux(m2_reg_uops(1).ctrl.mul, mul.map(_.io.resp.bits.data).getOrElse(m2_alu_wdata_p1),
                        m2_alu_wdata_p1))
  val m2_waddr_p1 = Mux(ll_wen_p1, ll_waddr_p1, m2_reg_uops(1).rd)

  val ctrl_killm2_p0 = replay_m2(0) || !m2_reg_valids(0)
  val ctrl_killm2_p1 = replay_m2(1) || !m2_reg_valids(1)
  val ctrl_killm2 = Wire(Vec(2, Bool()))
  ctrl_killm2(0) := Mux(!m2_reg_swap, ctrl_killm2_p0, ctrl_killm2_p0 || ctrl_killm2_p1 || m2_xcpt(1)) || vxcpt_flush
  ctrl_killm2(1) := Mux(m2_reg_swap, ctrl_killm2_p1, ctrl_killm2_p0 || ctrl_killm2_p1 || csr.io.eret || m2_xcpt(0)) || vxcpt_flush
  div.io.kill := ctrl_killm1(1) && RegNext(div.io.req.fire) ||
                 ctrl_killm2(1) && RegNext(RegNext(div.io.req.fire))
  val m2_valids = Seq.tabulate(2)(i => m2_reg_valids(i) && !replay_m2(i) && !m2_xcpt(i))

  // Busy Table set/clear
  val m2_set_busyTable_p0 = (m2_dcache_miss || m2_reg_uops(0).vec) &&
                            m2_reg_valids(0) && m2_reg_uops(0).ctrl.wxd
  val m2_set_busyTable_p1 = m2_reg_uops(1).ctrl.div && m2_wxd_p1
  val busyTable = new BusyTable(zero = true)
  busyTable.clear(Seq(ll_wen_p0, ll_wen_p1), Seq(m2_waddr_p0, m2_waddr_p1))
  busyTable.set(Seq(m2_set_busyTable_p0 && !ctrl_killm2(0), m2_set_busyTable_p1 && !ctrl_killm2(1)), 
                Seq(m2_reg_uops(0).rd, m2_reg_uops(1).rd))
  def id_busytable_clear_bypass(rdaddr: UInt) = {
    ll_wen_p0 && ll_waddr_p0 === rdaddr || ll_wen_p1 && ll_waddr_p1 === rdaddr
  }
  val id_stall_fpu = if (usingFPU) {
    val fp_busyTable = new BusyTable
    fp_busyTable.set(Seq((m2_dcache_miss && m2_reg_uops(1).ctrl.wfd || io.fpu.sboard_set || m2_reg_uops(1).vec_arith && m2_reg_uops(1).ctrl.wfd) &&
                          m2_reg_valids(1)), Seq(m2_reg_uops(1).rd))
    fp_busyTable.clear(Seq(dmem_resp_replay && dmem_resp_fpu, io.fpu.sboard_clr, io.vwb_ready.wb_fp_ready && io.vcomplete.wen_fp),
                       Seq(dmem_resp_waddr, io.fpu.sboard_clra, io.vcomplete.wdata_reg_idx))
    checkHazards(fp_hazard_targets, fp_busyTable.read _)
  } else false.B

  // Vector Scoreboard
  class VScoreboardEntry extends Bundle {
    val valid = Bool()
    val wb = Bool() // write-back (i.e. complete)
    val no_illegal_xcpt = Bool()
    val no_mem_xcpt = Bool()
    val inst = UInt(32.W)
    val pc = UInt(vaddrBitsExtended.W)
  }
  val vsb = Reg(Vec(VScoreboardSize, new VScoreboardEntry))
  for (i <- 0 until VScoreboardSize) {
    when (reset.asBool) {
      vsb(i).valid := false.B
      vsb(i).wb := false.B
    }
  }
  class VsbPtr extends CircularQueuePtr[VsbPtr](VScoreboardSize)
  val enqPtrVsb = RegInit(0.U.asTypeOf(new VsbPtr))
  val deqPtrVsb = RegInit(0.U.asTypeOf(new VsbPtr))
  val vsb_almost_full = hasFreeEntries(enqPtrVsb, deqPtrVsb) <= 3.U

  //---- Some M2 utils ----                         hit   p0/p1
  def swap_select(swap: Bool, v0: Bool, v1: Bool): (Bool, Bool) = (v0 || v1, Mux(!swap, !v0, v1))
  def swap_select(swap: Bool, v: Seq[Bool]): (Bool, Bool) = swap_select(swap, v(0), v(1))  
  
  val m2_xcpt_select = swap_select(m2_reg_swap, Seq.tabulate(2)(i => !ctrl_killm2(i) && m2_xcpt(i)))
  def m2_xcpt_data_select[T <: Data](d: Seq[T]) = Mux(m2_xcpt_select._2, d(1), d(0))
  val m2_cause_select = m2_xcpt_data_select(m2_cause)
  val m2_pc_xcpt_select = Mux(vxcpt_flush, vsb(deqPtrVsb.value).pc, m2_xcpt_data_select(m2_reg_uops.map(_.pc)))

  val m2_replay_select = swap_select(m2_reg_swap, Seq.tabulate(2)(i => replay_m2(i)))
  def m2_replay_data_select[T <: Data](d: Seq[T]) = Mux(m2_replay_select._2, d(1), d(0))
  val m2_pc_replay_select = m2_replay_data_select(m2_reg_uops.map(_.pc))

  //---- CSR IO ----
  csr.io.ungated_clock := clock
  csr.io.decode(0).inst := id_uops(0).inst
  csr.io.decode(1).inst := id_uops(1).inst
  csr.io.exception := m2_xcpt_select._1
  csr.io.cause := m2_cause_select
  csr.io.retire := PopCount(Seq.tabulate(2)(i => m2_valids(i) && !ctrl_killm2(i)))
  csr.io.inst(0) := m2_reg_uops(0).inst  // Only for trace ?
  csr.io.inst(1) := m2_reg_uops(1).inst  // Only for trace ?
  csr.io.interrupts := io.interrupts
  csr.io.hartid := io.hartid
  io.fpu.fcsr_rm := csr.io.fcsr_rm
  csr.io.fcsr_flags := io.fpu.fcsr_flags
  io.fpu.time := csr.io.time(31,0)
  io.fpu.hartid := io.hartid
  csr.io.pc := m2_pc_xcpt_select
  val tval_dmem_addr = !m2_reg_xcpt(0)
  val tval_any_addr = tval_dmem_addr ||
    m2_reg_cause(0).isOneOf(Causes.breakpoint.U, Causes.fetch_access.U, Causes.fetch_page_fault.U, Causes.fetch_guest_page_fault.U)
  val tval_inst = m2_reg_cause(0) === Causes.illegal_instruction.U
  val tval_valid = csr.io.exception && (tval_any_addr || tval_inst)
  // FIXME - real value of tval
  csr.io.tval := Mux(tval_valid, encodeVirtualAddress(m2_reg_wdata(0), m2_reg_wdata(0)), 0.U)
  csr.io.gva := false.B
  csr.io.htval := false.B

  io.ptw := DontCare
  //TODO - Recheck ptw ports
  io.ptw.ptbr := csr.io.ptbr
  io.ptw.hgatp := csr.io.hgatp
  io.ptw.vsatp := csr.io.vsatp
  (io.ptw.customCSRs.csrs zip csr.io.customCSRs).map { case (lhs, rhs) => lhs <> rhs }
  io.ptw.status := csr.io.status
  io.ptw.hstatus := csr.io.hstatus
  io.ptw.gstatus := csr.io.gstatus
  io.ptw.pmp := csr.io.pmp

  val m2_valid_final_p0 = m2_reg_valids(0) && !(m2_reg_swap && m2_xcpt(1))
  csr.io.rw.addr := m2_reg_uops(0).inst(31,20)
  csr.io.rw.cmd := CSR.maskCmd(m2_valid_final_p0, m2_reg_uops(0).ctrl.csr)
  csr.io.rw.wdata := m2_reg_wdata(0)

  def vlmul_mag(vlmul: UInt): UInt = Mux(vlmul(2) && vlmul(0), Cat(!vlmul(1), true.B), vlmul(1, 0))
  def mag_to_vlmul(sign: Bool, mag: UInt) = Mux(sign && mag(0), Cat(sign, !mag(1), true.B), Cat(sign, mag))
  // vector csr write by v_config
  csr.io.vector.foreach { csr_vio =>
    csr_vio.set_vs_dirty := false.B //FIXME -
    csr_vio.set_vstart.valid := false.B //FIXME -
    csr_vio.set_vstart.bits := 0.U //FIXME -
    csr_vio.set_vxsat := false.B //FIXME - 
    csr_vio.set_vconfig.valid := m2_valid_final_p0 && m2_reg_uops(0).vec_config
    val cfig_case = m2_reg_uops(0).vec_config_case
    val vtype = Mux(cfig_case(2), m2_reg_uops(0).inst(30, 20),
                Mux(cfig_case(1), m2_reg_uops(0).inst(29, 20), m2_reg_uops(0).rs2))
    val vtype_vlmul_mag = Cat(vtype(vtype.getWidth - 1, 2), vlmul_mag(vtype(2, 0)))
    val avl = Mux(cfig_case(1), m2_reg_uops(0).inst(19, 15), m2_reg_uops(0).rs1)
    csr_vio.set_vconfig.bits.vtype := VType.fromUInt(vtype_vlmul_mag, true)
    // csr_vio.set_vconfig.bits.vl := csr_vio.set_vconfig.bits.vtype.vl(avl, 0.U, false.B, true.B, false.B)
    csr_vio.set_vconfig.bits.vl := 1.U //FIXME -
  }

  // vector csr read by vector instrn
  val m2_vissue = Wire(new VIssue)
  m2_vissue.valid := m2_valid_final_p0 && m2_reg_uops(0).vec
  m2_vissue.vsb_id := enqPtrVsb.asUInt
  m2_vissue.inst := m2_reg_uops(0).inst
  m2_vissue.rs1 := m2_reg_rsdata(0)(0)
  m2_vissue.rs2 := m2_reg_rsdata(0)(1)
  csr.io.vector.foreach { csr_vio =>
    m2_vissue.vcsr.vstart := csr_vio.vstart
    m2_vissue.vcsr.vxrm := csr_vio.vxrm
    m2_vissue.vcsr.frm := csr.io.fcsr_rm
    m2_vissue.vcsr.vl := csr_vio.vconfig.vl
    m2_vissue.vcsr.vill := csr_vio.vconfig.vtype.vill
    m2_vissue.vcsr.vma := csr_vio.vconfig.vtype.vma
    m2_vissue.vcsr.vta := csr_vio.vconfig.vtype.vta
    m2_vissue.vcsr.vsew := csr_vio.vconfig.vtype.vsew
    m2_vissue.vcsr.vlmul := mag_to_vlmul(csr_vio.vconfig.vtype.vlmul_sign, csr_vio.vconfig.vtype.vlmul_mag)
  }

  //---- V scoreboard operations ----
  // Enq
  when (m2_vissue.valid) {
    vsb(enqPtrVsb.value).valid := true.B
    vsb(enqPtrVsb.value).wb := false.B
    vsb(enqPtrVsb.value).no_illegal_xcpt := false.B
    vsb(enqPtrVsb.value).no_mem_xcpt := m2_reg_uops(0).vec_arith
    vsb(enqPtrVsb.value).inst := m2_reg_uops(0).inst
    vsb(enqPtrVsb.value).pc := m2_reg_uops(0).pc
    enqPtrVsb := enqPtrVsb + 1.U
  }
  // Update no_illegal_xcpt
  when (io.villegal.valid) {
    for (i <- 0 until VScoreboardSize) {
      when (i.U === io.villegal.vsb_id(bVScoreboardId - 2, 0)) {
        vsb(i).no_illegal_xcpt := vsb(i).no_illegal_xcpt || io.villegal.not_illegal
      }
    }
  }
  // Complete
  when (io.vcomplete.valid) {
    vsb(io.vcomplete.vsb_id(bVScoreboardId - 2, 0)).wb := true.B
    for (i <- 0 until VScoreboardSize) {
      when (i.U === io.vcomplete.vsb_id(bVScoreboardId - 2, 0)) {
        vsb(i).no_mem_xcpt := vsb(i).no_mem_xcpt || !io.vcomplete.mem_xcpt_valid
      }
    }
  }
  // Deq
  when (vsb(deqPtrVsb.value).wb) {
    deqPtrVsb := deqPtrVsb + 1.U
    vsb(deqPtrVsb.value).valid := false.B
  }
  // Vector exception tracking
  val vxcpt_ptr = RegInit(0.U.asTypeOf(new VsbPtr))
  val vxcpt_type = Reg(Bool()) // 0: illegal instrn   1: mem xcpt
  val s_vsb_idle :: s_vsb_xcpt :: Nil = Enum(2)
  val state_vsb = RegInit(s_vsb_idle)
  val vcomplete_xcpt = io.vcomplete.valid && (io.vcomplete.illegal_inst || io.vcomplete.mem_xcpt_valid)
  when (state_vsb === s_vsb_idle) {
    when (vcomplete_xcpt) { state_vsb := s_vsb_xcpt }
  }.otherwise {
    when (vxcpt_flush) { state_vsb := s_vsb_idle }
  }
  val vxcpt_valid = state_vsb === s_vsb_xcpt
  val new_vxcpt_ptr = io.vcomplete.vsb_id.asTypeOf(new VsbPtr)
  when (vcomplete_xcpt) {
    when (state_vsb === s_vsb_idle || new_vxcpt_ptr < vxcpt_ptr) {
      vxcpt_ptr := new_vxcpt_ptr
    }
    vxcpt_type := io.vcomplete.mem_xcpt_valid
    vxcpt_cause := io.vcomplete.mem_xcpt_cause
  }
  // Flush from vector scorebord
  vxcpt_flush := vxcpt_valid && vsb(deqPtrVsb.value).valid && deqPtrVsb === vxcpt_ptr
  when (vxcpt_flush) {
    deqPtrVsb := enqPtrVsb // Empty the scoreboard
    vsb.foreach(_.valid := false.B)
  }

  // Vector instrn stall the ID
  val pipe_has_vec = Seq(ex_reg_uops(0), m1_reg_uops(0), m2_reg_uops(0)).map(uop =>
                         uop.vec).reduce(_ || _)
  val vsb_all_noXcpt = vsb.map(v => !v.valid || v.valid && v.no_illegal_xcpt && v.no_mem_xcpt).reduce(_ && _)
  val vsb_stall_id = !id_uops(0).vec && (pipe_has_vec || !vsb_all_noXcpt)

  // TODO: trace -> debugROB, need swap
  io.trace.time := DontCare
  io.trace.insns := DontCare

  //TODO - io.bpwatch
  io.bpwatch := DontCare

  /** Check RAW/WAW data hazard for:
   *    ID <--> busy table     */
  val id_busytable_hazard_uop0 = checkHazards(hazard_targets(0), rd => busyTable.read(rd) && !id_busytable_clear_bypass(rd))
  val id_busytable_hazard_uop1 = checkHazards(hazard_targets(1), rd => busyTable.read(rd) && !id_busytable_clear_bypass(rd))

  //---- id_stall ----
  val stall_singleStep = csr.io.singleStep && (ex_reg_valids.orR || m1_reg_valids.orR || m2_reg_valids.orR)
  id_stall(0) := id_ex_hazard_uop0 || id_m1_hazard_uop0 || id_m2_hazard_uop0 || id_busytable_hazard_uop0 ||
                 stall_singleStep ||
                 !clock_en ||
                 id_do_fence ||
                 csr.io.csr_stall ||
                 id_reg_pause ||
                 vsb_almost_full && id_uops(0).vec ||
                 vsb_stall_id
  id_stall(1) := id_ex_hazard_uop1 || id_m1_hazard_uop1 || id_m2_hazard_uop1 || id_busytable_hazard_uop1 ||
                 stall_singleStep ||
                 id_csr_en && csr.io.decode(0).fp_csr && !io.fpu.fcsr_rdy ||
                 id_ctrls(1).fp && id_stall_fpu ||
                 id_ctrls(1).div && (!(div.io.req.ready || (div.io.resp.valid && !m2_wxd_p1)) || div.io.req.valid) || // reduce odds of replay
                 !clock_en ||
                 csr.io.csr_stall ||
                 id_reg_pause
  
  /** WB stage
   */
  val ll_wen_wb = RegNext(VecInit(ll_wen_p0, ll_wen_p1))
  val rf_wen = Wire(Vec(2, Bool()))
  val wb_reg_waddr = Reg(Vec(2, chiselTypeOf(m2_waddr_p0)))
  when (!ctrl_killm2(0) || ll_wen_p0) {
    wb_reg_wdata(0) := m2_wdata_p0
    wb_reg_waddr(0) := m2_waddr_p0
  }
  when (!ctrl_killm2(1) || ll_wen_p1) {
    wb_reg_wdata(1) := m2_wdata_p1
    wb_reg_waddr(1) := m2_waddr_p1
  }
  for (i <- 0 until 2) {
    when (!ctrl_killm2(i)) {
      wb_reg_uops(i) := m2_reg_uops(i)
      wb_reg_uops(i).ctrl.wxd := m2_reg_uops(i).ctrl.wxd
    }
    wb_reg_valids(i) := m2_valids(i) && !ctrl_killm2(i)
    rf_wen(i) := wb_reg_valids(i) && wb_reg_uops(i).ctrl.wxd || ll_wen_wb(i)
    when (rf_wen(i)) { rf.write(wb_reg_waddr(i), wb_reg_wdata(i)) }
  }
  when (!ctrl_killm2(0) && m2_vissue.valid) {
    wb_reg_vissue := m2_vissue
  }
  wb_reg_vissue.valid := m2_vissue.valid
  io.vissue := wb_reg_vissue

  io.imem.req.valid := take_pc_all
  io.imem.req.bits.speculative := take_pc_ex_p1 || take_pc_m2_cfi
  io.imem.req.bits.pc :=
    Mux(csr.io.exception || csr.io.eret, csr.io.evec, // exception or [m|s]ret
    Mux(m2_replay_select._1,    m2_pc_replay_select,   // replay
    Mux(take_pc_ex_p1, ex_npc, m2_npc))) //FIXME - m2_npc   // flush or branch misprediction
  io.imem.flush_icache := m2_reg_valids(0) && m2_reg_uops(0).ctrl.fence_i && !io.dmem.s2_nack
  io.imem.might_request := {
    imem_might_request_reg := ex_reg_valids.orR || m1_reg_valids.orR || io.ptw.customCSRs.disableICacheClockGate
    imem_might_request_reg  //TODO - ex_reg_valid --> ex_pc_valid
  }
  io.imem.progress := RegNext(m2_reg_valids(0) && !replay_m2(0) ||
                              m2_reg_valids(1) && !replay_m2(1))  //REVIEW - why !replay_wb_common (in Rocket)
  
  io.imem.sfence.bits := DontCare

  io.imem.sfence.valid := m2_reg_valids(0) && m2_reg_sfence
  io.imem.sfence.bits.rs1 := m2_reg_mem_size(0)
  io.imem.sfence.bits.rs2 := m2_reg_mem_size(1)
  io.imem.sfence.bits.addr := m2_reg_wdata(0)
  io.imem.sfence.bits.asid := m2_reg_rsdata(0)(1)
  io.ptw.sfence := io.imem.sfence

  val m2_take_pc_select = swap_select(m2_reg_swap, take_pc_m2_p0, take_pc_m2_p1)
  val m2_take_pc_is_p1 = m2_take_pc_select._1 && m2_take_pc_select._2
  val mispredict_m2 = m2_take_pc_is_p1 && !take_pc_m2_p1_others
  val mispredict_hit = take_pc_ex_p1 || mispredict_m2
  val final_cfi = Mux(mispredict_m2, m2_cfi, ex_cfi)
  val final_cfi_taken = Mux(mispredict_m2, m2_cfi_taken, ex_cfi_taken)
  val final_cfi_uop = Mux(mispredict_m2, m2_reg_uops(1), ex_reg_uops(1))
  val final_br_taken = Mux(mispredict_m2, m2_br_taken, ex_br_taken)

  
  io.imem.btb_update.valid := mispredict_hit && (!final_cfi || final_cfi_taken)
  io.imem.btb_update.bits.isValid := final_cfi
  io.imem.btb_update.bits.cfiType :=
    Mux((final_cfi_uop.ctrl.jal || final_cfi_uop.ctrl.jalr) && final_cfi_uop.rd === 1.U, CFIType.call,
    Mux(final_cfi_uop.ctrl.jalr && final_cfi_uop.inst(19,15) === BitPat("b00?01"), CFIType.ret,
    Mux(final_cfi_uop.ctrl.jal || final_cfi_uop.ctrl.jalr, CFIType.jump,
    CFIType.branch)))
  // io.imem.btb_update.bits.target := io.imem.req.bits.pc
  io.imem.btb_update.bits.br_pc := (if (usingCompressed) final_cfi_uop.pc + Mux(final_cfi_uop.rvc, 0.U, 2.U) else final_cfi_uop.pc)
  io.imem.btb_update.bits.pc := ~(~io.imem.btb_update.bits.br_pc | (coreInstBytes*fetchWidth-1).U)
  io.imem.btb_update.bits.prediction := final_cfi_uop.btb_resp
  io.imem.btb_update.bits.taken := DontCare
  
  io.imem.bht_update.valid := mispredict_hit
  io.imem.bht_update.bits.pc := io.imem.btb_update.bits.pc
  io.imem.bht_update.bits.taken := final_br_taken
  io.imem.bht_update.bits.mispredict := mispredict_hit
  io.imem.bht_update.bits.branch := final_cfi_uop.ctrl.branch
  io.imem.bht_update.bits.prediction := final_cfi_uop.btb_resp.bht
  
  // Connect RAS in Frontend
  io.imem.ras_update := DontCare

  io.fpu.valid := !ctrl_killd(1) && id_ctrls(1).fp //TODO - check if ctrl_killd(1) is set by ctrl_killd(0)
  io.fpu.killx := ctrl_killx(1)
  io.fpu.killm := ctrl_killm1(1) //REVIEW 
  io.fpu.inst := id_uops(1).inst
  io.fpu.fromint_data := ex_p1_rs(0)
  io.fpu.dmem_resp_val := dmem_resp_valid && dmem_resp_fpu
  io.fpu.dmem_resp_data := (if (minFLen == 32) io.dmem.resp.bits.data_word_bypass else io.dmem.resp.bits.data)
  io.fpu.dmem_resp_type := io.dmem.resp.bits.size
  io.fpu.dmem_resp_tag := dmem_resp_waddr
  io.fpu.keep_clock_enabled := io.ptw.customCSRs.disableCoreClockGate
  io.vwb_ready.wb_fp_ready := io.fpu.vfp_wb.ready
  io.fpu.vfp_wb.valid := io.vcomplete.wen_fp
  io.fpu.vfp_wb.bits.wdata := io.vcomplete.wdata
  io.fpu.vfp_wb.bits.waddr := io.vcomplete.wdata_reg_idx

  io.dmem.req.valid     := ex_reg_valids(0) && ex_reg_uops(0).ctrl.mem
  val ex_dcache_tag = Cat(ex_reg_uops(0).rd, ex_reg_uops(0).ctrl.fp)
  require(coreParams.dcacheReqTagBits >= ex_dcache_tag.getWidth)
  io.dmem.req.bits.tag  := ex_dcache_tag
  io.dmem.req.bits.cmd  := ex_reg_uops(0).ctrl.mem_cmd
  io.dmem.req.bits.size := ex_reg_mem_size
  io.dmem.req.bits.signed := ex_reg_uops(0).inst(14)
  io.dmem.req.bits.phys := false.B
  io.dmem.req.bits.addr := encodeVirtualAddress(ex_p1_rs(0), alu_p0.io.adder_out)
  io.dmem.req.bits.idx.foreach(_ := io.dmem.req.bits.addr)
  io.dmem.req.bits.dprv := csr.io.status.dprv
  io.dmem.req.bits.dv := csr.io.status.dv
  io.dmem.req.bits.no_alloc := DontCare
  io.dmem.req.bits.no_xcpt := DontCare
  io.dmem.req.bits.data := DontCare
  io.dmem.req.bits.mask := DontCare

  io.dmem.asid := m2_reg_rsdata(0)(1)
  io.dmem.s1_data.data := (if (fLen == 0) m1_reg_rsdata(0)(1) else Mux(m1_reg_uops(0).ctrl.fp, Fill((xLen max fLen) / fLen, io.fpu.store_data), m1_reg_rsdata(0)(1)))
  io.dmem.s1_data.mask := DontCare

  io.dmem.s1_kill := ctrl_killm1(0) //REVIEW
  io.dmem.s2_kill := false.B
  // don't let D$ go to sleep if we're probably going to use it soon
  io.dmem.keep_clock_enabled := true.B //REVIEW

  //TODO - delete below
  io.rocc.resp.ready := false.B
  io.rocc.mem.req.ready := false.B
  io.rocc.mem := DontCare
  csr.io.rocc_interrupt := io.rocc.interrupt
  io.rocc.csrs <> csr.io.roccCSRs
  io.rocc.cmd.valid := DontCare
  io.rocc.exception := DontCare
  io.rocc.cmd.bits := DontCare


  io.cease := csr.io.status.cease
  io.wfi := csr.io.status.wfi

  //TODO -  CSR flush

  //TODO -  check logic-loop: 
  //   io.fpu.killm := killm_common   =>   io.fpu.killm := killm(1)

  val coreMonitorBundle = Wire(new CoreMonitorBundle(xLen, fLen))

  coreMonitorBundle.clock := clock
  coreMonitorBundle.reset := reset
  coreMonitorBundle.hartid := io.hartid
  coreMonitorBundle.timer := csr.io.time(31,0)
  coreMonitorBundle.valid := csr.io.trace(0).valid && !csr.io.trace(0).exception
  coreMonitorBundle.pc := csr.io.trace(0).iaddr(vaddrBitsExtended-1, 0).sextTo(xLen)
  coreMonitorBundle.wrenx := DontCare //FIXME - 
  coreMonitorBundle.wrenf := DontCare //FIXME - 
  coreMonitorBundle.wrdst := DontCare //FIXME - 
  coreMonitorBundle.wrdata := DontCare //FIXME - 
  coreMonitorBundle.rd0src := DontCare //FIXME - 
  coreMonitorBundle.rd0val := DontCare //FIXME - 
  coreMonitorBundle.rd1src := DontCare //FIXME - 
  coreMonitorBundle.rd1val := DontCare //FIXME - 
  coreMonitorBundle.inst := csr.io.trace(0).insn
  coreMonitorBundle.excpt := csr.io.trace(0).exception
  coreMonitorBundle.priv_mode := csr.io.trace(0).priv

  /**
    * Verification logic //TODO - so far, only integer instrn logic
    */
  val ver_module = Module(new UvmVerification)
  if(gpcParams.useVerif) {
    ver_module.io.uvm_in := DontCare
    ver_module.io.uvm_in.swap := wb_reg_swap
    for (i <- 0 until 2) {
      ver_module.io.uvm_in.rob_enq(i).valid := wb_reg_valids(i)
      ver_module.io.uvm_in.rob_enq(i).bits.pc := wb_reg_uops(i).pc
      ver_module.io.uvm_in.rob_enq(i).bits.insn := wb_reg_uops(i).inst
      ver_module.io.uvm_in.rob_enq(i).bits.int := wb_reg_uops(i).ctrl.wxd
      ver_module.io.uvm_in.rob_enq(i).bits.fp := wb_reg_uops(i).wfd
      ver_module.io.uvm_in.rob_enq(i).bits.waddr := wb_reg_waddr(i)
      ver_module.io.uvm_in.rob_enq(i).bits.wdata := wb_reg_wdata(i)
      ver_module.io.uvm_in.rob_wb(i).valid := ll_wen_wb(i)
      ver_module.io.uvm_in.rob_wb(i).bits.int := ll_wen_wb(i)
      ver_module.io.uvm_in.rob_wb(i).bits.waddr := wb_reg_waddr(i)
      ver_module.io.uvm_in.rob_wb(i).bits.wdata := wb_reg_wdata(i)
    }

    dontTouch(io.verif.get)
    io.verif.get <> ver_module.io.uvm_out
  }


  } // leaving gated-clock domain
  val gpcImpl = withClock (gated_clock) { new GpcImpl }

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  def coverExceptions(exceptionValid: Bool, cause: UInt, labelPrefix: String, coverCausesLabels: Seq[(Int, String)]): Unit = {
    for ((coverCause, label) <- coverCausesLabels) {
      property.cover(exceptionValid && (cause === coverCause.U), s"${labelPrefix}_${label}")
    }
  }

  def checkHazards(targets: Seq[(Bool, UInt)], cond: UInt => Bool) =
    targets.map(h => h._1 && cond(h._2)).reduce(_||_)

  def encodeVirtualAddress(a0: UInt, ea: UInt) = if (vaddrBitsExtended == vaddrBits) ea else {
    // efficient means to compress 64-bit VA into vaddrBits+1 bits
    // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1))
    val b = vaddrBitsExtended-1
    val a = (a0 >> b).asSInt
    val msb = Mux(a === 0.S || a === -1.S, ea(b), !ea(b-1))
    Cat(msb, ea(b-1, 0))
  }

}

class RegFile(n: Int, w: Int, zero: Boolean = false) {
  val rf = Mem(n, UInt(w.W))
  private def access(addr: UInt) = rf(~addr(log2Up(n)-1,0))
  private val reads = ArrayBuffer[(UInt,UInt)]()
  private var canRead = true
  def read(addr: UInt) = {
    require(canRead)
    reads += addr -> Wire(UInt())
    reads.last._2 := Mux(zero.B && addr === 0.U, 0.U, access(addr))
    reads.last._2
  }
  def write(addr: UInt, data: UInt) = {
    canRead = false
    when (addr =/= 0.U) {
      access(addr) := data
      for ((raddr, rdata) <- reads)
        when (addr === raddr) { rdata := data }
    }
  }
}

object ImmGen {
  def apply(sel: UInt, inst: UInt) = {
    val sign = Mux(sel === IMM_Z, 0.S, inst(31).asSInt)
    val b30_20 = Mux(sel === IMM_U, inst(30,20).asSInt, sign)
    val b19_12 = Mux(sel =/= IMM_U && sel =/= IMM_UJ, sign, inst(19,12).asSInt)
    val b11 = Mux(sel === IMM_U || sel === IMM_Z, 0.S,
              Mux(sel === IMM_UJ, inst(20).asSInt,
              Mux(sel === IMM_SB, inst(7).asSInt, sign)))
    val b10_5 = Mux(sel === IMM_U || sel === IMM_Z, 0.U, inst(30,25))
    val b4_1 = Mux(sel === IMM_U, 0.U,
               Mux(sel === IMM_S || sel === IMM_SB, inst(11,8),
               Mux(sel === IMM_Z, inst(19,16), inst(24,21))))
    val b0 = Mux(sel === IMM_S, inst(7),
             Mux(sel === IMM_I, inst(20),
             Mux(sel === IMM_Z, inst(15), 0.U)))

    Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0).asSInt
  }
}

class BusyTable(zero: Boolean = false) {
    val table = RegInit(0.U(32.W))
    def reqVecToMask(enVec: Seq[Bool], addrVec: Seq[UInt]): UInt = {
      enVec zip addrVec map {case (en, addr) => Mux(en, UIntToOH(addr), 0.U)} reduce {_|_}
    }
    def set(enVec: Seq[Bool], addrVec: Seq[UInt]): Unit = {
      val updated = table | reqVecToMask(enVec, addrVec)
      table := { if (!zero) updated else { updated & "hFFFF_FFFE".U } }
    }
    def clear(enVec: Seq[Bool], addrVec: Seq[UInt]): Unit = {
      table := table & (~reqVecToMask(enVec, addrVec))
    }
    def read(addr: UInt): Bool = {
      val addrOH = Seq.tabulate(32)(addr === _.U)
      Mux1H(addrOH, table.asBools)
    }
}
  


// object Main extends App {
//   println("Generating hardware of grape cove main pipe")
//   emitVerilog(new Gpc(), Array("--target-dir", "generated",
//               "--emission-options=disableMemRandomization,disableRegisterRandomization"))
// }
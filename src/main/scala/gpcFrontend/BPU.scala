package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import chisel3.internal.InstanceId
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.util._


case class BHTParams(
  nEntries: Int = 512,  //number of PHT entry
  counterLength: Int = 2,  
  historyLength: Int = 20, //TODO: maybe need to change
  historyBits: Int = 9, //TODO: maybe need to change
  tagBits: Int = 6,
  nCountersPerEntry: Int = 8,
  entryLength: Int = 8)

case class BTBParams(
  nEntries: Int = 28,   //TODO:need to change
  nMatchBits: Int = 14,
  nPages: Int = 6,
  nRAS: Int = 6,  //TODO: need to change
  bhtParams: Option[BHTParams] = Some(BHTParams()),
  updatesOutOfOrder: Boolean = false,
  ijtpParams: Option[IJTPParams] = Some(IJTPParams()))

  //TODO:IJTP parameters
case class IJTPParams(
  nEntries: Int = 8,
  nMatchBits: Int = 14,
  localHistoryLength: Int = 8,
  historyBits: Int = 3,
  bhrEntries:Int = 8
)

trait HasBtbParameters extends HasCoreParameters { this: InstanceId =>
  val btbParams = tileParams.btb.getOrElse(BTBParams(nEntries = 0))
  val matchBits = btbParams.nMatchBits max log2Ceil(p(CacheBlockBytes) * tileParams.icache.get.nSets)
  val entries = btbParams.nEntries
  val updatesOutOfOrder = btbParams.updatesOutOfOrder
  val nPages = (btbParams.nPages + 1) / 2 * 2 // control logic assumes 2 divides pages
}

abstract class BtbModule(implicit val p: Parameters) extends Module with HasBtbParameters {
  Annotated.params(this, btbParams)
}

abstract class BtbBundle(implicit val p: Parameters) extends Bundle with HasBtbParameters

class RAS(nras: Int) {
  def push(addr: UInt): Unit = {
    when (count < nras.U) { count := count + 1.U }
    val nextPos = Mux((isPow2(nras)).B || pos < (nras-1).U, pos+1.U, 0.U)
    stack(nextPos) := addr
    pos := nextPos
  }
  def peek: UInt = stack(pos)
  def pop(): Unit = when (!isEmpty) {
    count := count - 1.U
    pos := Mux((isPow2(nras)).B || pos > 0.U, pos-1.U, (nras-1).U)
  }
  def clear(): Unit = count := 0.U
  def isEmpty: Bool = count === 0.U

  private val count = RegInit(0.U(log2Up(nras+1).W))
  private val pos = RegInit(0.U(log2Up(nras).W))
  private val stack = Reg(Vec(nras, UInt()))
}

class IJTP(params: IJTPParams)(implicit val p: Parameters) extends HasCoreParameters {
  private def hashAddr(addr: UInt) = {
      val hi = addr >> log2Ceil(fetchBytes)
      hi(log2Ceil(params.nEntries)-1, 0) ^ (hi >> log2Ceil(params.nEntries))(1, 0)
    }
  def bhridxMatch(addr: UInt) = {
                  val idx = hashAddr(addr)
                  bhrIdx.map(_ === idx).asUInt }
  def getLocalHistory(addr: UInt):UInt = {
        Mux1H(bhridxMatch(addr), bhrs)}
  def index(addr: UInt) = {
    def hashHistory(hist: UInt) = if (params.localHistoryLength == params.historyBits) hist else {
      val k = math.sqrt(3)/2
      val i = BigDecimal(k * math.pow(2, params.localHistoryLength)).toBigInt
      (i.U * hist)(params.localHistoryLength-1, params.localHistoryLength-params.historyBits)
    }
    hashAddr(addr) ^ (hashHistory(getLocalHistory(addr)) << (log2Up(params.nEntries) - params.historyBits))
  }
  def jumpTarget(addr: UInt): UInt = {
    targetTable(index(addr))
  }
  def updateLocalHistory(addr: UInt, taken: Bool) = {
    Cat(taken, Mux1H(bhridxMatch(addr), bhrs) >> 1)
  }
  def updateTargetTable(addr: UInt, taken: Bool, jumpTarget: UInt): Unit={
    wen := true.B
    waddr := index(addr)
    wdata := jumpTarget
  }

  val bhrs = Reg(Vec(params.bhrEntries, UInt(params.localHistoryLength.W)))
  val bhrIdx = Reg(Vec(params.bhrEntries, UInt((params.nMatchBits - log2Up(coreInstBytes)).W)))  //TODO:the length need to be check
  private val targetTable = Mem(params.nEntries, UInt(params.localHistoryLength.W))
  private val reset_waddr = RegInit(0.U((params.nEntries.log2+1).W))
  private val resetting = !reset_waddr(params.nEntries.log2)
  private val wen = WireInit(false.B)
  private val waddr = WireInit(0.U)
  private val wdata = WireInit(0.U)
  when (wen) { targetTable(waddr) := wdata }

  def isEmpty: Bool= {
    bhrs.forall(_.andR === false.B) 
  }
}

//zxr: change the BHT
class BHTResp(implicit p: Parameters) extends BtbBundle()(p) {
  val history = UInt(btbParams.bhtParams.map(_.historyLength).getOrElse(1).W)
  val value = UInt(btbParams.bhtParams.map(_.counterLength).getOrElse(1).W)
  val hasBias = Bool()   
  def taken = value(1)
  def strongly_taken = value === 3.U
}

/* ------------------------------- BHT --------------------------------- 
  The global history:
     - updated speculatively in fetch (if there's a BTB hit).
     - on a mispredict, the history register is reset (again, only if BTB hit).
  The counter table:
     - each counter corresponds with the address of the fetch packet ("fetch pc").
     - updated when a branch resolves (and BTB was a hit for that branch).
       The updating branch must provide its "fetch pc".

         PC:    │xxxxxx│xxxxxxxxx│xxx│xxx│
                       └─────────┘   └───┘ 
                      log(nEntries)  log(fetchBytes)    
                └──────┘         └───┘
                tagBits       log(entryLength)
*/

class BHT(params: BHTParams)(implicit val p: Parameters) extends HasCoreParameters {
//direction PHT
val taken_array = DescribedSRAM(
    name = "taken_array",
    desc = "taken direction PHT",
    size = params.nEntries * params.nCountersPerEntry,
    data = UInt((params.counterLength).W))

val taken_tag_array = DescribedSRAM(
    name = "taken_tag_array",
    desc = "taken direction PHT",
    size = params.nEntries,
    data = UInt((params.tagBits).W))

val not_taken_array = DescribedSRAM(
    name = "not_taken_array",
    desc = "not taken direction PHT",
    size = params.nEntries * params.nCountersPerEntry,
    data = UInt((params.counterLength).W))

val not_taken_tag_array = DescribedSRAM(
    name = "not_taken_array",
    desc = "not taken direction PHT",
    size = params.nEntries,
    data = UInt((params.tagBits).W))

// -------------------------- choice_PHT ----------------------------  
//   Size: 1 KiB (nEntries * nCountersPerEntry * counterLength) 
//             |xx|xx|xx|xx|xx|xx|xx|xx|
//                                  ↑counterLength           
//             └── nCountersPerEntry ──┘                                                      
// ------------------------------------------------------------------
val choice_array = DescribedSRAM(
    name = "choice_array",
    desc = "choice PHT",
    size = params.nEntries * params.nCountersPerEntry,
    data = UInt((params.counterLength).W))

// GHR
val globalHistory = RegInit(0.U(params.historyLength.W))

//hash global history
def hashHistory(hist: UInt) = if (params.historyLength == params.historyBits) hist else {
      val k = math.sqrt(3)/2
      val i = BigDecimal(k * math.pow(2, params.historyLength)).toBigInt
      (i.U * hist)(params.historyLength-1, params.historyLength-params.historyBits)
}
//index used to find tag
def entryIdx(addr: UInt, history: UInt) = {
  hashHistory(history) ^ addr.extract(log2Up(params.nEntries) + log2Up(params.entryLength) + log2Ceil(fetchBytes) - 1,
                                        log2Up(params.entryLength) + log2Ceil(fetchBytes))
}
//index used to find direction PHT entry data
def dataIdx(addr: UInt, history: UInt) = {
  val offsetIdx = history.extract(log2Up(params.entryLength) - 1, 0) ^  addr.extract(log2Up(params.entryLength) + log2Ceil(fetchBytes) - 1, log2Ceil(fetchBytes))
  Cat(entryIdx(addr,history),offsetIdx)
}
//tag used to compare
def tag(addr: UInt) = {
      addr.extract(log2Up(params.nEntries) + log2Up(params.entryLength) + log2Ceil(fetchBytes) + params.tagBits -1,
                    log2Up(params.nEntries) + log2Up(params.entryLength) + log2Ceil(fetchBytes))
}

//look up for BHT results
def getresult(addr: UInt, rvalid: Bool): BHTResp = {
  val res = Wire(new BHTResp)
  val tag_taken_rdata = taken_tag_array.read(entryIdx(addr,globalHistory),rvalid && !taken_direction_wen)        //TODO:need complete
  val tag_not_taken_rdata = not_taken_tag_array.read(entryIdx(addr,globalHistory),rvalid && !not_taken_direction_wen)
  val taken_array_rdata = taken_array.read(dataIdx(addr,globalHistory),rvalid && !taken_direction_wen)
  val not_taken_array_rdata = not_taken_array.read(dataIdx(addr,globalHistory),rvalid && !not_taken_direction_wen)
  val s1_tag = RegNext(tag(addr))
  val takenTagMatch = s1_tag === tag_taken_rdata
  val notTakenTagMatch = s1_tag === tag_not_taken_rdata
  val choice_rdata = choice_array.read(addr.extract(log2Up(params.nEntries) + log2Up(params.entryLength) + log2Ceil(fetchBytes) - 1,
                                                    log2Ceil(fetchBytes)),rvalid && !choice_wen)
  val tagHit = Mux(choice_rdata(1),notTakenTagMatch,takenTagMatch)
  val directionData = Mux(choice_rdata(1),not_taken_array_rdata,taken_array_rdata)

  res.value := Mux(tagHit,directionData,choice_rdata)
  res.history := globalHistory
  res.hasBias := tagHit
  res
}

//update 2bc
def update2BC(taken: Bool, d: BHTResp): UInt = {
  params.counterLength match {
        case 1 => taken
        case 2 => Cat(taken && d.value(0) || d.value(1) && (d.value(0)^taken), taken && (d.value(1) || ~d.value(0)) || d.value(1) && ~d.value(0))
      }
}

//update choice PHT
def updateChoicePHT(addr: UInt, d: BHTResp, taken: Bool): Unit = {
    choice_wen := true.B
    when (!resetting) {
      choice_waddr := addr.extract(log2Up(params.nEntries) + log2Up(params.entryLength) + log2Ceil(fetchBytes) - 1,
                                                    log2Ceil(fetchBytes))
      choice_wdata := update2BC(taken, d)
  }
  }

  private val reset_waddr = RegInit(0.U((params.nCountersPerEntry.log2 + params.nEntries.log2 + 1).W))
  private val resetting = !reset_waddr(params.nCountersPerEntry.log2 + params.nEntries.log2)
  private val choice_wen = WireInit(resetting)
  private val choice_waddr = WireInit(reset_waddr)
  private val choice_wdata = WireInit(0.U)
  private val taken_direction_wen = WireInit(resetting)
  private val not_taken_direction_wen = WireInit(resetting)
  private val direction_waddr = WireInit(reset_waddr)
  private val direction_wdata = WireInit(0.U)
  private val tag_waddr = WireInit(reset_waddr)
  private val tag_wdata = WireInit(0.U)

  when(choice_wen){
      choice_array.write(choice_waddr,choice_wdata)
  }
  
  when (resetting) { reset_waddr := reset_waddr + 1.U }
  
//update direction PHT
def updateDirectionPHT(addr: UInt, d: BHTResp, taken: Bool): Unit = {
      when(taken){
        taken_direction_wen := true.B
      }
      when(!taken){
        not_taken_direction_wen := true.B
      }
      when(!resetting){
      direction_waddr := dataIdx(addr,d.history)
      direction_wdata := update2BC(taken, d)
      tag_waddr := entryIdx(addr,d.history)
      tag_wdata := tag(addr)
  }
}
  
  when(taken_direction_wen){
      taken_array.write(direction_waddr,direction_wdata)
      taken_tag_array.write(tag_waddr,tag_wdata)
  }
  when(not_taken_direction_wen){
      not_taken_array.write(direction_waddr,direction_wdata)
      not_taken_tag_array.write(tag_waddr,tag_wdata)
  }
      
//reset GHR
def resetHistory(d: BHTResp): Unit = {
    globalHistory := d.history
  }

//update GHR
def updateHistory(addr: UInt, d: BHTResp, taken: Bool): Unit = {
    globalHistory := Cat(taken, d.history >> 1)
  }

//update GHR by pre-decode
def advanceHistory(taken: Bool): Unit = {
    globalHistory := Cat(taken, globalHistory >> 1)
    }
  }

  //zxr: change CFI
object CFIType {
  def SZ = 3
  def apply() = UInt(SZ.W)
  def branch = 0.U
  def jump = 1.U
  def call = 2.U
  def ret = 3.U
  def indrjump = 4.U //indirect jump 
}

// BTB update occurs during branch resolution (and only on a mispredict).
//  - "pc" is what future fetch PCs will tag match against.
//  - "br_pc" is the PC of the branch instruction.
class BTBUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val prediction = new BTBResp
  val pc = UInt(vaddrBits.W)
  //val target = UInt(vaddrBits.W)
  val taken = Bool()
  val isValid = Bool()
  val br_pc = UInt(vaddrBits.W)
  val cfiType = CFIType()
}

// BHT update occurs during branch resolution on all conditional branches.
//  - "pc" is what future fetch PCs will tag match against.
class BHTUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val prediction = new BHTResp
  val pc = UInt(vaddrBits.W)
  val branch = Bool()
  val taken = Bool()
  val mispredict = Bool()
}

class RASUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val cfiType = CFIType()
  val returnAddr = UInt(vaddrBits.W)
}


class IJTPUpdate(implicit p: Parameters) extends BtbBundle()(p){
  val pc = UInt(vaddrBits.W)
  val taken = Bool()
  val jumpTarget = UInt(vaddrBits.W)
}

//  - "bridx" is the low-order PC bits of the predicted branch (after
//     shifting off the lowest log(inst_bytes) bits off).
//  - "mask" provides a mask of valid instructions (instructions are
//     masked off by the predicted taken branch from the BTB).
class BTBResp(implicit p: Parameters) extends BtbBundle()(p) {
  val cfiType = CFIType()
  val taken = Bool()
  val mask = Bits(fetchWidth.W)
  val bridx = Bits(log2Up(fetchWidth).W)
  val target = UInt(vaddrBits.W)
  val entry = UInt(log2Up(entries + 1).W)
  val bht = new BHTResp
}

class BTBReq(implicit p: Parameters) extends BtbBundle()(p) {
   val addr = UInt(vaddrBits.W)
}

class BTB(implicit p: Parameters) extends BtbModule {
  val io = IO(new Bundle {
    val req = Flipped(Valid(new BTBReq))
    val resp = Valid(new BTBResp)
    val btb_update = Flipped(Valid(new BTBUpdate))
    val bht_update = Flipped(Valid(new BHTUpdate))
    val bht_advance = Flipped(Valid(new BTBResp))
    val ras_update = Flipped(Valid(new RASUpdate))
    val ras_head = Valid(UInt(vaddrBits.W))
    val flush = Input(Bool())
    val ijtp_update = Flipped(Valid(new IJTPUpdate))
  })

  val s0_valid = io.req.valid   //stage 0
  val s1_valid = RegNext(s0_valid)
  //val s1_valid = RegNext(s0_valid && !io.kill)    //stage 1  //TODO: comlete kill
  val s0_pc = io.req.bits.addr   // look up BHT and BTB in stage 0 
  val s1_pc = RegEnable(s0_pc, s0_valid) //TODO: need complete s0_valid

  val idxs = Reg(Vec(entries, UInt((matchBits - log2Up(coreInstBytes)).W)))
  val idxPages = Reg(Vec(entries, UInt(log2Up(nPages).W)))
  val tgts = Reg(Vec(entries, UInt((matchBits - log2Up(coreInstBytes)).W)))
  val tgtPages = Reg(Vec(entries, UInt(log2Up(nPages).W)))
  val pages = Reg(Vec(nPages, UInt((vaddrBits - matchBits).W)))
  val pageValid = RegInit(0.U(nPages.W))
  val pagesMasked = (pageValid.asBools zip pages).map { case (v, p) => Mux(v, p, 0.U) }

  val isValid = RegInit(0.U(entries.W))
  val cfiType = Reg(Vec(entries, CFIType()))
  val brIdx = Reg(Vec(entries, UInt(log2Up(fetchWidth).W)))
  
  //partial pc in pages
  private def page(addr: UInt) = addr >> matchBits

  //page field hit
  private def pageMatch(addr: UInt) = {
    val p = page(addr)
    pageValid & pages.map(_ === p).asUInt
  }

  // look up BTB enntry
  private def idxMatch(addr: UInt) = {
    val idx = addr(matchBits-1, log2Up(coreInstBytes))
    idxs.map(_ === idx).asUInt & isValid
  }

  val r_btb_update = Pipe(io.btb_update)
  val update_target = io.req.bits.addr

  val pageHit = pageMatch(s0_pc)  //page hit in F0 stage
  val idxHit = idxMatch(s0_pc)  //idx hit in F0 stage
  val btbHit = RegEnable(idxHit,s0_valid)  //btb hit in F1 stage

  val updatePageHit = pageMatch(r_btb_update.bits.pc)
  val (updateHit, updateHitAddr) =
    if (updatesOutOfOrder) {
      val updateHits = (pageHit << 1)(Mux1H(idxMatch(r_btb_update.bits.pc), idxPages))
      (updateHits.orR, OHToUInt(updateHits))
    } else (r_btb_update.bits.prediction.entry < entries.U, r_btb_update.bits.prediction.entry)

  val useUpdatePageHit = updatePageHit.orR
  val usePageHit = pageHit.orR
  val doIdxPageRepl = !useUpdatePageHit
  val nextPageRepl = RegInit(0.U(log2Ceil(nPages).W))
  val idxPageRepl = Cat(pageHit(nPages-2,0), pageHit(nPages-1)) | Mux(usePageHit, 0.U, UIntToOH(nextPageRepl))
  val idxPageUpdateOH = Mux(useUpdatePageHit, updatePageHit, idxPageRepl)
  val idxPageUpdate = OHToUInt(idxPageUpdateOH)
  val idxPageReplEn = Mux(doIdxPageRepl, idxPageRepl, 0.U)

  val samePage = page(r_btb_update.bits.pc) === page(update_target)
  val doTgtPageRepl = !samePage && !usePageHit
  val tgtPageRepl = Mux(samePage, idxPageUpdateOH, Cat(idxPageUpdateOH(nPages-2,0), idxPageUpdateOH(nPages-1)))
  val tgtPageUpdate = OHToUInt(pageHit | Mux(usePageHit, 0.U, tgtPageRepl))
  val tgtPageReplEn = Mux(doTgtPageRepl, tgtPageRepl, 0.U)

  when (r_btb_update.valid && (doIdxPageRepl || doTgtPageRepl)) {
    val both = doIdxPageRepl && doTgtPageRepl
    val next = nextPageRepl + Mux[UInt](both, 2.U, 1.U)
    nextPageRepl := Mux(next >= nPages.U, next(0), next)
  }

  val repl = new PseudoLRU(entries)
  val waddr = Mux(updateHit, updateHitAddr, repl.way)
  val r_resp = Pipe(io.resp)
  when (r_resp.valid && r_resp.bits.taken || r_btb_update.valid) {
    repl.access(Mux(r_btb_update.valid, waddr, r_resp.bits.entry))
  }

  // update BTB
  when (r_btb_update.valid) {
    val mask = UIntToOH(waddr)
    idxs(waddr) := r_btb_update.bits.pc(matchBits-1, log2Up(coreInstBytes))
    tgts(waddr) := update_target(matchBits-1, log2Up(coreInstBytes))
    idxPages(waddr) := idxPageUpdate +& 1.U // the +1 corresponds to the <<1 on io.resp.valid
    tgtPages(waddr) := tgtPageUpdate
    cfiType(waddr) := r_btb_update.bits.cfiType
    isValid := Mux(r_btb_update.bits.isValid, isValid | mask, isValid & ~mask)
    if (fetchWidth > 1)
      brIdx(waddr) := r_btb_update.bits.br_pc >> log2Up(coreInstBytes)

    require(nPages % 2 == 0)
    val idxWritesEven = !idxPageUpdate(0)

    def writeBank(i: Int, mod: Int, en: UInt, data: UInt) =
      for (i <- i until nPages by mod)
        when (en(i)) { pages(i) := data }

    writeBank(0, 2, Mux(idxWritesEven, idxPageReplEn, tgtPageReplEn),
      Mux(idxWritesEven, page(r_btb_update.bits.pc), page(update_target)))
    writeBank(1, 2, Mux(idxWritesEven, tgtPageReplEn, idxPageReplEn),
      Mux(idxWritesEven, page(update_target), page(r_btb_update.bits.pc)))
    pageValid := pageValid | tgtPageReplEn | idxPageReplEn
  }

  
  val s0_btb_target = Cat(pagesMasked(Mux1H(idxHit, tgtPages)), Mux1H(idxHit, tgts) << log2Up(coreInstBytes))
  val s0_bridx = if (fetchWidth > 1) Mux1H(idxHit, brIdx) else 0.U
  val s0_cfiType = Mux1H(idxHit, cfiType)
  val s0_mask = Cat((1.U << ~Mux(io.resp.bits.taken, ~io.resp.bits.bridx, 0.U))-1.U, 1.U)
  val s1_btb_target = RegEnable(s0_btb_target,s0_valid)
  val s1_bridx = RegEnable(s0_bridx,s0_valid)
  val s1_cfitype = RegEnable(s0_cfiType,s0_valid)
  val s1_mask = RegEnable(s0_mask,s0_valid)

  // BTB get result in stage 1
  io.resp.valid := s1_valid && RegNext((pageHit << 1)(Mux1H(idxHit, idxPages)))
  io.resp.bits.taken := true.B
  io.resp.bits.target := s1_btb_target
  io.resp.bits.entry := OHToUInt(idxHit)
  io.resp.bits.bridx := s1_bridx
  io.resp.bits.mask := s1_mask
  io.resp.bits.cfiType := s1_cfitype

  // if multiple entries for same PC land in BTB, zap them
  when (PopCountAtLeast(idxHit, 2)) {
    isValid := isValid & ~idxHit
  }
  when (io.flush) {
    isValid := 0.U
  }
  
  /*  Training BHT
  1. Choice PHT:
    update normally;
    don't update, if the result from choice PHT is different from the final result ,but the accepted result from direction PHT is same

  2. Direction PHT
    update if the direction result is accepted by the final result (Hit)
    update if the final result is different from the result from choice PHT 
   */

  if (btbParams.bhtParams.nonEmpty) {
    val bht = new BHT(Annotated.params(this, btbParams.bhtParams.get))
    val isBranch = (btbHit & cfiType.map(_ === CFIType.branch).asUInt).orR
    val res = bht.getresult(s0_pc,s0_valid)
    when (io.bht_update.valid) {
      when (io.bht_update.bits.branch) {
        when(!(io.bht_update.bits.prediction.hasBias && !io.bht_update.bits.mispredict)) {
        bht.updateChoicePHT(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken)  //update choice PHT
        }
        when(io.bht_update.bits.prediction.hasBias && !io.bht_update.bits.mispredict ||
              io.bht_update.bits.mispredict && !io.bht_update.bits.prediction.hasBias ){
        bht.updateDirectionPHT(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken)  //update direction PHT
        }
        when(io.bht_update.bits.mispredict) {
         bht.updateHistory(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken)   //update GHR
        } }
      }.elsewhen (io.bht_update.bits.mispredict) {
        bht.resetHistory(io.bht_update.bits.prediction)
      }
    when (io.bht_advance.valid) {
      bht.advanceHistory(io.bht_advance.bits.bht.taken)
      }
    when (!res.taken && isBranch) { io.resp.bits.taken := false.B }
      io.resp.bits.bht := res
    }
  
  // look up RAS in F1 stage 
  if (btbParams.nRAS > 0) {
    val ras = new RAS(btbParams.nRAS)
    val doPeek = (btbHit & cfiType.map(_ === CFIType.ret).asUInt).orR
    io.ras_head.valid := !ras.isEmpty
    io.ras_head.bits := ras.peek
    when (!ras.isEmpty && doPeek) {
      io.resp.bits.target := ras.peek
    }
    when (io.ras_update.valid) {
      when (io.ras_update.bits.cfiType === CFIType.call) {
        ras.push(io.ras_update.bits.returnAddr)
      }.elsewhen (io.ras_update.bits.cfiType === CFIType.ret) {
        ras.pop()
      }
    }
  }

  // look up IJTP in F1 stage 
  if (btbParams.ijtpParams.nonEmpty){
    val ijtp = new IJTP(Annotated.params(this, btbParams.ijtpParams.get))
    val isIndrJump = (btbHit & cfiType.map(_ === CFIType.indrjump).asUInt).orR  //TODO: need to change from jump to indirect jump
    when ( !ijtp.isEmpty && isIndrJump ) {
      io.resp.bits.target := ijtp.jumpTarget(RegNext(io.req.bits.addr))
    }
    when(io.ijtp_update.valid){
        ijtp.updateLocalHistory(io.ijtp_update.bits.pc,io.ijtp_update.bits.taken)
        ijtp.updateTargetTable(io.ijtp_update.bits.pc,io.ijtp_update.bits.taken,io.ijtp_update.bits.jumpTarget)
      }
    }
}

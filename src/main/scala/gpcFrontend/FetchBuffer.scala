package gpc.core

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config.{Parameters}

import freechips.rocketchip.rocket.ExpandedInstruction

class EnqueueBuddle(implicit p: Parameters) extends CoreBundle()(p) {
  val btb_resp = Output(new BTBResp)
  val pcs = Output(Vec(fetchWidth, UInt(vaddrBitsExtended.W))) // a group pcs
  val inst_exp = Output(Vec(fetchWidth, UInt(32.W)))
  val inst_mask = Output(Vec(fetchWidth, Bool()))
  val raw_insts = Output(Vec(fetchWidth, UInt(32.W)))
  val rvc = Output(Vec(fetchWidth, Bool()))
  val xcpt = Output(new FrontendExceptions)
  val replay = Output(Bool())
}

class Ptr(numEntries: Int) extends Bundle {
  val ptr = UInt(numEntries.W)
  val flag = Bool()
  
  def isEmpty(other: Ptr): Bool = {
    (this.ptr === other.ptr) && (this.flag === other.flag)
  }
  
  def isFull(other: Ptr): Bool = {
    (this.ptr === other.ptr) && (this.flag =/= other.flag)
  }
}

class FetchBuffer(implicit p: Parameters) extends CoreModule{
    val numEntries = (decodeWidth * 3) + 1  // 2*3+1 = 7
    val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new EnqueueBuddle))
    val deq = Vec(decodeWidth, Decoupled(new FrontendResp))
    //val peek = Vec(decodeWidth, Valid(new FrontendResp))
    val flush = Input(Bool())
    val mask = Output(UInt(numEntries.W))
    })
    dontTouch(io.enq)
    dontTouch(io.deq)
    val fb = RegInit(VecInit.fill(numEntries)(0.U.asTypeOf(Valid(new FrontendResp))))
    val cnt = RegInit(0.U(log2Up(numEntries).W))
    io.mask := Mux(cnt === 0.U, 0.U, (1.U << cnt) - 1.U)

   // def isFull: Bool = cnt === numEntries.U 
    
    val enq_ptr = RegInit({
      val ptr_bundle = Wire(new Ptr(numEntries))
      ptr_bundle.flag := false.B
      ptr_bundle.ptr := 1.U
      ptr_bundle
      })
    val deq_ptr = RegInit({
      val ptr_bundle = Wire(new Ptr(numEntries))
      ptr_bundle.flag := false.B
      ptr_bundle.ptr := 1.U
      ptr_bundle
      })
      require(numEntries >= fetchWidth)
    io.enq.ready := PopCount(fb.map(_.valid)) +& PopCount(io.enq.bits.inst_mask) <= numEntries.U
    def rotateLeft(in: Ptr): Ptr = {
    val w = in.ptr.getWidth
    val rotatedPtr = ((in.ptr << 1) | in.ptr(w-1))(w-1,0)
    val newFlag = Wire(Bool()) 
      when(in.ptr(w-1) && !rotatedPtr(w-1)){
          newFlag := !in.flag
      }.otherwise{
          newFlag := !in.flag
      }
    
    val result = Wire(new Ptr(numEntries))
        result.ptr := rotatedPtr
        result.flag := newFlag
        result
  }

  def rotateLeft(in: Ptr, n: UInt): Ptr = {
    val w = in.ptr.getWidth
    val full = Wire(UInt((w*2).W))
    full := in.ptr << n
    assert(n <= w.U, "n must be less than or equal to w")
    val newFlag = Wire(Bool()) 
      when((full((w*2-1),w)).orR){
          newFlag := !in.flag
      }.otherwise{
          newFlag := !in.flag
      }

    val result = Wire(new Ptr(numEntries))
    result.ptr := (full(w-1,0) | (full >> w))(w-1,0)
    result.flag := newFlag
    result
  }
  
  //def isEmpty: Bool = enq_ptr.isEmpty(deq_ptr)
  

  val enq_data = Wire(Vec(fetchWidth, Valid(new FrontendResp)))
  for (i <- 0 until fetchWidth) {
    enq_data(i).valid := io.enq.valid && io.enq.bits.inst_mask(i)
    enq_data(i).bits.pc := io.enq.bits.pcs(i)
    enq_data(i).bits.next_pc := DontCare
    enq_data(i).bits.inst := io.enq.bits.inst_exp(i)
    enq_data(i).bits.raw_inst := io.enq.bits.raw_insts(i)
    enq_data(i).bits.rvc := io.enq.bits.rvc(i)
    enq_data(i).bits.xcpt := io.enq.bits.xcpt
    enq_data(i).bits.replay := io.enq.bits.replay
    enq_data(i).bits.btb := io.enq.bits.btb_resp
  }
  dontTouch(enq_data)
/* Enqueue */
  var ptr_en = enq_ptr
  val write_mask = Wire(Vec(numEntries, Vec(fetchWidth, Bool())))
        write_mask.foreach(_.foreach(_ := false.B))

  for (i <- 0 until fetchWidth) {
    for (j <- 0 until numEntries) {
      write_mask(j)(i) := ptr_en.ptr(j) && enq_data(i).valid
    }
    ptr_en = Mux(enq_data(i).valid, rotateLeft(ptr_en), ptr_en)
  }
when (io.enq.fire) {
    enq_ptr := rotateLeft(enq_ptr, PopCount(io.enq.bits.inst_mask))
    for (i <- 0 until numEntries) {
      when (!fb(i).valid) {
        fb(i) := Mux1H(write_mask(i), enq_data)
      }.otherwise{
        fb(i) := fb(i)
      }
    }
  }
dontTouch(io.enq.fire)
/* -------------------- dequeue -------------------- */
  var ptr_de = deq_ptr
  var deq_mask = 0.U(numEntries.W)
  for (i <- 0 until retireWidth) {
    val deq_data = Mux1H(ptr_de.ptr, fb)
    val next_pc =  Mux1H(rotateLeft(ptr_de).ptr, fb.map(_.bits.pc))
    io.deq(i).valid := deq_data.valid
    io.deq(i).bits := deq_data.bits
    io.deq(i).bits.next_pc := next_pc
    deq_mask = Mux(io.deq(i).fire, deq_mask | ptr_de.ptr, deq_mask)
    ptr_de = rotateLeft(ptr_de)
  }

  for (i <- 0 until numEntries) {
    when (deq_mask(i)) {
      fb(i).valid := false.B
    }
  }

  deq_ptr := rotateLeft(deq_ptr, PopCount(io.deq.map(_.fire)))
  for (i <- 1 until retireWidth) {
    when (io.deq(i).ready) {
      assert(io.deq.take(i).map(_.ready).reduce(_&&_))
    }
  }

  when (io.flush) {
    enq_ptr.ptr := 1.U
    enq_ptr.flag := false.B
    deq_ptr.ptr := 1.U
    deq_ptr.flag := false.B
    fb.foreach(_.valid := false.B)
    cnt := 0.U
  }.otherwise{
     for (i <- 0 until retireWidth) {
      when(io.enq.fire && io.deq(i).fire) {
        cnt := cnt 
      }.elsewhen(io.enq.fire){
        cnt := cnt + 1.U
      }.elsewhen(io.deq(i).fire){
        cnt := cnt - 1.U
      }
    }
  }
  dontTouch(io.flush)
}

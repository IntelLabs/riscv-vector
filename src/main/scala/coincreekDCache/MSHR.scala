// package coincreekDCache

// import chisel3._
// import chisel3.util._
// import util._
// import java.rmi.server.UID

// class ioCachepipeMSHRFile extends Bundle() {
//   // val allocate_valid = Bool()
//   val allocate_tag  = UInt(addrWidth.W)
//   val allocate_type = UInt(1.W) // 1 for write && 0 for read
//   val allocate_mask = UInt(mshrEntryMaskWidth.W)
//   val allocate_data = UInt(mshrEntryDataWidth.W)
// }

// class ioMSHR extends Bundle() {
//   // mshr entry search & add new request inst
//   val allocateReq  = Input(Bool())
//   val allocateType = Input(UInt(1.W))
//   val tag          = Input(UInt(tagWidth.W))

//   val tagMatch = Output(Bool())                   // mshr allocate match
//   val isEmpty  = Output(Bool())
//   val isFull   = Output(Bool())                   // mshr allocate full -> need stall outside
//   val privErr  =
//   val wrLineOH = Output(UInt(mshrEntryDataNum.W)) // new inst data write to which line of current mshr entry

//   // mshr sender port
//   val senderReq = Output(Bool()) // req to send this entry

//   val senderResp = Input(Bool())  // permitted to send this entry now
//   val senderPriv = Output(Bool()) // 1 for write, 0 for read
//   val senderTag  = Output(UInt(tagWidth.W))
// }

// class MSHR(id: Int) extends Module() {
//   val io = IO(new ioMSHR)

//   val mode_idle :: mode_req_enqueue :: mode_resp_wait :: mode_replay :: mode_refill :: Nil = Enum(5)

//   val state = RegInit(mode_idle)

//   // info regs & wires
//   val sendedPermission = Wire(1.W) // 1 for write, 0 for read

//   val typeList = RegInit(0.U(mshrEntryDataNum.W))
//   val tagReg   = RegInit(0.U(addrWidth.W))

//   val totalCounter = RegInit(0.U(log2Up(mshrEntryDataNum).W))

//   // match & output
//   val tagMatch = io.allocateReq && tagReg === io.tag
//   val privErr  = tagMatch && (state > mode_req_enqueue) && !sendedPermission && io.allocateType
//   val isFull   = io.allocateReq && totalCounter >= mshrEntryDataNum.asUInt
//   io.tagMatch := tagMatch
//   io.isFull   := isFull
//   io.isEmpty  := state === mode_idle
//   io.privErr  := privErr

//   io.wrLineOH := Mux(io.allocateReq, UIntToOH(totalCounter, mshrEntryDataNum), 0.U)

//   // info regs & wires update
//   sendedPermission := typeList.orR

//   typeList := Mux(state === mode_refill,
//     0.U,
//     Mux(tagMatch && !isFull && ,
//       typeList | (Cat(0.U((mshrEntryDataNum - 1).W), io.allocateType) << totalCounter),
//       typeList
//     )
//   )

//   tagReg := Mux(state === mode_idle,
//     Mux(tagMatch, io.tag, 0.U),
//     tagReg
//   )

//   totalCounter := Mux(state === mode_refill, 0.U, Mux(tagMatch && !isFull, totalCounter + 1.U, totalCounter))

//   // FSM
//   state := MuxLookup(state, state)(
//     Seq(
//       mode_idle -> Mux(tagMatch, mode_req_enqueue, state),
//       mode_req_enqueue ->
//     )
//   )

// }

// class MSHRFile extends Module() {

//   val io = IO(
//     new Bundle(
//       pipeline_req = Flipped(Decoupled(new ioCachepipeMSHRFile))
//     )
//   )

//   val array4MaskAndData = Module(
//     new SRAMWrapper(
//       gen = UInt((mshrEntryMaskWidth + mshrEntryDataWidth).W),
//       set = mshrEntryDataNum * mshrEntryDataNum
//     )
//   )

//   val senderQueue = Module(new Queue(UInt(log2Up(mshrEntryNum).W), mshrEntryNum))

//   senderQueue.io.enq.valid :=
//     senderQueue.io.enq.bits :=
//     senderQueue.io.deq.ready :=

//   val allocateArb = Module(new Arbiter(UInt(), mshrEntryNum))
//   alloc_arb.io.in.foreach(_.bits := DontCare)

//   val tagMatchList   = Wire(Vec(mshrEntryNum, Bool()))
//   val entryFullList  = Wire(Vec(mshrEntryNum, Bool()))
//   val sramWriteIdxOH = Wire(Vec(mshrEntryNum, UInt(mshrEntryDataNum.W)))

//   val needStall = (tagMatchList & entryFullList).orR

//   val mshrs = (0 until mshrEntryNum) map {
//     i =>
//       val mshr = Module(new MSHR(i))

//       allocateArb.io.in(i).valid := mshr.io.isEmpty
//       mshr.io.allocateReq        := allocateArb.io.in(i).ready

//       tagMatchList(i)   := mshr.io.tagMatch
//       entryFullList(i)  := mshr.io.isFull
//       sramWriteIdxOH(i) := mshr.io.wrLineOH

//   }

//   // Write Mask & Data SRAM
//   array4MaskAndData.io.w.req.valid := Mux(!needStall && io.pipeline_req.valid, True.B, False.B)

//   array4MaskAndData.io.w.bits.apply(
//     setIdx  := OHToUInt(sramAccessIdxOH.asTypeOf(UInt)),
//     data    := Cat(io.pipeline_req.allocate_mask, io.pipeline_req.allocate_data),
//     waymask := 1
//   )

// }

// // This is vector LSU with modified OVI interface.
// // So far only focus on basic mem instructions.
// // Issues: 
// //   so far (1) el_id/el_off/el_count of seq_id of ovi_load is not supported
// //          (2) vector load queue dose not use the memop:sync_end (only use ovi_load)
// //          (3) Only support unit-stride load/store, do not support load retry ...
// //          (4) store code are too simple

// package darecreek.lsu

// import chisel3._
// import chisel3.util._
// import utils._
// import darecreek.DarecreekParam._
// import darecreek.{VLdInput, VStInput, VLdOutput, VStOutput, VExpdUOp}
// import darecreek.{OVImemop, OVIload, OVIstore, OVImaskIdx}
// import darecreek.UIntSplit

// object LsuParam {
//   val LdQSize = 32
//   val StQSize = 32
// }
// import LsuParam._

// class VLdQEntry extends Bundle {
//   val valid = Bool()
//   val complete = Bool()
//   val uop = new VExpdUOp
// }

// class VStQEntry extends Bundle {
//   val valid = Bool()
//   val complete = Bool()
//   val uop = new VExpdUOp
// }

// class VLdQPtr extends CircularQueuePtr[VLdQPtr](LdQSize)
// class VStQPtr extends CircularQueuePtr[VStQPtr](StQSize)

// class VLsu extends Module with HasCircularQueuePtrHelper {
//   val io = IO(new Bundle {
//     val fromIQ = new Bundle {
//       val ld = Flipped(Decoupled(new VLdInput))
//       val st = Flipped(Decoupled(new VStInput))
//     }
//     val wb = new Bundle {
//       val ld = ValidIO(new VLdOutput)
//       val st = ValidIO(new VStOutput)
//     }
//     // OVI interfaces
//     val ovi_memop = new OVImemop
//     val ovi_load = new OVIload
//     val ovi_store = new OVIstore
//     // val ovi_maskIdx = new OVImaskIdx
//   })

//   /**
//     * Input arbiter
//     */
//   val ld = io.fromIQ.ld
//   val st = io.fromIQ.st
//   val ldBeforeSt = isBefore(ld.bits.uop.vRobIdx, st.bits.uop.vRobIdx)
//   val readyLdQ = Wire(Bool())
//   val readyStQ = Wire(Bool())
//   when (ld.valid && st.valid) {
//     ld.ready := ldBeforeSt && readyLdQ
//     st.ready := !ldBeforeSt && readyStQ
//   }.otherwise {
//     ld.ready := !ld.valid || readyLdQ
//     st.ready := !st.valid || readyStQ
//   }

//   /**
//     * Load Queue: enqueue
//     */
//   val ldQ = RegInit(VecInit(Seq.fill(LdQSize)(0.U.asTypeOf(new VLdQEntry))))
//   val ldEnqPtr = RegInit(0.U.asTypeOf(new VLdQPtr))
//   val ldDeqPtr = RegInit(0.U.asTypeOf(new VLdQPtr))
//   //---- Enq ----
//   when (ld.fire) {
//     ldQ(ldEnqPtr.value).valid := true.B
//     ldQ(ldEnqPtr.value).complete := false.B
//     ldQ(ldEnqPtr.value).uop := ld.bits.uop
//     ldEnqPtr := ldEnqPtr + 1.U
//   }
//   //---- OVI syn_start: a little verbose and not tested ----
//   val validCountLdQ = distanceBetween(ldEnqPtr, ldDeqPtr)
//   val syncStartLd = RegInit(false.B)
//   val alreadyIssueLd = RegInit(false.B)
//   readyLdQ := !isFull(ldEnqPtr, ldDeqPtr)
//   when (ld.fire) {
//     when (ld.bits.uop.expdIdx === 0.U) {
//       syncStartLd := ld.bits.uop.expdEnd || validCountLdQ <= (LdQSize - 8).U
//       alreadyIssueLd := !ld.bits.uop.expdEnd
//     }.elsewhen (ld.bits.uop.expdEnd) {
//       syncStartLd := !alreadyIssueLd
//       alreadyIssueLd := false.B
//     }.otherwise {
//       syncStartLd := false.B
//       alreadyIssueLd := alreadyIssueLd
//     }
//   }.otherwise {
//     syncStartLd := false.B
//     alreadyIssueLd := alreadyIssueLd
//   }
//   /**
//     * Load Queue: dequeue
//     */
//   val hitLd = Wire(Vec(LdQSize, Bool()))
//   when (io.ovi_load.valid) {
//     // CAM
//     hitLd := ldQ.map(x => x.uop.sb_id === io.ovi_load.seq_id(33, 29) && 
//                           x.uop.ldestExpd === io.ovi_load.seq_id(4, 0))
//     for (i <- 0 until LdQSize) {ldQ(i).complete := ldQ(i).complete || hitLd(i)}
//   }.otherwise {
//     hitLd := VecInit(Seq.fill(LdQSize)(false.B))
//   }
//   io.wb.ld.valid := RegNext(io.ovi_load.valid)
//   io.wb.ld.bits.vd := RegNext(VecInit(UIntSplit(io.ovi_load.data)))
//   io.wb.ld.bits.uop := RegNext(Mux1H(hitLd, ldQ.map(_.uop)))
//   when (ldQ(ldDeqPtr.value).complete) {
//     ldDeqPtr := ldDeqPtr + 1.U
//     ldQ(ldDeqPtr.value).valid := false.B
//   }


//   /**
//     * Store: a dummy one
//     */
//   //---- OVI syn_start: a little verbose and not tested ----
//   val syncStartSt = RegInit(false.B)
//   val alreadyIssueSt = RegInit(false.B)
//   readyStQ := true.B
//   when (st.fire) {
//     when (st.bits.uop.expdIdx === 0.U) {
//       syncStartSt := st.bits.uop.expdEnd
//       alreadyIssueSt := !st.bits.uop.expdEnd
//     }.elsewhen (st.bits.uop.expdEnd) {
//       syncStartSt := !alreadyIssueSt
//       alreadyIssueSt := false.B
//     }.otherwise {
//       syncStartSt := false.B
//       alreadyIssueSt := alreadyIssueSt
//     }
//   }.otherwise {
//     syncStartSt := false.B
//     alreadyIssueSt := alreadyIssueSt
//   }
//   io.ovi_store.valid := st.fire
//   io.ovi_store.data := RegNext(Cat(st.bits.vs3.reverse))
//   /**
//     * Store Queue: enqueue
//     */
//   val stQ = RegInit(VecInit(Seq.fill(StQSize)(0.U.asTypeOf(new VStQEntry))))
//   val stEnqPtr = RegInit(0.U.asTypeOf(new VStQPtr))
//   val stDeqPtr = RegInit(0.U.asTypeOf(new VStQPtr))
//   //---- Enq ----
//   when (st.fire) {
//     stQ(stEnqPtr.value).valid := true.B
//     stQ(stEnqPtr.value).complete := false.B
//     stQ(stEnqPtr.value).uop := st.bits.uop
//     stEnqPtr := stEnqPtr + 1.U
//   }
//   /**
//     * Store Queue: dequeue
//     */
//   when (io.ovi_memop.sync_end) {
//     stQ.foreach(x => x.complete := x.complete || (x.uop.sb_id === io.ovi_memop.sb_id && x.valid))
//   }
//   when (stQ(stDeqPtr.value).complete) {
//     stDeqPtr := stDeqPtr + 1.U
//     stQ(stDeqPtr.value).valid := false.B
//   }
//   io.wb.st.valid := RegNext(stQ(stDeqPtr.value).complete)
//   io.wb.st.bits.uop := RegNext(stQ(stDeqPtr.value).uop)

//   //---- Output sync_start ----
//   io.ovi_memop.sync_start := syncStartLd || syncStartSt
//   io.ovi_memop.load := syncStartLd

// }
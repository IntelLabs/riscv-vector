// This is vector LSU with OVI interface. (OVI version: 1.05)
// Notes:
//  1) Only support VLEN = 256
//  2) Load and store are serial and ordered
//  3) The elements past vstart_vlfof may alter architectural state (depends on the load.seq_id from CPU)
//  4) A. Fault-only-first load acts the same as unit-stride load (active elements past reported-VL may be 1 or old value).
//     B. Whole Register instrn (OVI not mentioned): same as unit-stride operations.
//     C. vlm, vsm (OVI not mentioned): same as unit-stride with eew=8. Need pay attention: tail is agnostic, and vstart is in units of bytes.
//     D. Segment instrn (OVI not supported): for load data, assume only one segment is transmitted from CPU and it is confined in
//        one 512-b load data (this assumption is not efficient for unit-stride op, and may cause extra complexity to CPU). The
//        el_off is 0, the el_count is number of fields in one segment, el_id is offset inside each RF register.
//        For segment store, so far we use same scheme as other store instrns (not friendly to CPU to handle segment).
//  5) So far when uop.lsrcValid(2) is set, which means need-old-vd, all tails and masks are set to old vd (so you will not see FFFF).

//  Todo: 1) vstart support (consider mask_idx.item)
//        2) Optimize: send mask_idx as early as memop.sync_start; mask_idx.last_idx goes early if all remain indices are masked off
//        3) vlm, vsm: tail is agnostic, and vstart is in units of bytes.

//  Question: 1) Will el_count = 0 and sync_end.vstart = 0 if exception occurs at elemnt 0 ?
//            2) Will seq_id.el_id from CPU be guaranteed to be >= vstart?

package darecreek.lsu

import chisel3._
import chisel3.util._
import utils._
import darecreek._

class VLoadDataBuf extends Module {
  val io = IO(new Bundle {
    val raddr = Input(UInt(3.W))
    val rdata = Output(UInt(VLEN.W))
    val wen = Input(Vec(3, Bool()))
    val waddr = Input(Vec(3, UInt(3.W)))
    val wmask = Input(Vec(3, Vec(vlenb, Bool())))
    val wdata = Input(Vec(3, Vec(vlenb, UInt(8.W))))
    // For segment-instrn write
    val wen_seg = Input(Bool())
    val woffset_seg = Input(UInt((vlenbWidth-3).W)) //Write 64b each time
    val wmask_seg = Input(Vec(8, Vec(8, Bool())))  //Write 64b each time
    val wdata_seg = Input(Vec(8, Vec(8, UInt(8.W)))) //Write 64b each time
  })
  val rf = Reg(Vec(8, Vec(vlenb, UInt(8.W))))
  for (i <- 0 until 3) {
    for (k <- 0 until vlenb) {
      when(io.wen(i) && io.wmask(i)(k)) {rf(io.waddr(i))(k) := io.wdata(i)(k)}
    }
  }
  //---- Segment write. Only for VLEN = 256 ----
  for (idxRf <- 0 until 8) {
    when (io.wen_seg) {
      for (offset <- 0 until 3) {
        when (io.woffset_seg === offset.U) {
          for (i <- 0 until 8) {
            when (io.wmask_seg(idxRf)(i)) {rf(idxRf)(offset*8 + i) := io.wdata_seg(idxRf)(i)}
          }
        }
      }
    }
  }

  io.rdata := Cat(rf(io.raddr).reverse)
}

class VLsu extends Module with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val fromIQ = new Bundle {
      val ld = Flipped(Decoupled(new VLdInput))
      val st = Flipped(Decoupled(new VStInput))
    }
    val wb = new Bundle {
      val ld = ValidIO(new VLdOutput)
      val st = ValidIO(new VStOutput)
    }
    val stateIsStore = Output(Bool())
    // OVI interfaces
    val ovi_memop = new OVImemop
    val ovi_load = new OVIload
    val ovi_store = new OVIstore
    val ovi_maskIdx = new OVImaskIdx
  })

  val ld = io.fromIQ.ld
  val st = io.fromIQ.st
  val s_idle :: s_load :: s_store :: Nil = Enum(3)
  val s_idle_LD :: s_issueUops_LD :: s_busy_LD :: s_complete_LD :: Nil = Enum(4)
  val s_idle_ST :: s_issueUops_ST :: s_busy_ST :: s_complete_ST :: Nil = Enum(4)

  val state = RegInit(s_idle)
  val stateLd = RegInit(s_idle_LD)
  val stateSt = RegInit(s_idle_ST)
  val completeLd = RegInit(false.B)
  val completeSt = RegInit(false.B)

  // Top FSM
  when (state === s_idle) {
    when (ld.bits.iqEmpty && st.bits.iqEmpty) {
      state := s_idle
    }.elsewhen (st.bits.iqEmpty) {
      state := s_load
    }.elsewhen (ld.bits.iqEmpty) {
      state := s_store
    }.otherwise {
      state := Mux(isBefore(ld.bits.nextVRobIdx, st.bits.nextVRobIdx), s_load, s_store)
    }
  }.elsewhen (state === s_load) {
    state := Mux(completeLd && stateLd === s_complete_LD, s_idle, s_load)
  }.elsewhen (state === s_store) {
    state := Mux(completeSt && stateSt === s_complete_ST, s_idle, s_store)
  }.otherwise {
    state := s_idle
  }

  val readyLd = Wire(Bool())
  val readySt = Wire(Bool())
  ld.ready := (!ld.valid || readyLd) && state === s_load
  st.ready := (!st.valid || readySt) && state === s_store
  val firstLdFire = ld.fire && ld.bits.uop.expdIdx === 0.U
  val firstStFire = st.fire && st.bits.uop.expdIdx === 0.U
  io.ovi_memop.sync_start := ld.fire && ld.bits.uop.expdEnd || firstStFire

  /**
    * ---- Load ----
    */
  // Load FSM
  when (stateLd === s_idle_LD) {
    stateLd := Mux(io.ovi_memop.sync_start, 
               Mux(ld.fire && ld.bits.uop.expdEnd, s_busy_LD, s_issueUops_LD), s_idle_LD)
  }.elsewhen (stateLd === s_issueUops_LD) { //Wait for all expanded uops of one instrn issued from IQ
    stateLd := Mux(ld.fire && ld.bits.uop.expdEnd, s_busy_LD, s_issueUops_LD)
  }.elsewhen (stateLd === s_busy_LD) {
    stateLd := Mux(io.ovi_memop.sync_end, s_complete_LD, s_busy_LD)
  }.otherwise {
    stateLd := Mux(completeLd, s_idle_LD, s_complete_LD)
  }
  readyLd := stateLd === s_idle_LD || stateLd === s_issueUops_LD

  // Load Uop Table
  val ldUopTable = Reg(Vec(8, new Bundle {
    val valid = Bool()
    val uop = new VExpdUOp
  }))
  val ldBufPtrEnq = RegInit(0.U(3.W))
  ldBufPtrEnq := Mux(completeLd, 0.U, Mux(ld.fire, ldBufPtrEnq + 1.U, ldBufPtrEnq))
  when (ld.fire) {
    ldUopTable(ldBufPtrEnq).valid := ld.bits.uop.pdestVal
    ldUopTable(ldBufPtrEnq).uop := ld.bits.uop
  }
  when (completeLd) {ldUopTable.foreach(_.valid := false.B)}

  // Some ctrl signals of load
  val ctrl_ld = Reg(new LdstCtrl)
  val ctrl_ld_wire = LdstDecoder(ld.bits.uop.ctrl.funct6, ld.bits.uop.ctrl.lsrc(1))
  val sew_ld = Reg(UInt(3.W))
  val vlmul_ld = Reg(UInt(3.W))
  val eew_ld = Reg(UInt(3.W))
  val eew_ld_wire = Cat(false.B, ld.bits.uop.ctrl.funct3(1, 0))
  val destEew_ld = Reg(UInt(3.W))
  val destEew_ld_wire = Mux(ctrl_ld_wire.indexed, ld.bits.uop.info.vsew, eew_ld_wire)
  val destEewOH_ld = Reg(new SewOH)
  val destEewOH_ld_wire = SewOH(destEew_ld_wire)
  val vstart_ld = Reg(UInt(bVstart.W))
  val vl_ld = Reg(UInt(bVL.W))
  val rs2_ld = Reg(UInt(xLen.W))
  val nf_ld = Reg(UInt(3.W))
  when (firstLdFire) {
    ctrl_ld := ctrl_ld_wire
    sew_ld := ld.bits.uop.info.vsew
    vlmul_ld := ld.bits.uop.info.vlmul
    eew_ld := eew_ld_wire
    destEew_ld := destEew_ld_wire
    destEewOH_ld := destEewOH_ld_wire
    vstart_ld := ld.bits.uop.info.vstart
    vl_ld := ld.bits.uop.info.vl
    rs2_ld := ld.bits.rs2
    nf_ld := ld.bits.uop.ctrl.funct6(5, 3)
  }

  // Ld Data Buffer
  val ldDataBuf = Module(new VLoadDataBuf)
 
  /**
    * Load date reorgnize  (Only works for VLEN = 256)
    * There are three stages (Reverse/Squash/Shift), registers are inserted after 2nd stage.
    * Notes: (1) seq_id.v_reg is not used
    *        (2) one load_data can write up to 3 regs of load data buffer (512/VLEN + 1)
    */
  val seqId = io.ovi_load.seq_id.asTypeOf(new SeqId)
  val ovi_ldValid = io.ovi_load.valid
  val ldValid = io.ovi_load.valid && stateLd === s_busy_LD
  //----------- 1st: Reverse stage (for stride of -1, -2, -4) ------------
  val strideByElem = rs2_ld.asSInt >> destEew_ld
  val negStride = rs2_ld(xLen-1)
  val ldDataReversed = Mux(negStride, Mux1H(destEewOH_ld.oneHot, Seq(8,16,32,64).map(sew =>
                       Cat(UIntSplit(io.ovi_load.data, sew)))), io.ovi_load.data)
  //----------- 2nd: Squash stage (for stride of +-2, +-4) ------------
  val ldDataSplit = Seq(8,16,32,64).map(sew => UIntSplit(ldDataReversed, sew))
  val ldDataSquash2 = Mux1H(destEewOH_ld.oneHot, ldDataSplit.map(data => 
                            Cat((data.indices.collect({case i if i % 2 == 0 => data(i)}) ++
                                 data.indices.collect({case i if i % 2 == 1 => data(i)})).reverse)))
  val ldDataSquash4 = Mux1H(destEewOH_ld.oneHot, ldDataSplit.map(data => 
                            Cat((data.indices.collect({case i if i % 4 == 0 => data(i)}) ++
                                 data.indices.collect({case i if i % 4 == 1 => data(i)}) ++
                                 data.indices.collect({case i if i % 4 == 2 => data(i)}) ++
                                 data.indices.collect({case i if i % 4 == 3 => data(i)})).reverse)))
  val ldStrideIs2 = strideByElem === 2.S || strideByElem === -2.S
  val ldStrideIs4 = strideByElem === 4.S || strideByElem === -4.S
  val ldDataSquash = Reg(UInt(512.W))
  val elOff = seqId.el_off
  val elOffSquash = Reg(UInt(6.W))
  val (elOffSquash2, elOffSquash4) = (Wire(UInt(6.W)), Wire(UInt(6.W)))
  elOffSquash2 := Mux1H(destEewOH_ld.oneHot, Seq(5, 4, 3, 2).map(k => Cat(elOff(0), elOff(k, 1))))
  elOffSquash4 := Mux1H(destEewOH_ld.oneHot, Seq(5, 4, 3, 2).map(k => Cat(elOff(1, 0), elOff(k, 2))))
  when (ovi_ldValid) { // ICG (clock gating)
    ldDataSquash := Mux(ldStrideIs4, ldDataSquash4, Mux(ldStrideIs2, ldDataSquash2, ldDataReversed))
    elOffSquash := Mux(ldStrideIs4, elOffSquash4, Mux(ldStrideIs2, elOffSquash2, elOff))
  }
  val elCountSquash = RegEnable(seqId.el_count, ovi_ldValid)
  
  //----------- 3rd: Shift stage ---------------
  //
  // Three consecutive regs  |---- 256b ----||---- 256b ----||---- 256b ----|
  // ovi_load_data           |----------   512b   ----------||----  0   ----|
  // after shift                  |----------   512b   ----------|
  val seqIdReg = RegEnable(seqId, ovi_ldValid)
  val el_id_high = Wire(UInt(3.W)) // log2(8)
  val el_id_low = Wire(UInt(vlenbWidth.W))
  el_id_low := Mux1H(destEewOH_ld.oneHot, Seq(4, 3, 2, 1).map(i => seqIdReg.el_id(i, 0)))
  el_id_high := Mux1H(destEewOH_ld.oneHot, Seq(4, 3, 2, 1).map(i => seqIdReg.el_id(i+3, i+1)))
  val nElemVLEN = Wire(UInt(vlenbWidth.W))
  nElemVLEN := Mux1H(destEewOH_ld.oneHot, Seq(vlenb.U, (vlenb/2).U, (vlenb/4).U, (vlenb/8).U))
  val shiftElm = Wire(UInt(7.W))
  shiftElm := elOffSquash -& el_id_low
  val shiftRight = shiftElm + nElemVLEN
  val sub1 = shiftElm              // shiftRight - nElemVLEN
  val sub2 = shiftElm - nElemVLEN  // shiftRight - nElemVLEN*2
  //---- The right-shift-bytes should be in range(0, vlenb-1) to reduce hardware complexity,
  //---- if shiftRight_bytes >= vlenb, should subtract vlenb (needSub1) or 2*vlenb (needSub2)
  val needSub1 = Mux1H(destEewOH_ld.oneHot, Seq(0,1,2,3).map(i => shiftRight(6-i, 5-i) === 1.U))
  val needSub2 = Mux1H(destEewOH_ld.oneHot, Seq(0,1,2,3).map(i => shiftRight(6-i, 5-i) === 2.U))
  val finalShiftR = Wire(UInt(6.W))
  finalShiftR := Mux(needSub2, sub2, Mux(needSub1, sub1, shiftRight))
  val shiftBytes = Wire(UInt(6.W))
  shiftBytes := Mux1H(destEewOH_ld.oneHot, Seq(0, 1, 2, 3).map(finalShiftR << _))

  val preShift = Cat(ldDataSquash, 0.U(VLEN.W))
  val preShiftVec = VecInit(UIntSplit(preShift, 8))
  // Reg or Wire? (timing concern)
  val afterShiftVec = Wire(Vec(vlenb*3, UInt(8.W)))
  // Shift-right shiftBytes bytes (by log2(vlenb) sub-shifts)
  afterShiftVec := (shiftBytes.asBools).zip(Seq(1,2,4,8,16)).foldLeft(preShiftVec) {
    case (data, (bit, n)) => {
      Mux(bit, VecInit(data.drop(n) ++ Seq.fill(n)(0.U(8.W))), data)
    }
  }

  // el_count mask
  val el_count_bytes = Wire(UInt(7.W))
  el_count_bytes := Mux1H(destEewOH_ld.oneHot, Seq(0, 1, 2, 3).map(elCountSquash << _))
  val elCount_to_1s = Wire(UInt(64.W))
  elCount_to_1s := Mux(el_count_bytes === 0.U, 0.U, UIntToCont1s(el_count_bytes, 7)(63, 0))
  val elCountMask = Wire(UInt(96.W)) // 3 * vlenb = 96
  val el_id_low_bytes = Wire(UInt(vlenbWidth.W))
  el_id_low_bytes := Mux1H(destEewOH_ld.oneHot, Seq(0, 1, 2, 3).map(el_id_low << _))
  elCountMask := elCount_to_1s << el_id_low_bytes
  val elCountMaskFinal = Wire(UInt(96.W)) // 3 * vlenb = 96
  elCountMaskFinal := Mux(needSub2, elCountMask << 64, Mux(needSub1, elCountMask << 32, elCountMask))

  // mask
  val loadMask_bytes = Reg(UInt(64.W)) 
  loadMask_bytes := Mux(io.ovi_load.mask_valid,
                        Mux1H(destEewOH_ld.oneHot, Seq(1, 2, 4, 8).map(k => 
                         Cat(io.ovi_load.mask(64/k-1, 0).asBools.map(bit => Fill(k, bit)).reverse))),
                        ~(0.U(64.W)))
  val loadMask = Wire(UInt(96.W))
  loadMask := Cat(0.U(32.W), loadMask_bytes) << el_id_low_bytes
  val loadMaskFinal = Wire(UInt(96.W))
  loadMaskFinal := Mux(needSub2, loadMask << 64, Mux(needSub1, loadMask << 32, loadMask))

  //---- Write Load Data Buffer ----
  val ldMaskWen = elCountMaskFinal & loadMaskFinal
  val ldValidWen = RegNext(ldValid)
  val ld_waddr_inc = Wire(Vec(3, UInt(3.W)))
  for (i <- 0 until 3) {
    ldDataBuf.io.wdata(i) := afterShiftVec.slice(i*vlenb, (i+1)*vlenb)
    ldDataBuf.io.wmask(i) := ldMaskWen(i*32 + 31, i*32).asBools
    ld_waddr_inc(i) := Mux(needSub2, i.U - 2.U, Mux(needSub1, i.U - 1.U, i.U))
    ldDataBuf.io.waddr(i) := el_id_high + ld_waddr_inc(i)
  }
  ldDataBuf.io.wen(2) := ldValidWen
  ldDataBuf.io.wen(1) := ldValidWen && !needSub2
  ldDataBuf.io.wen(0) := ldValidWen && !needSub1 && !needSub2


  /**
    *  Write Load Data Buffer for Segment instrn
    */
  val veew_minus_vsew = destEew_ld - sew_ld
  val vemul = vlmul_ld + veew_minus_vsew
  val seg_wmask = io.ovi_load.mask(0)
  val seg_ValidWen = RegNext(ldValid)
  val seg_wdata = Reg(Vec(8, Vec(8, UInt(8.W))))
  val seg_wen = Reg(Vec(8, UInt(8.W)))
  val cntSeg = Reg(UInt((vlenbWidth + 3).W))
  cntSeg := Mux(stateLd === s_idle, 0.U, Mux(seg_ValidWen, cntSeg +
                                  Mux1H(destEewOH_ld.oneHot, Seq(1.U, 2.U, 4.U, 8.U)), cntSeg))
  // If emul > 1, each filed will take multiple registers, idxRf -> idxRf/emul
  // Note: for segment instrn, emul <= 4
  def segDataSelect(vemul: UInt, data: Seq[UInt], idxRf: Int, eewByte: Int, j: Int) = {
    Mux1H(Seq(
      (vemul === 0.U || vemul(2)) -> data(idxRf * eewByte + j),
      (vemul === 1.U) -> data((idxRf/2) * eewByte + j),
      (vemul === 2.U) -> data((idxRf/4) * eewByte + j),
      (vemul === 3.U) -> data((idxRf/8) * eewByte + j)
    ))
  }
  for (idxRf <- 0 until 8) {
    when (ldValid) {
      when (destEewOH_ld.is8) {
        seg_wen(idxRf) := "b0000_0001".U << cntSeg(2, 0) 
        seg_wdata(idxRf)(cntSeg(2, 0)) := segDataSelect(vemul, UIntSplit(io.ovi_load.data, 8), idxRf, 1, 0)
      }.elsewhen (destEewOH_ld.is16) {
        seg_wen(idxRf) := "b0000_0011".U << Cat(cntSeg(2, 1), 0.U(1.W))
        for (j <- 0 until 2) {
          seg_wdata(idxRf)(Cat(cntSeg(2, 1), j.U(1.W))) := segDataSelect(vemul, UIntSplit(io.ovi_load.data, 8), idxRf, 2, j)
        }
      }.elsewhen (destEewOH_ld.is32) {
        seg_wen(idxRf) := "b0000_1111".U << Cat(cntSeg(2), 0.U(2.W))
        for (j <- 0 until 4) {
          seg_wdata(idxRf)(Cat(cntSeg(2), j.U(2.W))) := segDataSelect(vemul, UIntSplit(io.ovi_load.data, 8), idxRf, 4, j)
        }
      }.otherwise {
        seg_wen(idxRf) := "b1111_1111".U
        for (j <- 0 until 8) {
          seg_wdata(idxRf)(j) := segDataSelect(vemul, UIntSplit(io.ovi_load.data, 8), idxRf, 8, j)
        }
      }
    }.otherwise {
      seg_wen(idxRf) := 0.U
    }
  }

  val vemulReg = RegNext(vemul)
  val emulKeep_seg = Wire(UInt(8.W))
  // Note: for segment instrn, emul <= 4
  // Only work for VLEN = 256
  when (vemulReg === 0.U || vemulReg(2)) {
    emulKeep_seg := "b1111_1111".U
  }.elsewhen (vemulReg === 1.U) {
    emulKeep_seg := Mux(cntSeg(vlenbWidth+2, 5) === 0.U, "b0101_0101".U, "b1010_1010".U)
  }.otherwise {
    emulKeep_seg := Mux(cntSeg(vlenbWidth+2, 5) === 0.U, "b0001_0001".U,
                    Mux(cntSeg(vlenbWidth+2, 5) === 1.U, "b0010_0010".U,
                    Mux(cntSeg(vlenbWidth+2, 5) === 2.U, "b0100_0100".U, "b1000_1000".U)))
  }
  val nfKeep_seg = Wire(UInt(8.W))
  val nf_oneHot = Seq.tabulate(8)(nf => nf_ld === nf.U)
  when (vemulReg === 0.U || vemulReg(2)) {
    nfKeep_seg := Mux1H(nf_oneHot, Seq.tabulate(8)(i => ("b" + "0"*(7-i) + "1"*(i+1)).U))
  }.elsewhen (vemulReg === 1.U) {
    nfKeep_seg := Mux1H(nf_oneHot.take(4), Seq.tabulate(4)(i => ("b" + "00"*(3-i) + "11"*(i+1)).U))
  }.otherwise {
    nfKeep_seg := Mux1H(nf_oneHot.take(2), Seq.tabulate(2)(i => ("b" + "0000"*(1-i) + "1111"*(i+1)).U))
  }

  for (idxRf <- 0 until 8) {
    ldDataBuf.io.wmask_seg(idxRf) := (Fill(8, (emulKeep_seg & nfKeep_seg)(idxRf)) & seg_wen(idxRf)).asBools
  }
  ldDataBuf.io.wen_seg := seg_ValidWen && ctrl_ld.segment
  ldDataBuf.io.woffset_seg := cntSeg(vlenbWidth-1, 3)
  ldDataBuf.io.wdata_seg := seg_wdata


  /**
    *---- Initial ldDataBuf: Old_vd  or  1s (when stateLd === s_issueUops_LD or s_idle_LD)
    */
  when (ld.fire) {
    ldDataBuf.io.wen(0) := true.B
    ldDataBuf.io.waddr(0) := ldBufPtrEnq
    ldDataBuf.io.wmask(0).foreach(_ := true.B)
    ldDataBuf.io.wdata(0) := Mux(ld.bits.uop.ctrl.lsrcVal(2), VecInit(UIntSplit(ld.bits.oldVd, 8)), 
      VecInit(UIntSplit(Mux1H(destEewOH_ld_wire.oneHot, Seq(8, 16, 32, 64).map(w => Fill(VLEN/w, ~(0.U(w.W))))), 8)))
  }

  //-------- Write-back of load --------
  val ldBufPtrDeq = RegInit(0.U(3.W))
  ldBufPtrDeq := Mux(stateLd === s_complete_LD && ldUopTable(ldBufPtrDeq).valid, ldBufPtrDeq + 1.U, 0.U)
  completeLd := stateLd === s_complete_LD && (!ldUopTable(ldBufPtrDeq + 1.U).valid || ldBufPtrDeq === 7.U)
  io.wb.ld.valid := stateLd === s_complete_LD && !completeLd
  ldDataBuf.io.raddr := ldBufPtrDeq
  io.wb.ld.bits.vd := ldDataBuf.io.rdata
  io.wb.ld.bits.uop := ldUopTable(ldBufPtrDeq).uop

  /**
    * ---- Store ----
    */
  // Store FSM
  when (stateSt === s_idle_ST) {
    stateSt := Mux(firstStFire, s_issueUops_ST, s_idle_ST)
  }.elsewhen (stateSt === s_issueUops_ST) { //Wait for all expanded uops of one instrn issued from IQ
    stateSt := Mux(st.fire && st.bits.uop.expdEnd, s_busy_ST, s_issueUops_ST)
  }.elsewhen (stateSt === s_busy_ST) {
    stateSt := Mux(io.ovi_memop.sync_end, s_complete_ST, s_busy_ST)
  }.otherwise {
    stateSt := Mux(completeSt, s_idle_ST, s_complete_ST)
  }
  val maxStCredit = 64.U(7.W)
  val cntStCredit = RegInit(0.U(7.W))
  readySt := (stateSt === s_idle_ST || stateSt === s_issueUops_ST) && cntStCredit =/= maxStCredit
  io.ovi_store.valid := st.fire
  io.ovi_store.data := st.bits.vs3
  cntStCredit := Mux(io.ovi_store.credit === io.ovi_store.valid, cntStCredit,
                     Mux(io.ovi_store.credit, cntStCredit - 1.U, cntStCredit + 1.U))
  assert(cntStCredit <= 64.U, "cntStCredit should <= 64")

  // Store Uop Table
  val stUopTable = Reg(Vec(8, new Bundle {
    val valid = Bool()
    val uop = new VExpdUOp
  }))
  val stBufPtrEnq = RegInit(0.U(3.W))
  stBufPtrEnq := Mux(completeSt, 0.U, Mux(st.fire, stBufPtrEnq + 1.U, stBufPtrEnq))
  when (st.fire) {
    stUopTable(stBufPtrEnq).valid := true.B
    stUopTable(stBufPtrEnq).uop := st.bits.uop
  }
  when (completeSt) {stUopTable.foreach(_.valid := false.B)}
  
  // Write-back of store
  val stBufPtrDeq = RegInit(0.U(3.W))
  stBufPtrDeq := Mux(stateSt === s_complete_ST && stUopTable(stBufPtrDeq).valid, stBufPtrDeq + 1.U, 0.U)
  completeSt := stateSt === s_complete_ST && (!stUopTable(stBufPtrDeq + 1.U).valid || stBufPtrDeq === 7.U)
  io.wb.st.valid := stateSt === s_complete_ST
  io.wb.st.bits.uop := stUopTable(stBufPtrDeq).uop

  // Some ctrl signals of store
  val ctrl_st = Reg(new LdstCtrl)
  val ctrl_st_wire = LdstDecoder(st.bits.uop.ctrl.funct6, st.bits.uop.ctrl.lsrc(1))
  val eew_st = Reg(UInt(3.W))
  val eew_st_wire = Cat(false.B, st.bits.uop.ctrl.funct3(1, 0))
  val vstart_st = Reg(UInt(bVstart.W))
  val vl_st = Reg(UInt(bVL.W))
  when (firstStFire) {
    ctrl_st := ctrl_st_wire
    eew_st := eew_st_wire
    vstart_st := st.bits.uop.info.vstart
    vl_st := st.bits.uop.info.vl
  }

  /**
    * Mask_idx generation: shared by load and store
    */
  //--- Mask Buffer: shared by load and store
  val stateIsSTORE = state === s_store
  io.stateIsStore := stateIsSTORE
  val vmask_shift = Wire(UInt(VLEN.W))
  vmask_shift := Mux(stateIsSTORE, st.bits.vmask, ld.bits.vmask) >>
                 Mux(stateIsSTORE, st.bits.uop.info.vstart, ld.bits.uop.info.vstart)
  val maskBuf = RegEnable(vmask_shift, firstLdFire || firstStFire)
  //---- Index Buffer (for vector indexed instrn): shared by load and store
  val idxBuf = Reg(Vec(8, Vec(vlenb, UInt(8.W))))
  val bufPtrEnq = Mux(stateIsSTORE, stBufPtrEnq, ldBufPtrEnq)
  when (ld.fire || st.fire) {idxBuf(bufPtrEnq) := UIntSplit(Mux(stateIsSTORE, st.bits.vs2, ld.bits.vs2), 8)}

  //---- Some ctrl signals shared by load and store
  val vl_ldst = Mux(stateIsSTORE, vl_st, vl_ld)
  val vstart_ldst = Mux(stateIsSTORE, vstart_st, vstart_ld)
  val ctrl_ldst = Mux(stateIsSTORE, ctrl_st, ctrl_ld)
  val ctrl_ldst_wire = Mux(stateIsSTORE, ctrl_st_wire, ctrl_ld_wire)
  val vm_ldst = Mux(stateIsSTORE, st.bits.uop.ctrl.vm, ld.bits.uop.ctrl.vm)
  val eew_ldst = Mux(stateIsSTORE, eew_st, eew_ld)
  //---- Begin generate mask_idx
  val cntMaskIdxCredit = RegInit(0.U(2.W))
  val nLdMask = vl_ldst - vstart_ldst
  val sendingMaskIdx = RegInit(false.B)
  val ldMaskOffset = Reg(UInt(bVL.W))
  val ldMaskOffsetUpdate = ldMaskOffset + Mux(ctrl_ldst.indexed, 1.U, 64.U)
  val stopSendingMaskIdx = ldMaskOffsetUpdate >= nLdMask
  when (io.ovi_memop.sync_start && (ctrl_ldst_wire.indexed || !ctrl_ldst_wire.mask && !vm_ldst)) {
    sendingMaskIdx := true.B
  }.elsewhen (stopSendingMaskIdx || stateLd === s_idle_LD || stateLd === s_issueUops_LD
                                 || stateSt === s_idle_ST || stateSt === s_issueUops_ST) {
    sendingMaskIdx := false.B
  }
  when (io.ovi_memop.sync_start) {
    ldMaskOffset := 0.U
  }.elsewhen (sendingMaskIdx) {
    ldMaskOffset := Mux(stopSendingMaskIdx, ldMaskOffset, ldMaskOffsetUpdate)
  }
  io.ovi_maskIdx.valid := sendingMaskIdx && !cntMaskIdxCredit(1)  //max No. of credit = 2
  
  val maskIdx_itemMask = Mux1H(Seq.tabulate(4)(i => ldMaskOffset(7, 6) === i.U),
                        Seq.tabulate(4)(i => maskBuf(64*i+63, 64*i)))
  val maskIdx_itemIdx_H = maskBuf(ldMaskOffset)
  val eewLdIdx = SewOH(eew_ldst)
  val ldIdxOffset = ldMaskOffset + vstart_ldst
  val ldIdxOffsetFinal = Wire(UInt(bVL.W))
  ldIdxOffsetFinal := Mux1H(eewLdIdx.oneHot, Seq.tabulate(4)(ldIdxOffset << _))
  val ldIdxOffsetArray = Seq.tabulate(8)(ldIdxOffsetFinal + _.U)
  val maskIdx_itemIdx = Cat(Seq.tabulate(8)(
    i => idxBuf(ldIdxOffsetArray(i)(vlenbWidth+2, vlenbWidth))(ldIdxOffsetArray(i)(vlenbWidth-1, 0))
  ).reverse)
  io.ovi_maskIdx.item := Mux(ctrl_ldst.indexed, Cat(maskIdx_itemIdx_H, maskIdx_itemIdx),
                                              Cat(false.B, maskIdx_itemMask))
  io.ovi_maskIdx.last_idx := Mux(ctrl_ldst.indexed, ldMaskOffset === nLdMask - 1.U, false.B)
  cntMaskIdxCredit := Mux(io.ovi_maskIdx.credit === io.ovi_maskIdx.valid, cntMaskIdxCredit,
                          Mux(io.ovi_maskIdx.credit, cntMaskIdxCredit - 1.U, cntMaskIdxCredit + 1.U))
  assert(cntMaskIdxCredit =/= 3.U, "cntMaskIdxCredit should < 3")

  // io.ovi_memop.load := io.ovi_memop.sync_start && !io.ovi_store.valid
}

object Main extends App {

  println("Generating the VLSU hardware")

  emitVerilog(new VLsu(), Array("--target-dir", "generated"))

}
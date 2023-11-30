/***************************************************************************************
*Copyright (c) 2023-2024 Intel Corporation
*Vector Acceleration IP core for RISC-V* is licensed under Mulan PSL v2.
*You can use this software according to the terms and conditions of the Mulan PSL v2.
*You may obtain a copy of Mulan PSL v2 at:
*        http://license.coscl.org.cn/MulanPSL2
*THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
*EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
*MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*See the Mulan PSL v2 for more details.
***************************************************************************************/

package darecreek

import chisel3._
import chisel3.util._
import darecreek.lsu._

/** Parallel ordered expanding method for renaming
  * parallel input --> calc expanded length --> expd len accumulator --> expander --> RF addrs update --> parallel output
  * First version: insert pipeline regs between accumulator and expander
  *
  * Requirement: inputs valid must be consecutive
  * Others: all inputs of io.in share one 'ready'. 'Ready' of all outputs are from one source.
  */
class ParallelExpander extends Module {
  val io = IO(new Bundle {
    val in = Vec(VRenameWidth, Flipped(Decoupled(new VMicroOp)))
    val out = Vec(VRenameWidth, Decoupled(new VExpdUOpForRename))
  })

  val ctrl = io.in.map(x => x.bits.ctrl)
  val info = io.in.map(x => x.bits.info)
  val fire = io.in(0).fire
  val canOut = io.out(0).ready

  val IDLE = 0.U(1.W)
  val BUSY = 1.U(1.W)
  val state = RegInit(UInt(1.W), IDLE)
  val busy_end = Wire(Bool())

  when (state === IDLE) {
    state := Mux(fire, BUSY, IDLE)
  }.otherwise {
    state := Mux(!fire && busy_end, IDLE, BUSY)
  }

  /**
    * Calculate expanded length
    */
  val expdLen = Wire(Vec(VRenameWidth, UInt(4.W)))
  val destEew = io.in.map(_.bits.info.destEew)
  val lmul = Wire(Vec(VRenameWidth, UInt(4.W)))
  val veew = Wire(Vec(VRenameWidth, UInt(3.W)))
  val veew_minus_vsew = Wire(Vec(VRenameWidth, UInt(3.W)))
  val vemul = Wire(Vec(VRenameWidth, UInt(3.W)))
  val emul = Wire(Vec(VRenameWidth, UInt(4.W)))
  val ldstCtrl = Wire(Vec(VRenameWidth, new LdstCtrl))
  val expdLen_indexVd = Wire(Vec(VRenameWidth, UInt(4.W)))
  for (i <- 0 until VRenameWidth) {
    lmul(i) := Vlmul_to_lmul(info(i).vlmul)
    // EMUL = (EEW/SEW)*LMUL
    veew(i) := Cat(false.B, ctrl(i).funct3(1,0)) //!! Only for ld/st !!
    veew_minus_vsew(i) := veew(i) - info(i).vsew
    vemul(i) := info(i).vlmul + veew_minus_vsew(i)
    emul(i) := Vlmul_to_lmul(vemul(i))
    ldstCtrl(i) := LdstDecoder(ctrl(i).funct6, ctrl(i).lsrc(1))

    val nf_ldst = Wire(UInt(4.W))
    nf_ldst := ctrl(i).funct6(5,3) +& 1.U
    // For ld/st indexed and segment-indexed instrns, the final expdLen is the larger of data and indices 
    val expdLen_ldst = Mux(ldstCtrl(i).wholeReg, nf_ldst,
                       Mux(ldstCtrl(i).indexed, Mux(veew_minus_vsew(i)(2), lmul(i), emul(i)),
                           emul(i)))
    val expdLen_segVd = Wire(UInt(4.W))
    expdLen_segVd := lmul(i) * nf_ldst
    val expdLen_segVs2 = emul(i)
    val expdLen_seg = Mux(expdLen_segVd > expdLen_segVs2, expdLen_segVd, expdLen_segVs2)
    // For ld/st indexed and segment-indexed instrns, the pdestVal should stop at expd_len of vd
    expdLen_indexVd(i) := Mux(ldstCtrl(i).segment, expdLen_segVd, emul(i))
    val perm_vmv_vfmv = ctrl(i).perm && ctrl(i).funct6 === "b010000".U
                                                 // 15.1     or      //15.4/5/6: vmsb(o/i)f
    val mask_onlyOneReg = ctrl(i).mask && (ctrl(i).funct6(3) || ctrl(i).funct6(2) && !ctrl(i).lsrc(0)(4))
    //---- expdLen ----
    when (ctrl(i).isLdst && !ldstCtrl(i).mask) {
      expdLen(i) := Mux(ldstCtrl(i).segment, expdLen_seg, expdLen_ldst) 
    }.elsewhen ((ldstCtrl(i).mask && ctrl(i).isLdst) || perm_vmv_vfmv || mask_onlyOneReg) {
      expdLen(i) := 1.U
    }.elsewhen (ctrl(i).widen || ctrl(i).widen2 || ctrl(i).narrow) {
      expdLen(i) := Mux(info(i).vlmul(2), 1.U, lmul(i) << 1)  // If lmul < 1, expdLen = 1 for widen/narrow
    }.elsewhen (ctrl(i).funct6 === "b100111".U && ctrl(i).funct3 === "b011".U) {//Whole register move
      expdLen(i) := ctrl(i).lsrc(0)(2, 0) +& 1.U
    }.otherwise {
      expdLen(i) := lmul(i)
    }
  }

  /**
    * Expanded length accumulator
    */
  val expdLenAcc = Wire(Vec(VRenameWidth + 1, UInt(ExpdLenAccWidth.W)))
  expdLenAcc(0) := 0.U
  for (i <- 1 until VRenameWidth + 1) {
    expdLenAcc(i) := Mux(io.in(i-1).valid, expdLen(i-1), 0.U) + expdLenAcc(i-1)
  }
  val expdLenAccRemain = Reg(Vec(VRenameWidth + 1, UInt(ExpdLenAccWidth.W)))
  val expdLenAccRemainUpdate = expdLenAccRemain.map(_ - VRenameWidth.U)
  for (i <- 0 until VRenameWidth + 1) {
    expdLenAccRemain(i) := Mux(fire, expdLenAcc(i),
                        Mux(canOut && state === BUSY, expdLenAccRemainUpdate(i), 
                            expdLenAccRemain(i)))
  }
  busy_end := state === BUSY && expdLenAccRemain(VRenameWidth) <= VRenameWidth.U && canOut

  val hasValid = Cat(io.in.map(_.valid)).orR
  val canIn = state === IDLE || busy_end
  for (i <- 0 until VRenameWidth) {io.in(i).ready := !hasValid || canOut && canIn}

  //---- get some ctrl signals ----
  val v_ext = ctrl.map(c => RegEnable(c.alu && c.funct3 === "b010".U && c.funct6 === "b010010".U, fire))
  val ldstCtrlReg = ldstCtrl.map(x => RegEnable(x, fire))
  val expdLen_indexVd_reg = expdLen_indexVd.map(x => RegEnable(x, fire))

  /**
    * Expander
    */
  val io_in_updateDestEew = Wire(Vec(VRenameWidth, new VMicroOp))
  for (i <- 0 until VRenameWidth) {
    io_in_updateDestEew(i) := io.in(i).bits
    io_in_updateDestEew(i).info.destEew := destEew(i)
  }
  val expdIn = io_in_updateDestEew.map(x => RegEnable(x, fire))
  val expdLenReg = RegEnable(expdLen, fire)
  val expdOut = Wire(Vec(VRenameWidth, new VMicroOp))
  val expdOutValid = Wire(Vec(VRenameWidth, Bool()))
  val expdIdx = Wire(Vec(VRenameWidth, UInt(3.W)))
  val veew_minus_vsew_r = veew_minus_vsew.map(RegEnable(_, fire))
  val veew_minus_vsew_out = Wire(Vec(VRenameWidth, UInt(3.W)))
  val v_ext_out = Wire(Vec(VRenameWidth, Bool()))
  // for (i <- 0 until VRenameWidth + 1) {
  for (i <- 0 until VRenameWidth) {
    val subt = (0 until VRenameWidth + 1) map { j =>
      i.U - expdLenAccRemain(j)
    }
    val lessThan = VecInit(subt.map(_(ExpdLenAccWidth-1))).asUInt
    // E.g., "1110000" -> "0010000"
    // val oneHotGen = lessThan ^ Cat(lessThan(VRenameWidth-1, 0), false.B)
    // E.g., lessThan "1110000" -> oneHotGen "001000"  -> 
    val oneHotGen = lessThan(VRenameWidth, 1) ^ Cat(lessThan(VRenameWidth-1, 1), false.B)
    expdOut(i) := Mux1H(oneHotGen, expdIn)
    expdIdx(i) := Mux1H(Cat(false.B, oneHotGen), subt)
    expdOutValid(i) := lessThan.orR
    veew_minus_vsew_out(i) := Mux1H(oneHotGen, veew_minus_vsew_r)
    v_ext_out(i) := Mux1H(oneHotGen, v_ext)
    io.out(i).bits.expdLen := Mux1H(oneHotGen, expdLenReg)
  }

  /**
    * RF addresses update
    */
  for (i <- 0 until VRenameWidth) {
    io.out(i).bits.ctrl := expdOut(i).ctrl
    io.out(i).bits.info := expdOut(i).info
    io.out(i).bits.vRobIdx := expdOut(i).vRobIdx
    io.out(i).bits.sb_id := expdOut(i).sb_id

    io.out(i).valid := state === BUSY && expdOutValid(i)
    val ctrl = expdOut(i).ctrl
    val info = expdOut(i).info

    // // Reg addr increment of EEW part reg
    // val eewSide_inc = Mux(veew_minus_vsew_out(i) === 5.U, 0.U,
    //                Mux(veew_minus_vsew_out(i) === 6.U, expdIdx(i) >> 2,
    //                Mux(veew_minus_vsew_out(i) === 7.U, expdIdx(i) >> 1,
    //                    expdIdx(i))))
    
    // out lsrc(1), which is vs2
    val lsrc1_inc = Wire(UInt(3.W))
    when (ctrl.widen || v_ext_out(i) && ctrl.lsrc(0)(2,1) === 3.U) {
      lsrc1_inc := expdIdx(i) >> 1
    }.elsewhen (v_ext_out(i) && ctrl.lsrc(0)(2,1) === 2.U) {
      lsrc1_inc := expdIdx(i) >> 2
    }.elsewhen (v_ext_out(i) && ctrl.lsrc(0)(2,1) === 1.U) {
      lsrc1_inc := expdIdx(i) >> 3
    }.elsewhen (ctrl.funct6 === "b010100".U) { //VMUNARY0
      lsrc1_inc := 0.U
    }.otherwise {
      lsrc1_inc := expdIdx(i)
    }
    io.out(i).bits.lsrcExpd(1) := ctrl.lsrc(1) + lsrc1_inc
    // out lsrc(1) valid
    io.out(i).bits.lsrcValExpd(1) := ctrl.lsrcVal(1)

    // out lsrc(0), which is vs1
    io.out(i).bits.lsrcExpd(0) := ctrl.lsrc(0) +            //vcompress
              Mux(ctrl.redu || (ctrl.funct6 === "b010111".U && ctrl.funct3 === 2.U), 0.U, 
              Mux(ctrl.widen || ctrl.widen2 || ctrl.narrow, expdIdx(i) >> 1, expdIdx(i)))
    // out lsrc(0) valid
    io.out(i).bits.lsrcValExpd(0) := ctrl.lsrcVal(0)

    //------ Need_old_value ----------
    val vlMax = Wire(UInt(bVL.W))
    val sew = SewOH(info.vsew)
    val lmul = Vlmul_to_lmul(info.vlmul)
    vlMax := Mux1H(Seq(
      sew.is8  -> Cat(lmul, 0.U((log2Up(vlenb)).W)),
      sew.is16 -> Cat(lmul, 0.U((log2Up(vlenb) -1).W)),
      sew.is32 -> Cat(lmul, 0.U((log2Up(vlenb) -2).W)),
      sew.is64 -> Cat(lmul, 0.U((log2Up(vlenb) -3).W)),
    ))
    val maskNeedOldVd = !ctrl.vm && !info.ma
    val perm_vmvnrr = ctrl.perm && ctrl.funct6 === "b100111".U
    val perm_vmvsx_vfmvsf = ctrl.perm && ctrl.funct6 === "b010000".U && ctrl.vx
    //vslideup: !!!! Lack of judgement of whether offset != 0, but rs1 is not visible here so far, and immediate of offset 0 seems meaningless
    val slideUpOffset = ctrl.funct6 === "b001110".U && (ctrl.funct3(0) === ctrl.funct3(1) && ctrl.funct3(1) =/= ctrl.funct3(2))
    val vcompress = ctrl.funct6 === "b010111".U && ctrl.funct3 === "b010".U
                                           // 15.1     or      //15.4/5/6: vmsb(o/i)f
    val mask_onlyOneReg = ctrl.mask && (ctrl.funct6(3) || ctrl.funct6(2) && !ctrl.lsrc(0)(4))
    // !! Temp, debug, since Spike does not support ta to write all 1's"
    val tailIsAgnostic = ctrl.narrow_to_1 || mask_onlyOneReg || (ldstCtrlReg(i).mask && ctrl.isLdst)
    val noTail = info.vl === vlMax && !ctrl.redu && !perm_vmvsx_vfmvsf && !vcompress || perm_vmvnrr || (ldstCtrlReg(i).wholeReg && ctrl.isLdst)
    val tailNeedOldVd = !(info.ta || tailIsAgnostic || noTail)
    val noWriteback = ctrl.store || ctrl.rdVal
    val needOldVd = (maskNeedOldVd || tailNeedOldVd || info.vstart =/= 0.U || slideUpOffset) && !noWriteback

    io.out(i).bits.lsrcValExpd(2) := ctrl.lsrcVal(2) || needOldVd

    // Reg addr increment of SEW part reg
    val sewSide_inc = Mux(veew_minus_vsew_out(i) === 3.U, 0.U,
                   Mux(veew_minus_vsew_out(i) === 2.U, expdIdx(i) >> 2,
                   Mux(veew_minus_vsew_out(i) === 1.U, expdIdx(i) >> 1,
                       expdIdx(i))))
    val sewSide_valid = Mux(veew_minus_vsew_out(i) === 3.U, expdIdx(i) === 7.U,
                   Mux(veew_minus_vsew_out(i) === 2.U, expdIdx(i)(1, 0) === 3.U,
                   Mux(veew_minus_vsew_out(i) === 1.U, expdIdx(i)(0),
                       true.B)))

    // out ldest (include store vs3)
    val ldest_inc = Wire(UInt(3.W))
    when (ldstCtrlReg(i).indexed && ctrl.isLdst) {
      ldest_inc := sewSide_inc
    }.elsewhen (ctrl.narrow) {
      ldest_inc := expdIdx(i) >> 1
    }.elsewhen (ctrl.redu || ctrl.narrow_to_1) {
      ldest_inc := 0.U
    }.otherwise {
      ldest_inc := expdIdx(i)
    }
    io.out(i).bits.ldestExpd := ctrl.ldest + ldest_inc

    when (ldstCtrlReg(i).indexed && ctrl.load) {
      io.out(i).bits.ldestValExpd := expdIdx(i) < expdLen_indexVd_reg(i)
    }.elsewhen (ldstCtrlReg(i).indexed && ctrl.load) {
      io.out(i).bits.ldestValExpd := sewSide_valid
    }.elsewhen (ctrl.narrow_to_1 || ctrl.redu) {
      io.out(i).bits.ldestValExpd := expdIdx(i) === io.out(i).bits.expdLen - 1.U
    }.elsewhen (ctrl.narrow) {
      io.out(i).bits.ldestValExpd := expdIdx(i)(0) || io.out(i).bits.expdLen === 1.U
    }.otherwise {
      io.out(i).bits.ldestValExpd := ctrl.ldestVal
    }
  
  io.out(i).bits.lmaskValExpd := !ctrl.vm

  io.out(i).bits.expdIdx := expdIdx(i)

  io.out(i).bits.expdEnd := expdIdx(i) === io.out(i).bits.expdLen - 1.U
  }

  /**
    * Debug print
    */
  // val debugCnt = RegInit(0.U(8.W))
  // for (i <- 0 until VRenameWidth) {
  //   when (io.out(0).fire && io.out(i).valid) {
  //     printf(p"----Get expanded uop (count$debugCnt) with sb_id ${io.out(i).bits.sb_id},  ") 
  //     printf(p"vd ${io.out(i).bits.ldestExpd} (valid ${io.out(i).bits.ldestValExpd}),  ")
  //     printf(p"expdIdx ${io.out(i).bits.expdIdx} / ${io.out(i).bits.expdLen} \n")
  //   }
  // }
  // when (io.out(0).fire) {
  //   debugCnt := debugCnt + 1.U
  // }
}
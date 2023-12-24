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

/**
  * IssueBlock: issue queues + physical register file
  *   So far only one arith IQ is supported
  */

package darecreek

import chisel3._
import chisel3.util._
import utils._
import darecreek.lsu._

class GetScalarOperand extends Bundle {
  val exu = new Bundle {
    val addr = Output(new VRobPtr)
    val data = Input(UInt(xLen.W))
  }
  val ls = new Bundle {
    val addr = Output(new VRobPtr)
    val data = Input(UInt(xLen.W))
  }
}

class VIssueBlock extends Module {
  val io = IO(new Bundle {
    val flush = Input(Bool())
    // from Dispatch
    val in = new Bundle {
      val toArithIQ = Vec(VRenameWidth, Flipped(Decoupled(new VExpdUOp)))
      val toLsIQ = Vec(VRenameWidth, Flipped(Decoupled(new VExpdUOp)))
    }
    // from BusyTable
    val fromBusyTable = Vec(VRenameWidth, Vec(4, Input(Bool())))
    // Read rs1 from VQ
    val get_rs1 = new GetScalarOperand
    // EXU
    val toExu = new Bundle {
      val bits = Output(new VExuInput)
      val valid = Output(Bool())
      val readys = Input(Vec(NArithFUs, Bool()))
    }
    val fromExu = Flipped(ValidIO(new VExuOutput))
    // LSU
    val toLSU = new Bundle {
      val ld = Decoupled(new VLdInput)
      val st = Decoupled(new VStInput)
    }
    val fromLSU = new Bundle {
      val ld = Flipped(ValidIO(new VLdOutput))
      val st = Flipped(ValidIO(new VStOutput))
    }
    // writeback: to BusyTable, to ROB
    val wbArith = ValidIO(new WbArith)
    val wbLSU = Vec(2, ValidIO(new VExpdUOp))
    // Just for debug
    val rfRdRvfi = Vec(VCommitWidth, new VRFReadPort(LaneWidth))
  })

  val arithIQ = Module(new VArithIssueQ(ArithIQSize, VRenameWidth))
  val lsIQ = Module(new VLdstIssueQ(LsIQSize, VRenameWidth))
  // Arith: 4    Ld: 3 (idx + old_dst + mask)  st: 3 (idx + vs3 + mask)
  // Todo: reduce num of read ports of ld/st
  val nVRFReadPorts = NArithIQs * 4 + 3 // So far load and store are serial, they need 3 read ports
  val regFile = Module(new VRegFile(nVRFReadPorts + nVRFReadPortsRvfi, nVRFWritePorts))

  // Write-back addresses: for wakeup (ready gen) of IQs
  val wbRFAddrs = Wire(Vec(nVRFWritePorts, ValidIO(UInt(VPRegIdxWidth.W))))
  wbRFAddrs(0).valid := io.fromExu.valid && io.fromExu.bits.uop.pdestVal
  wbRFAddrs(0).bits := io.fromExu.bits.uop.pdest
  wbRFAddrs(1).valid := io.fromLSU.ld.valid && io.fromLSU.ld.bits.uop.pdestVal
  wbRFAddrs(1).bits := io.fromLSU.ld.bits.uop.pdest

  /** ---- Arithmetic ----
    *  IQ --> Read RF --> EXU
    */
  val validPipe_arith = RegInit(false.B)
  // Inputs of Arith IQ
  arithIQ.io.flush := io.flush
  arithIQ.io.in <> io.in.toArithIQ
  arithIQ.io.fromBusyTable := io.fromBusyTable
  arithIQ.io.vRFWriteback := wbRFAddrs
  // Ready to issue to EXU
  val toExuReady = (io.toExu.readys zip io.toExu.bits.uop.ctrl.fuSel map {case (r, v) => r && v}
                   ).reduce(_ || _)
  arithIQ.io.out.ready := toExuReady || !validPipe_arith
  // A uop of arith IQ is successfully issued to read RF
  val fireArithIQout = arithIQ.io.out.fire
  // Read RF, and regEnable the read data
  for (srcIdx <- 0 until 4) {
    regFile.io.read(4*0 + srcIdx).addr := arithIQ.io.out.bits.psrc(srcIdx)
    io.toExu.bits.vSrc(srcIdx) := RegEnable(regFile.io.read(4*0 + srcIdx).data, fireArithIQout)
  }
  // RegEnable the uop
  io.toExu.bits.uop := RegEnable(arithIQ.io.out.bits, fireArithIQout)
  // RegEnable the rs1
  io.get_rs1.exu.addr := arithIQ.io.out.bits.vRobIdx
  io.toExu.bits.rs1 := RegEnable(io.get_rs1.exu.data, fireArithIQout)
  // Pipeline of valid
  when (io.flush) {
    validPipe_arith := false.B
  }.elsewhen (fireArithIQout) {
    validPipe_arith := true.B
  }.elsewhen (toExuReady) {
    validPipe_arith := false.B
  }
  io.toExu.valid := validPipe_arith
  // VL remain
  val destEewExu = SewOH(io.toExu.bits.uop.info.destEew)
  io.toExu.bits.vlRemain := calcRemain(destEewExu, fireArithIQout, 
                                  arithIQ.io.out.bits, arithIQ.io.out.bits.info.vl)
  // vstart remain
  // io.toExu.bits.vstartRemain := calcRemain(io.toExu.bits.uop.info.destEew, fireArithIQout, 
  //                                 arithIQ.io.out.bits.expdIdx, arithIQ.io.out.bits.info.vstart)
  io.toExu.bits.vstartRemain := 0.U
  
  // RF write
  regFile.io.write(0).wen := io.fromExu.valid && io.fromExu.bits.uop.pdestVal //&& io.fromExu.bits.uop.info.wenRF
  regFile.io.write(0).addr := io.fromExu.bits.uop.pdest
  regFile.io.write(0).data := io.fromExu.bits.vd
  // Write-back: to BusyTable, to ROB 
  io.wbArith.valid := io.fromExu.valid
  io.wbArith.bits.uop := io.fromExu.bits.uop
  io.wbArith.bits.fflags := io.fromExu.bits.fflags
  io.wbArith.bits.vxsat := io.fromExu.bits.vxsat
  io.wbArith.bits.rd := io.fromExu.bits.vd(0)(xLen-1, 0)

  /**
    * ---- Load/Store ----
    */
  // Inputs of ld/st IQs
  lsIQ.io.flush := io.flush
  lsIQ.io.in <> io.in.toLsIQ
  lsIQ.io.fromBusyTable := io.fromBusyTable
  lsIQ.io.vRFWriteback := wbRFAddrs
  
  // -- Load --
  // A uop of load/store IQ is successfully issued
  val fireLsIQout = lsIQ.io.out.fire
  // Info for LSU
  // Read RF, and regEnable the read data
  regFile.io.read(NArithIQs * 4).addr := lsIQ.io.out.bits.psrc(1)
  regFile.io.read(NArithIQs * 4 + 1).addr := lsIQ.io.out.bits.psrc(2)
  regFile.io.read(NArithIQs * 4 + 2).addr := lsIQ.io.out.bits.psrc(3)
  io.toLSU.ld.bits.vs2 := RegEnable(Cat(regFile.io.read(NArithIQs * 4).data.reverse), fireLsIQout)
  io.toLSU.ld.bits.oldVd := RegEnable(Cat(regFile.io.read(NArithIQs * 4 + 1).data.reverse), fireLsIQout)
  io.toLSU.ld.bits.vmask := RegEnable(Cat(regFile.io.read(NArithIQs * 4 + 2).data.reverse), fireLsIQout)
  // RegEnable the uop
  io.toLSU.ld.bits.uop := RegEnable(lsIQ.io.out.bits, fireLsIQout)
  // RegEnable the rs1
  io.get_rs1.ls.addr := lsIQ.io.out.bits.vRobIdx
  io.toLSU.ld.bits.rs2 := RegEnable(io.get_rs1.ls.data, fireLsIQout)
  // So far, the ready of io.toLSU is ld.ready && st.ready
  // Todo: support overlapping of load and store excution
  val io_toLSU_ready = io.toLSU.ld.ready && io.toLSU.st.ready
  // Pipeline of valid
  val validPipe_ls = RegInit(false.B)
  when (io.flush) {
    validPipe_ls := false.B
  }.elsewhen (fireLsIQout) {
    validPipe_ls := true.B
  }.elsewhen (io_toLSU_ready) {
    validPipe_ls := false.B
  }
  io.toLSU.ld.valid := validPipe_ls && io.toLSU.ld.bits.uop.ctrl.load
  // Ready
  lsIQ.io.out.ready := io_toLSU_ready || !validPipe_ls
  // RF write of Ld
  regFile.io.write(NArithIQs).wen := io.fromLSU.ld.valid
  regFile.io.write(NArithIQs).addr := io.fromLSU.ld.bits.uop.pdest
  for (i <- 0 until NLanes) {regFile.io.write(NArithIQs).data(i) := UIntSplit(io.fromLSU.ld.bits.vd)(i)}
  // Write-back: to BusyTable, to ROB 
  io.wbLSU(0).valid := io.fromLSU.ld.valid
  io.wbLSU(0).bits := io.fromLSU.ld.bits.uop

  // -- Store --
  // Read RF, and regEnable the read data
  io.toLSU.st.bits.vs2 := io.toLSU.ld.bits.vs2 
  io.toLSU.st.bits.vs3 := io.toLSU.ld.bits.oldVd 
  io.toLSU.st.bits.vmask := io.toLSU.ld.bits.vmask 
  // RegEnable the uop
  io.toLSU.st.bits.uop := io.toLSU.ld.bits.uop
  // RegEnable the rs1
  io.toLSU.st.bits.rs2 := io.toLSU.ld.bits.rs2

  io.toLSU.st.valid := validPipe_ls && io.toLSU.ld.bits.uop.ctrl.store
  // Write-back: to BusyTable, to ROB 
  io.wbLSU(1).valid := io.fromLSU.st.valid
  io.wbLSU(1).bits := io.fromLSU.st.bits.uop


  def calcRemain(destEew: SewOH, fireIn: Bool, uop: VExpdUOp, vl: UInt) = {
    // Notice: max VL must <= 10000...0   max vstart <= 01111...1
    val remain = Reg(UInt(bVL.W)) 
    val nElemVLEN = Wire(UInt(bVL.W))
    // nElemVLEN := vlenb.U >> destEew
    nElemVLEN := Mux1H(Seq(
      destEew.is8  -> vlenb.U,
      destEew.is16 -> (vlenb.U >> 1),
      destEew.is32 -> (vlenb.U >> 2),
      destEew.is64 -> (vlenb.U >> 3),
    ))
    val sub = Wire(UInt(bVL.W))
    sub := remain - Mux(uop.ctrl.narrow && uop.expdIdx(0), 0.U, nElemVLEN)
    when (fireIn) {
      remain := Mux(uop.expdIdx === 0.U, vl, Mux(sub(bVL-1), 0.U, sub))
    }
    remain
  }

  /**
    * Debug print
    */
  // when (arithIQ.io.out.fire) {
  //   printf(p"----Get one arith IQ out \n") 
  // }
  // when (lsIQ(0).io.out.fire) {
  //   printf(p"----Get one load IQ out \n") 
  // }
  // when (lsIQ(1).io.out.fire) {
  //   printf(p"----Get one store IQ out \n") 
  // }

  /**
    * Debug
    */
  io.rfRdRvfi(0) <> regFile.io.read(nVRFReadPorts)
  io.rfRdRvfi(1) <> regFile.io.read(nVRFReadPorts + 1)

}
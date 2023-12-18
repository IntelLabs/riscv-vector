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

class VDecodeOut extends Bundle {
  val sb_id = Output(UInt(5.W))
  val scalar_opnd = Output(UInt(64.W))
  val ctrl = Output(new VCtrl)
  val csr = Output(new VCsr)
}

class VDecodeUnit extends Module {
  val io = IO(new Bundle {
    val in = new OVIissue
    // credit from ROB
    val issueCredit = Input(Bool())
    val out = ValidIO(new VDecodeOut)
  })
  
  // Pass througth
  io.in.credit := io.issueCredit

  val decode = Module(new VDecode)
  decode.io.in := io.in.inst

  io.out.valid := RegNext(io.in.valid)
  io.out.bits.scalar_opnd := RegEnable(io.in.scalar_opnd, io.in.valid)
  io.out.bits.ctrl := RegEnable(decode.io.out, io.in.valid)
  io.out.bits.sb_id := RegEnable(io.in.sb_id, io.in.valid)
  io.out.bits.csr.vstart := RegEnable(io.in.v_csr(13, 0), io.in.valid)
  io.out.bits.csr.vl := RegEnable(io.in.v_csr(28, 14), io.in.valid)
  io.out.bits.csr.vxrm := RegEnable(io.in.v_csr(30, 29), io.in.valid)
  io.out.bits.csr.frm := RegEnable(io.in.v_csr(33, 31), io.in.valid)
  io.out.bits.csr.vlmul := RegEnable(io.in.v_csr(36, 34), io.in.valid)
  io.out.bits.csr.vsew := RegEnable(io.in.v_csr(39, 37), io.in.valid)
  io.out.bits.csr.vill := RegEnable(io.in.v_csr(40), io.in.valid)
  io.out.bits.csr.ma := RegEnable(io.in.v_csr(41), io.in.valid)
  io.out.bits.csr.ta := RegEnable(io.in.v_csr(42), io.in.valid)
  // io.out.bits.vInfo.destEew := 0.U  // don't care
  // io.out.bits.vInfo.emulVd := 0.U  // don't care
}
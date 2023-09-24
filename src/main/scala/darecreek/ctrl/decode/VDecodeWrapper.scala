package darecreek

import chisel3._
import chisel3.util._

class VDecodeOut extends Bundle {
  val sb_id = Output(UInt(5.W))
  val scalar_opnd = Output(UInt(64.W))
  val vCtrl = Output(new VCtrl)
  val vInfo = Output(new VInfo)
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
  io.out.bits.vCtrl := RegEnable(decode.io.out, io.in.valid)
  io.out.bits.sb_id := RegEnable(io.in.sb_id, io.in.valid)
  io.out.bits.vInfo.vstart := RegEnable(io.in.v_csr(13, 0), io.in.valid)
  io.out.bits.vInfo.vl := RegEnable(io.in.v_csr(28, 14), io.in.valid)
  io.out.bits.vInfo.vxrm := RegEnable(io.in.v_csr(30, 29), io.in.valid)
  io.out.bits.vInfo.frm := RegEnable(io.in.v_csr(33, 31), io.in.valid)
  io.out.bits.vInfo.vlmul := RegEnable(io.in.v_csr(36, 34), io.in.valid)
  io.out.bits.vInfo.vsew := RegEnable(io.in.v_csr(39, 37), io.in.valid)
  io.out.bits.vInfo.vill := RegEnable(io.in.v_csr(40), io.in.valid)
  io.out.bits.vInfo.ma := RegEnable(io.in.v_csr(41), io.in.valid)
  io.out.bits.vInfo.ta := RegEnable(io.in.v_csr(42), io.in.valid)
  io.out.bits.vInfo.destEew := 0.U  // don't care
  io.out.bits.vInfo.emulVd := 0.U  // don't care
}
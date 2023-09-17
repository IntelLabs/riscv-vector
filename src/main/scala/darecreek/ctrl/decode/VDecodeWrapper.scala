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
  io.out.bits.scalar_opnd := RegNext(io.in.scalar_opnd)
  io.out.bits.vCtrl := RegNext(decode.io.out)
  io.out.bits.sb_id := RegNext(io.in.sb_id)
  io.out.bits.vInfo.vstart := RegNext(io.in.v_csr(13, 0))
  io.out.bits.vInfo.vl := RegNext(io.in.v_csr(28, 14))
  io.out.bits.vInfo.vxrm := RegNext(io.in.v_csr(30, 29))
  io.out.bits.vInfo.frm := RegNext(io.in.v_csr(33, 31))
  io.out.bits.vInfo.vlmul := RegNext(io.in.v_csr(36, 34))
  io.out.bits.vInfo.vsew := RegNext(io.in.v_csr(39, 37))
  io.out.bits.vInfo.vill := RegNext(io.in.v_csr(40))
  io.out.bits.vInfo.ma := RegNext(io.in.v_csr(41))
  io.out.bits.vInfo.ta := RegNext(io.in.v_csr(42))
  io.out.bits.vInfo.destEew := 0.U  // don't care
  io.out.bits.vInfo.emulVd := 0.U  // don't care
}
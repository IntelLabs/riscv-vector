package darecreek

import chisel3._
import chisel3.util._

class VDecodeUnit extends Module {
  val io = IO(new Bundle {
    val in = new OVIissue
    // credit from ROB
    val issueCredit = Input(Bool())
    val out = new Bundle {
      val vCtrl = Output(new VCtrl)
      val scalar_opnd = Output(UInt(64.W))
      val sb_id = Output(UInt(5.W))
      val vInfo = Output(new VInfo)
      val valid = Output(Bool())
    }

  })
  
  // Pass througth
  io.in.credit := io.issueCredit

  val decode = Module(new VDecode)
  decode.io.in := io.in.inst

  io.out.vCtrl := RegNext(decode.io.out)
  io.out.scalar_opnd := RegNext(io.in.scalar_opnd)
  io.out.sb_id := RegNext(io.in.sb_id)
  io.out.valid := RegNext(io.in.valid)
  io.out.vInfo.vstart := RegNext(io.in.v_csr(13, 0))
  io.out.vInfo.vl := RegNext(io.in.v_csr(28, 14))
  io.out.vInfo.vxrm := RegNext(io.in.v_csr(30, 29))
  io.out.vInfo.frm := RegNext(io.in.v_csr(33, 31))
  io.out.vInfo.vlmul := RegNext(io.in.v_csr(36, 34))
  io.out.vInfo.vsew := RegNext(io.in.v_csr(39, 37))
  io.out.vInfo.vill := RegNext(io.in.v_csr(40))
  io.out.vInfo.ma := RegNext(io.in.v_csr(41))
  io.out.vInfo.ta := RegNext(io.in.v_csr(42))
  io.out.vInfo.destEew := 0.U  // don't care
  io.out.vInfo.emulVd := 0.U  // don't care
}
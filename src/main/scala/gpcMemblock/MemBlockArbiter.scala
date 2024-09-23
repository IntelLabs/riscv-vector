// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package gpc.core

import chisel3._
import chisel3.util.{Cat, log2Up}
import org.chipsalliance.cde.config.Parameters

class MemBlockArbiter(n: Int)(
    implicit p: Parameters
) extends Module {
  val io = IO(new Bundle {
    val requestor = Flipped(Vec(n, new MemBlockIO))
    val mem       = new MemBlockIO
  })

  if (n == 1) {
    io.mem <> io.requestor.head
  } else {
    val s1_id = Reg(UInt())
    val s2_id = RegNext(s1_id)

    io.mem.req.valid          := io.requestor.map(_.req.valid).reduce(_ || _)
    io.requestor(0).req.ready := io.mem.req.ready
    for (i <- 1 until n)
      io.requestor(i).req.ready := io.requestor(i - 1).req.ready && !io.requestor(i - 1).req.valid

    for (i <- n - 1 to 0 by -1) {
      val req = io.requestor(i).req
      def connect_s0() = {
        io.mem.req.bits := req.bits
        s1_id           := i.U
      }
      def connect_s1() = {
        io.mem.s1_kill  := io.requestor(i).s1_kill
        io.mem.s1_wdata := io.requestor(i).s1_wdata
        io.mem.s1_wmask := io.requestor(i).s1_wmask
      }
      def connect_s2() =
        io.mem.s2_kill := io.requestor(i).s2_kill

      if (i == n - 1) {
        connect_s0()
        connect_s1()
        connect_s2()
      } else {
        when(req.valid)(connect_s0())
        when(s1_id === i.U)(connect_s1())
        when(s2_id === i.U)(connect_s2())
      }
    }

    for (i <- 0 until n) {
      val resp    = io.requestor(i).resp
      val tag_hit = io.mem.resp.bits.source === i.U
      resp.valid                  := io.mem.resp.valid && tag_hit
      io.requestor(i).s2_xcpt     := io.mem.s2_xcpt
      io.requestor(i).fenceRdy    := io.mem.fenceRdy
      io.requestor(i).nextCycleWb := io.mem.nextCycleWb

      resp.bits := io.mem.resp.bits

    }
  }
}

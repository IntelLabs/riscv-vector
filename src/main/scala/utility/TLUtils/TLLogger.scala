/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utility

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleMap

trait HasCLikeTypes {
  // c++ firendly data types
  def uint8_t = UInt(8.W)
  def uint32_t = UInt(32.W)
  def uint64_t = UInt(64.W)
}

class TLLog extends Bundle with HasCLikeTypes {
  // a b c d e
  // 0 1 2 3 4
  val channel = uint8_t
  val opcode = uint8_t
  val param = uint8_t
  val source = uint8_t
  val sink = uint8_t
  val address = uint64_t
  val data = Vec(4, uint64_t)
  val user = uint64_t
  val echo = uint64_t
}

class TLLogWriter(prefix: String) extends Module {
  val io = IO(Flipped(ValidIO(new TLLog)))
  TLLogger.table.log(io, prefix, this.clock, this.reset)
}

class TLLogger(name: String)(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode()
  lazy val module = new TLLoggerImp(this, name)
}

class TLLoggerImp(outer: TLLogger, name: String) extends LazyModuleImp(outer) {
  val node = outer.node
  for (((in, edgeIn), (out, edgeOut)) <- node.in.zip(node.out)) {
    out <> in
    TLLogger.track(in, edgeIn, this.clock, this.reset)(name)
  }
}

object TLLogger {

  def a = 0.U
  def b = 1.U
  def c = 2.U
  def d = 3.U
  def e = 4.U // not used

  val table = ChiselDB.createTable("TLLog", new TLLog, basicDB = true)

  def writeChannel[T <: TLChannel](log: TLLog, chn: T): Unit = {
    for ((name, data) <- log.elements.filterNot(_._1 == "data")) {
      val e = chn.elements.find(_._1 == name)
      if (e.nonEmpty) {
        data := e.get._2.asUInt
      } else {
        data := 0.U
      }
    }
    def bmp_to_uint(bmp: BundleMap): UInt = {
      if(bmp.fields.nonEmpty){
        bmp.asUInt
      } else {
        0.U
      }
    }
    chn match {
      case a_chn: TLBundleA =>
        log.channel := a
        log.user := bmp_to_uint(a_chn.user)
        log.echo := bmp_to_uint(a_chn.echo)
      case _: TLBundleB =>
        log.channel := b
        log.user := 0.U
        log.echo := 0.U
      case c_chn: TLBundleC =>
        log.channel := c
        log.user := bmp_to_uint(c_chn.user)
        log.echo := bmp_to_uint(c_chn.echo)
      case d_chn: TLBundleD =>
        log.channel := d
        log.user := bmp_to_uint(d_chn.user)
        log.echo := bmp_to_uint(d_chn.echo)
    }
  }

  def logA(log: TLLog, a: TLBundleA) = {
    writeChannel(log, a)
  }

  def logB(log: TLLog, b: TLBundleB) = {
    writeChannel(log, b)
  }

  def logC(log: TLLog, c: TLBundleC) = {
    writeChannel(log, c)
    log.data := c.data.asTypeOf(log.data)
  }

  def logD(log: TLLog, d: TLBundleD, addr: UInt) = {
    writeChannel(log, d)
    log.address := addr
    log.data := d.data.asTypeOf(log.data)
  }

  def track(in: TLBundle, edge: TLEdgeIn, clock: Clock, reset: Reset)(name: String) = {
    val numClients = edge.client.endSourceId

    // Acquire/Get -> Grant
    val a_d_addrs = Reg(Vec(numClients, UInt(edge.bundle.addressBits.W)))
    // Release -> ReleaseAck
    val c_d_addrs = Reg(Vec(numClients, UInt(edge.bundle.addressBits.W)))
    val a_log, b_log, c_log, d_log = WireInit(0.U.asTypeOf(new TLLog))
    val a_writer, b_writer, c_writer, d_writer = Module(new TLLogWriter(name))

    def connect(writer: TLLogWriter, log: TLLog, wen: Bool) = {
      writer.io.bits.channel := log.channel
      writer.io.bits.opcode := log.opcode
      writer.io.bits.param := log.param
      writer.io.bits.source := log.source
      writer.io.bits.sink := log.sink
      writer.io.bits.address := log.address
      writer.io.bits.data := log.data
      writer.io.bits.user := log.user
      writer.io.bits.echo := log.echo
      writer.io.valid := wen
    }

    connect(a_writer, a_log, in.a.fire)
    connect(b_writer, b_log, in.b.fire)
    connect(c_writer, c_log, in.c.fire)
    connect(d_writer, d_log, in.d.fire)

    when(in.a.fire) {
      logA(a_log, in.a.bits)
      a_d_addrs(in.a.bits.source) := in.a.bits.address
    }

    when(in.b.fire) {
      logB(b_log, in.b.bits)
    }

    when(in.c.fire) {
      logC(c_log, in.c.bits)
      c_d_addrs(in.c.bits.source) := in.c.bits.address
    }

    when(in.d.fire) {
      val a_d = a_d_addrs(in.d.bits.source)
      val c_d = c_d_addrs(in.d.bits.source)
      val addr = Mux(in.d.bits.opcode === TLMessages.ReleaseAck, c_d, a_d)
      logD(d_log, in.d.bits, addr)
    }

  }

  def apply(name: String, enable: Boolean = true)(implicit p: Parameters) = {
    if (enable) {
      val logger = LazyModule(new TLLogger(name))
      logger.node
    } else {
      TLTempNode()
    }
  }

}

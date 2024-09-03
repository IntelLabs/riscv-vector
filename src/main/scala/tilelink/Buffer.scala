// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

class TLBufferNode (
  a: BufferParams,
  b: BufferParams,
  c: BufferParams,
  d: BufferParams,
  e: BufferParams)(implicit valName: ValName) extends TLAdapterNode(
    clientFn  = { p => p.v1copy(minLatency = p.minLatency + b.latency + c.latency) },
    managerFn = { p => p.v1copy(minLatency = p.minLatency + a.latency + d.latency) }
) {
  override lazy val nodedebugstring = s"a:${a.toString}, b:${b.toString}, c:${c.toString}, d:${d.toString}, e:${e.toString}"
  override def circuitIdentity = List(a,b,c,d,e).forall(_ == BufferParams.none)
}

class TLBuffer(
  a: BufferParams,
  b: BufferParams,
  c: BufferParams,
  d: BufferParams,
  e: BufferParams)(implicit p: Parameters) extends LazyModule
{
  def this(ace: BufferParams, bd: BufferParams)(implicit p: Parameters) = this(ace, bd, ace, bd, ace)
  def this(abcde: BufferParams)(implicit p: Parameters) = this(abcde, abcde)
  def this()(implicit p: Parameters) = this(BufferParams.default)

  val node = new TLBufferNode(a, b, c, d, e)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a <> a(in .a)
      in .d <> d(out.d)

      if (edgeOut.manager.anySupportAcquireB && edgeOut.client.anySupportProbe) {
        in .b <> b(out.b)
        out.c <> c(in .c)
        out.e <> e(in .e)
      } else {
        in.b.valid := false.B
        in.c.ready := true.B
        in.e.ready := true.B
        out.b.ready := true.B
        out.c.valid := false.B
        out.e.valid := false.B
      }
    }
  }
}

object TLBuffer
{
  def apply()                                   (implicit p: Parameters): TLNode = apply(BufferParams.default)
  def apply(abcde: BufferParams)                (implicit p: Parameters): TLNode = apply(abcde, abcde)
  def apply(ace: BufferParams, bd: BufferParams)(implicit p: Parameters): TLNode = apply(ace, bd, ace, bd, ace)
  def apply(
      a: BufferParams,
      b: BufferParams,
      c: BufferParams,
      d: BufferParams,
      e: BufferParams)(implicit p: Parameters): TLNode =
  {
    val buffer = LazyModule(new TLBuffer(a, b, c, d, e))
    buffer.node
  }

  def chain(depth: Int, name: Option[String] = None)(implicit p: Parameters): Seq[TLNode] = {
    val buffers = Seq.fill(depth) { LazyModule(new TLBuffer()) }
    name.foreach { n => buffers.zipWithIndex.foreach { case (b, i) => b.suggestName(s"${n}_${i}") } }
    buffers.map(_.node)
  }

  def chainNode(depth: Int, name: Option[String] = None)(implicit p: Parameters): TLNode = {
    chain(depth, name)
      .reduceLeftOption(_ :*=* _)
      .getOrElse(TLNameNode("no_buffer"))
  }
}

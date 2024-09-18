package utility


import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleField


class XbarCircuit
(
  policy: TLArbiter.Policy,
  edgeIn: Seq[TLEdge],
  edgeOut: Seq[TLEdge]
) extends Module {

  val io = IO(new Bundle{
    val in = MixedVec(edgeIn.map(e => Flipped(TLBundle(e.bundle))))
    val out = MixedVec(edgeOut.map(e => TLBundle(e.bundle)))
  })

  val inSeq = io.in.zip(edgeIn).toSeq
  val outSeq = io.out.zip(edgeOut).toSeq

  TLXbar.circuit(policy, inSeq, outSeq)

}

case class BinaryArbiterNode
(
  clientFn: Seq[TLMasterPortParameters] => TLMasterPortParameters,
  managerFn: Seq[TLSlavePortParameters] => TLSlavePortParameters
)(implicit valName: ValName) extends TLCustomNode {

  override def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require(iStars == 0 && oKnown == 0 && oStars == 1)
    if(iKnown < 4) {
      (0, 1)
    } else {
      require(2 * (iKnown / 2) == iKnown)
      (0, 2)
    }
  }

  override def mapParamsD(n: Int, p: Seq[TLClientPortParameters]): Seq[TLClientPortParameters] = {
    if(n == 1){
      Seq(clientFn(p))
    } else {
      require(n == 2)
      p.grouped(2).toList.transpose.map(grp => clientFn(grp))
    }
  }

  override def mapParamsU(n: Int, p: Seq[TLManagerPortParameters]): Seq[TLManagerPortParameters] = {
    Seq.fill(n)(p.head)
  }
}

class BinaryArbiter(policy: TLArbiter.Policy)(implicit p: Parameters) extends LazyModule {

  val node = BinaryArbiterNode(
    clientFn = { seq =>
      seq.head.v1copy(
        echoFields = BundleField.union(seq.flatMap(_.echoFields)),
        requestFields = BundleField.union(seq.flatMap(_.requestFields)),
        responseKeys = seq.flatMap(_.responseKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        clients = (TLXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.clients map { client =>
            client.v1copy(
              sourceId = client.sourceId.shift(range.start)
            )
          }
        }
      )
    },
    managerFn = { seq =>
      val fifoIdFactory = TLXbar.relabeler()
      seq.head.v1copy(
        responseFields = BundleField.union(seq.flatMap(_.responseFields)),
        requestKeys = seq.flatMap(_.requestKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        managers = seq.flatMap { port =>
          require(port.beatBytes == seq.head.beatBytes,
            s"Xbar ($name with parent $parent) data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
          val fifoIdMapper = fifoIdFactory()
          port.managers map { manager =>
            manager.v1copy(
              fifoId = manager.fifoId.map(fifoIdMapper(_))
            )
          }
        }
      )}
  )

  lazy val module = new LazyModuleImp(this){

    if(node.out.size == 1){
      TLXbar.circuit(policy, node.in, node.out)
    } else {
      require(node.out.size == 2)
      /*
            0, 1, 2, 3 => (0, 2) (1, 3)
       */
      val grps = node.in.grouped(2).toList.transpose
      require(grps.size == 2)
      for((gp, out) <- grps.zip(node.out)){
        val xbar = Module(new XbarCircuit(policy, gp.map(_._2), Seq(out._2)))
        xbar.io.in.zip(gp.map(_._1)).foreach(x => x._1 <> x._2)
        out._1 <> xbar.io.out.head
      }
    }
  }
}

object BinaryArbiter {
  def apply(policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) = {
    val arbiter = LazyModule(new BinaryArbiter(policy))
    arbiter.node
  }
}

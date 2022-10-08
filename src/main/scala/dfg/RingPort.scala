package org.datenlord
package dfg

import org.datenlord.arithmetic.CpaConfig
import spinal.core.UInt

/** Consisting of a vertex and its output/input order, RingPort can be used as a variable in datapath, this class makes the description of RingDag more natural
 *
 * @param order output order of the vertex, the specific output variable is specified by this parameter
 */
case class RingPort(override val vertex: RingVertex, override val order: Int, override val direction: Direction)
  extends DagPort[UInt](vertex, order, direction) {

  def width = vertex.widthsOut(order)

  def checkIsOut(): Unit = assert(direction == Out, "only out ports can be used as drivers")

  def :=(that: RingPort)(implicit dag: RingDag) = {
    require(this.direction == In && that.direction == Out)
    dag.addEdge(that, this)
  }

  /** --------
   * following methods are the interface of RingOps exposed to the programmer/user, they can be used like a dsl
   * -------- */

  private def addSub(that: RingPort, opType: AdderType)(implicit dag: RingDag) =
    CpaConfig(this.width max that.width, opType).asRingOp(Seq(this, that)).head

  def mult(that: RingPort, mode: MultiplierType)(implicit dag: RingDag) = {
    checkIsOut()
    val multVertex = BaseMultVertex(s"*", mode, Seq(this.width, that.width))
    dag.addVertex(multVertex)
    dag.addEdge(this, multVertex.in(0))
    dag.addEdge(that, multVertex.in(1))
    multVertex.out(0)
  }

  // TODO: paper, and position
  def karaWith(b: RingPort, c: RingPort, d: RingPort)(implicit dag: RingDag) = {
    val karaVertex = BaseKaraVertex(s"kara", Seq(this, b, c, d).map(_.width))
    dag.addVertexWithDrivers(karaVertex, this, b, c, d)
    (karaVertex.out(0), karaVertex.out(1), karaVertex.out(2))
  }

  def &(that: RingPort)(implicit dag: RingDag): RingPort = {
    require(this.width == that.width)
    val vertex = AndVertex("AND", width)
    dag.addVertexWithDrivers(vertex, this, that)
    vertex.out(0)
  }

  def muxBy(that: RingPort)(implicit dag: RingDag): RingPort = {
    require(that.width == 1)
    val vertex = MuxVertex("MUX", Seq(this.width, that.width))
    dag.addVertexWithDrivers(vertex, this, that)
    vertex.out(0)
  }

  // TODO: make this low to high
  def split(splitPoints: Seq[Int])(implicit dag: RingDag): Seq[RingPort] = {
    checkIsOut()
    val splitVertex = SplitVertex(s"split_${splitPoints.mkString("_")}", width, splitPoints)
    dag.addVertexWithDrivers(splitVertex, this)
    (0 until splitPoints.length + 1).map(splitVertex.out)
  }

  def splitAt(splitPoint: Int)(implicit dag: RingDag): (RingPort, RingPort) = {
    val seq = split(Seq(splitPoint))
    (seq(0), seq(1))
  }

  def merge(tail: Seq[RingPort])(implicit dag: RingDag): RingPort = {
    checkIsOut()
    val all = this +: tail
    val mergeVertex = MergeVertex(s"merge", all.map(_.width))
    dag.addVertexWithDrivers(mergeVertex, all: _*)
    mergeVertex.out(0)
  }

  def resize(widthOut: Int)(implicit dag: RingDag) = {
    checkIsOut()
    if (this.width == widthOut) this // skip when it is not necessary
    else {
      val padVertex = ResizeVertex(s"resize", this.width, widthOut)
      dag.addVertex(padVertex)
      dag.addEdge(this, padVertex.in(0))
      padVertex.out(0)
    }
  }

  def <<(shift: Int)(implicit dag: RingDag) = {
    val vertex = ShiftVertex(s"SHIFT$shift", width, shift)
    dag.addVertexWithDrivers(vertex, this)
    vertex.out(0)
  }

  /** --------
   * following methods are aliases of the methods above
   * -------- */
  def +^(that: RingPort)(implicit dag: RingDag): RingPort = addSub(that, BinaryAdder)

  def -(that: RingPort)(implicit dag: RingDag): RingPort = addSub(that, BinarySubtractor)

  def @@(that: RingPort)(implicit dag: RingDag): RingPort = merge(Seq(that))

  def *(that: RingPort)(implicit dag: RingDag): RingPort = mult(that, FullMultiplier)

  def *%(that: RingPort)(implicit dag: RingDag): RingPort = mult(that, LsbMultiplier)

  def square(implicit dag: RingDag): RingPort = mult(this, SquareMultiplier)

  def >>(shift: Int)(implicit dag: RingDag) = <<(-shift)

  override def toString = s"${vertex}_$order"
}

object RingPort {
  def fromDagPort(dagPort: DagPort[UInt]) = RingPort(dagPort.vertex.asInstanceOf[RingVertex], dagPort.order, dagPort.direction)
}

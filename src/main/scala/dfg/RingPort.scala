package org.datenlord
package dfg

import spinal.core.UInt
import dfg.Direction._
import dfg.OpType._
import arithmetic.MultplierMode._

import org.datenlord.dfg.ArithmeticGraphs.{addGraph, karatsubaGraph, subGraph}

import scala.collection.immutable

/** Consisting of a vertex and its output order, RingPort can be used as a variable in datapath, this class makes the description of RingDag more natural
 *
 * @param order output order of the vertex, the specific output variable is specified by this parameter
 */
case class RingPort(override val vertex: RingVertex, override val order: Int, override val direction: Direction)
  extends DagPort[BigInt, UInt](vertex, order, direction) {

  def width = vertex.infosOut(order)

  /** Split a vertex into multiple
   * @param splitPoints high to low split points
   * @example
   */
  def split(splitPoints: Seq[Int])(implicit dag: RingDag): Seq[RingPort] = {
    require(this.direction == Out)
    require(splitPoints.nonEmpty && splitPoints.max < this.width && splitPoints.min > 0)
    val splitVertex = SplitVertex(s"${vertex.name}_split", width, splitPoints)
    dag.addVertex(splitVertex)
    dag.addEdge(this, splitVertex.in(0))
    (0 until splitPoints.length + 1).map(splitVertex.out)
  }

  def splitAt(splitPoint: Int)(implicit dag: RingDag) = {
    val seq = split(Seq(splitPoint))
    (seq(0), seq(1))
  }

  def +^(that: RingPort, carry: RingPort = null)(implicit dag: RingDag): (RingPort, RingPort) = {
    require(this.direction == Out & that.direction == Out)
    val widthsIn = if (carry != null) Seq(width, that.width, 1) else Seq(width, that.width)
    val addVertex = AddVertex(s"$this+$that", widthsIn)
    dag.addVertex(addVertex)
    dag.addEdge(this, addVertex.in(0))
    dag.addEdge(that, addVertex.in(1))
    if (carry != null) dag.addEdge(carry, addVertex.in(2))
    (addVertex.out(0), addVertex.out(1))
  }

  def -^(that: RingPort, carry: RingPort = null)(implicit dag: RingDag): (RingPort, RingPort) = {
    require(this.direction == Out & that.direction == Out)
    val widthsIn = if (carry != null) Seq(width, that.width, 1) else Seq(width, that.width)
    val subVertex = SubVertex(s"$this-$that", widthsIn)
    dag.addVertex(subVertex)
    dag.addEdge(this, subVertex.in(0))
    dag.addEdge(that, subVertex.in(1))
    if (carry != null) dag.addEdge(carry, subVertex.in(2))
    (subVertex.out(0), subVertex.out(1))
  }

  def merge(tail: Seq[RingPort])(implicit dag: RingDag) = {
    require(this.direction == Out)
    val all = this +: tail
    require(all.length > 1)
    val mergeVertex = MergeVertex(s"${vertex.name}_merge", all.map(_.width))
    dag.addVertex(mergeVertex)
    all.zipWithIndex.foreach { case (port, i) => dag.addEdge(port, mergeVertex.in(i)) }
    mergeVertex.out(0)
  }

  def @@(that: RingPort)(implicit dag: RingDag) = merge(Seq(that))

  def resize(widthOut: Int)(implicit dag: RingDag) = {
    require(this.direction == Out)
    if (this.width == widthOut) this // skip when it is not necessary
    else {
      val padVertex = ResizeVertex(s"${vertex.name}_merge", this.width, widthOut)
      dag.addVertex(padVertex)
      dag.addEdge(this, padVertex.in(0))
      padVertex.out(0)
    }
  }

  def multByMode(that: RingPort, mode: MultiplierMode)(implicit dag: RingDag) = {
    mode match {
      case Full => this * that
      case Low => this *% that
      case Square => this.square
    }
  }

  def *(that: RingPort)(implicit dag: RingDag): RingPort = {
    require(this.direction == Out & that.direction == Out)
    val multVertex = MultVertex(s"$this*$that", FullMult, Seq(width, that.width))
    dag.addVertex(multVertex)
    dag.addEdge(this, multVertex.in(0))
    dag.addEdge(that, multVertex.in(1))
    multVertex.out(0)
  }

  def *%(that: RingPort)(implicit dag: RingDag): RingPort = {
    require(this.direction == Out & that.direction == Out)
    val multVertex = MultVertex(s"$this*$that", LowMult, Seq(width, that.width))
    dag.addVertex(multVertex)
    dag.addEdge(this, multVertex.in(0))
    dag.addEdge(that, multVertex.in(1))
    multVertex.out(0)
  }

  def square(implicit dag: RingDag): RingPort = {
    require(this.direction == Out)
    val multVertex = MultVertex(s"$this*$this", SquareMult, Seq(width, width))
    dag.addVertex(multVertex)
    dag.addEdge(this, multVertex.in(0))
    dag.addEdge(this, multVertex.in(1))
    multVertex.out(0)
  }

  def +:+^(that: RingPort)(implicit dag: RingDag): RingPort = {
    val widthAdd = this.width max that.width
    val padded = Seq(this, that).map(port => if (port.width < widthAdd) port.resize(widthAdd) else port)
    dag.addGraphsAfter(addGraph(widthAdd), padded)
      .head.asInstanceOf[RingPort]
  }

  def +:+(that: RingPort)(implicit dag: RingDag): RingPort = {
    val widthAdd = this.width max that.width
    (this +:+^ that).resize(widthAdd)
  }

  def -:-^(that: RingPort)(implicit dag: RingDag): RingPort = {
    require(this.width >= that.width)
    val widthSub = this.width max that.width
    val padded = that.resize(widthSub)
    dag.addGraphsAfter(subGraph(widthSub), Seq(this, padded))
      .head.asInstanceOf[RingPort]
  }

  def -:-(that: RingPort)(implicit dag: RingDag) = {
    require(this.width >= that.width)
    val widthSub = this.width max that.width
    (this -:-^ that).resize(widthSub)
  }

  def *:*(that: RingPort)(implicit dag: RingDag): RingPort = {
    require(this.width == that.width)
    dag.addGraphsAfter(karatsubaGraph(this.width, Full), Seq(this, that))
      .head.asInstanceOf[RingPort]
  }

  def *-:*-(that: RingPort)(implicit dag: RingDag): RingPort = {
    require(this.width == that.width)
    dag.addGraphsAfter(karatsubaGraph(this.width, Low), Seq(this, that))
      .head.asInstanceOf[RingPort]
  }

  def bigSquare(implicit dag: RingDag): RingPort = {
    dag.addGraphsAfter(karatsubaGraph(this.width, Square), Seq(this, this))
      .head.asInstanceOf[RingPort]
  }

  def bigSquare(that:RingPort)(implicit dag: RingDag): RingPort = {
    dag.addGraphsAfter(karatsubaGraph(this.width, Square), Seq(this, that))
      .head.asInstanceOf[RingPort]
  }

  override def toString = s"${vertex}_$order"
}

object RingPort {
  def fromDagPort(dagPort: DagPort[BigInt, UInt]) = RingPort(dagPort.vertex.asInstanceOf[RingVertex], dagPort.order, dagPort.direction)
}

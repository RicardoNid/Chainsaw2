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

  def info = vertex.infosOut(order)

  def width = vertex.widthsOut(order)

  def shift = vertex.shiftsOut(order)

  /** Split a vertex into multiple
   *
   * @param splitPoints high to low split points
   * @example
   */
  def split(splitPoints: Seq[Int])(implicit dag: RingDag): Seq[RingPort] = {
    require(this.direction == Out)
    require(splitPoints.nonEmpty && splitPoints.max < this.width && splitPoints.min > 0)
    val splitVertex = SplitVertex(s"${vertex.name}_split", info, splitPoints)
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
    val infosIn = if (carry != null) Seq(info, that.info, carry.info) else Seq(info, that.info)
    val addVertex = Add2Vertex(s"$this+$that", Add2, infosIn)
    dag.addVertex(addVertex)
    dag.addEdge(this, addVertex.in(0))
    dag.addEdge(that, addVertex.in(1))
    if (carry != null) dag.addEdge(carry, addVertex.in(2))
    (addVertex.out(0), addVertex.out(1))
  }

  def -^(that: RingPort, carry: RingPort = null)(implicit dag: RingDag): (RingPort, RingPort) = {
    require(this.direction == Out & that.direction == Out)
    val infosIn = if (carry != null) Seq(info, that.info, carry.info) else Seq(info, that.info)
    val subVertex = Add2Vertex(s"$this+$that", Sub2, infosIn)
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
    val mergeVertex = MergeVertex(s"${vertex.name}_merge", all.map(_.info))
    dag.addVertex(mergeVertex)
    all.zipWithIndex.foreach { case (port, i) => dag.addEdge(port, mergeVertex.in(i)) }
    mergeVertex.out(0)
  }

  def @@(that: RingPort)(implicit dag: RingDag) = merge(Seq(that))

  def resize(widthOut: Int)(implicit dag: RingDag) = {
    require(this.direction == Out)
    if (this.width == widthOut) this // skip when it is not necessary
    else {
      val padVertex = ResizeVertex(s"${vertex.name}_merge", this.info, widthOut)
      dag.addVertex(padVertex)
      dag.addEdge(this, padVertex.in(0))
      padVertex.out(0)
    }
  }

  def mult(a: RingPort, b: RingPort, opType: OpType)(implicit dag: RingDag) = {
    require(a.direction == Out & b.direction == Out)
    val multVertex = MultVertex(s"$a*$b", opType, Seq(a.info, b.info))
    dag.addVertex(multVertex)
    dag.addEdge(a, multVertex.in(0))
    dag.addEdge(b, multVertex.in(1))
    multVertex.out(0)
  }

  def *(that: RingPort)(implicit dag: RingDag): RingPort = mult(this, that, FullMult)

  def *%(that: RingPort)(implicit dag: RingDag): RingPort = mult(this, that, LowMult)

  def square(implicit dag: RingDag): RingPort = mult(this, this, SquareMult)

  def multByMode(that: RingPort, mode: MultiplierMode)(implicit dag: RingDag) = {
    mode match {
      case Full => this * that
      case Low => this *% that
      case Square => this.square
    }
  }

  def +:+^(that: RingPort)(implicit dag: RingDag): RingPort = {
//    require(this.shift == that.shift)
    val widthAdd = this.width max that.width
    val padded = Seq(this, that).map(port => if (port.width < widthAdd) port.resize(widthAdd) else port)
    dag.addGraphsAfter(addGraph(widthAdd, this.shift), padded)
      .head.asInstanceOf[RingPort]
  }

  def +:+(that: RingPort)(implicit dag: RingDag): RingPort = {
    val widthAdd = this.width max that.width
    (this +:+^ that).resize(widthAdd)
  }

  def -:-^(that: RingPort)(implicit dag: RingDag): RingPort = {
    require(this.width >= that.width)
//    require(this.shift == that.shift)
    val widthSub = this.width max that.width
    val padded = that.resize(widthSub)
    dag.addGraphsAfter(subGraph(widthSub, this.shift), Seq(this, padded))
      .head.asInstanceOf[RingPort]
  }

  def -:-(that: RingPort)(implicit dag: RingDag) = {
    require(this.width >= that.width)
    val widthSub = this.width max that.width
    (this -:-^ that).resize(widthSub)
  }

  def bigMult(a: RingPort, b: RingPort, mode: MultiplierMode)(implicit dag: RingDag) = {
    require(a.width == b.width)
    require(a.shift == b.shift)
    dag.addGraphsAfter(karatsubaGraph(a.width, a.shift, mode), Seq(a, b))
      .head.asInstanceOf[RingPort]
  }

  def *:*(that: RingPort)(implicit dag: RingDag) = bigMult(this, that, Full)

  def *%:*%(that: RingPort)(implicit dag: RingDag) = bigMult(this, that, Low)

  def bigSquare(implicit dag: RingDag) = bigMult(this, this, Square)

  def bigSquare(that: RingPort)(implicit dag: RingDag) = bigMult(this, that, Square)

  override def toString = s"${vertex}_$order"
}

object RingPort {
  def fromDagPort(dagPort: DagPort[BigInt, UInt]) = RingPort(dagPort.vertex.asInstanceOf[RingVertex], dagPort.order, dagPort.direction)
}

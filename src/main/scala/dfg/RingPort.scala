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

  // TODO: remove this property
  def shift = vertex.shiftsOut(order)

  /** Split a vertex into multiple
   *
   * @param splitPoints high to low split points
   * @example
   */
  def split(splitPoints: Seq[Int])(implicit dag: RingDag): Seq[RingPort] = {
    require(splitPoints.nonEmpty
      && splitPoints.max < this.width
      && splitPoints.min > 0
      && splitPoints.sorted.equals(splitPoints.reverse),
      s"width: ${this.width}, splits: ${splitPoints.mkString(" ")}, split points should be high to low")
    val splitVertex = SplitVertex(s"split_${splitPoints.mkString(" ")}", info, splitPoints)
    dag.addVertexWithDrivers(splitVertex, this)
    (0 until splitPoints.length + 1).map(splitVertex.out)
  }

  def splitAt(splitPoint: Int)(implicit dag: RingDag) = {
    val seq = split(Seq(splitPoint))
    (seq(0), seq(1))
  }

  def addSubBase(that: RingPort, carry: RingPort = null, opType: OpType)(implicit dag: RingDag): (RingPort, RingPort) = {
    val infosIn = if (carry != null) Seq(info, that.info, carry.info) else Seq(info, that.info)
    val name = if (opType == BASEADD) "+<" else "-<"
    val vertex = BaseAddSubVertex(name, opType, infosIn)
    dag.addVertexWithDrivers(vertex, this, that)
    if (carry != null) dag.addEdge(carry, vertex.in(2))
    (vertex.out(0), vertex.out(1))
  }

  def +<(that: RingPort, carry: RingPort = null)(implicit dag: RingDag): (RingPort, RingPort) =
    addSubBase(that, carry, BASEADD)

  def -<(that: RingPort, carry: RingPort = null)(implicit dag: RingDag): (RingPort, RingPort) =
    addSubBase(that, carry, BASESUB)

  def addSub(that: RingPort, opType: OpType)(implicit dag: RingDag) = {
    val name = opType match {
      case ADD => "++"
      case SUB => "--"
      case ADDC => "++^"
      case SUBC => "--^"
    }
    val vertex = AddSubVertex(name, opType, Seq(this.info, that.info))
    dag.addVertexWithDrivers(vertex, this, that)
    vertex.out(0)
  }

  def +(that: RingPort)(implicit dag: RingDag) = addSub(that, ADD)

  def -(that: RingPort)(implicit dag: RingDag) = addSub(that, SUB)

  def +^(that: RingPort)(implicit dag: RingDag) = addSub(that, ADDC)

  def -^(that: RingPort)(implicit dag: RingDag) = addSub(that, SUBC)

  def merge(tail: Seq[RingPort])(implicit dag: RingDag) = {
    val all = this +: tail
    val mergeVertex = MergeVertex(s"merge", all.map(_.info))
    dag.addVertexWithDrivers(mergeVertex, all: _*)
    mergeVertex.out(0)
  }

  def @@(that: RingPort)(implicit dag: RingDag) = merge(Seq(that))

  def resize(widthOut: Int)(implicit dag: RingDag) = {
    require(this.direction == Out)
    if (this.width == widthOut) this // skip when it is not necessary
    else {
      val padVertex = ResizeVertex(s"resize", this.info, widthOut)
      dag.addVertex(padVertex)
      dag.addEdge(this, padVertex.in(0))
      padVertex.out(0)
    }
  }

  def mult(a: RingPort, b: RingPort, opType: OpType)(implicit dag: RingDag) = {
    require(a.direction == Out & b.direction == Out)
    val multVertex = MultVertex(s"*", opType, Seq(a.info, b.info))
    dag.addVertex(multVertex)
    dag.addEdge(a, multVertex.in(0))
    dag.addEdge(b, multVertex.in(1))
    multVertex.out(0)
  }

  def karaWith(b: RingPort, c: RingPort, d: RingPort)(implicit dag: RingDag) = {
    val karaVertex = KaraVertex(s"kara", Seq(this, b, c, d).map(_.info))
    dag.addVertexWithDrivers(karaVertex, this, b, c, d)
    (karaVertex.out(0), karaVertex.out(1), karaVertex.out(2))
  }

  def *(that: RingPort)(implicit dag: RingDag): RingPort = mult(this, that, FullMult)

  def *%(that: RingPort)(implicit dag: RingDag): RingPort = mult(this, that, LowMult)

  def square(implicit dag: RingDag): RingPort = mult(this, this, SquareMult)

  def multByMode(that: RingPort, mode: MultiplierMode)(implicit dag: RingDag) = {
    mode match {
      case FULL => this * that
      case HALFLOW => this *% that
      case SQUARE => this.square
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
    val inputs = if (mode == SQUARE) Seq(a) else Seq(a, b)
    dag.addGraphsAfter(karatsubaGraph(a.width, a.shift, mode), inputs)
      .head.asInstanceOf[RingPort]
  }

  def *:*(that: RingPort)(implicit dag: RingDag) = bigMult(this, that, FULL)

  def *%:*%(that: RingPort)(implicit dag: RingDag) = bigMult(this, that, HALFLOW)

  def bigSquare(implicit dag: RingDag) = bigMult(this, this, SQUARE)

  def bigSquare(that: RingPort)(implicit dag: RingDag) = bigMult(this, that, SQUARE)

  def &(that: RingPort)(implicit dag: RingDag): RingPort = {
    require(this.width == that.width)
    val vertex = AndVertex("AND", info)
    dag.addVertexWithDrivers(vertex, this, that)
    vertex.out(0)
  }

  def muxBy(that: RingPort)(implicit dag: RingDag): RingPort = {
    require(that.width == 1)
    val vertex = MuxVertex("MUX", Seq(this.info, that.info))
    dag.addVertexWithDrivers(vertex, this, that)
    vertex.out(0)
  }

  def <<(shift: Int)(implicit dag: RingDag) = {
    val vertex = ShiftVertex(s"SHIFT$shift", info, shift)
    dag.addVertexWithDrivers(vertex, this)
    vertex.out(0)
  }

  def >>(shift: Int)(implicit dag: RingDag) = <<(-shift)

  override def toString = s"${vertex}_$order"
}

object RingPort {
  def fromDagPort(dagPort: DagPort[BigInt, UInt]) = RingPort(dagPort.vertex.asInstanceOf[RingVertex], dagPort.order, dagPort.direction)
}

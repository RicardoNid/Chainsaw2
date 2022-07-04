package org.datenlord
package dfg

import arithmetic.MultplierMode._

import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._

import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._

case class RingInt(value: BigInt, width: Int) {
  require(value.bitLength <= width)

  def multLowBits(that: RingInt) = {
    require(width == that.width)
    val newValue = (value * that.value) % (BigInt(1) << width)
    RingInt(newValue, width)
  }

  def *(that: RingInt) = {
    val newValue = value * that.value
    RingInt(newValue, width + that.width)
  }

  def +^(that: RingInt) = RingInt(value + that.value, width max that.width + 1)

  def +(that: RingInt) = {
    val widthMax = width max that.width
    RingInt((value + that.value) % (BigInt(1) << widthMax), widthMax)
  }

  def square = this * this

  def split(splitPoints: Seq[Int]) = {
    val widthsOut = (width +: splitPoints).zip(splitPoints :+ 0).map { case (width, low) => width - low }
    val segments = value.split(splitPoints)
    segments.zip(widthsOut).map { case (value, width) => RingInt(value, width) }
  }

}

object RingInt {
  def apply(constant: BigInt): RingInt = RingInt(constant, constant.bitLength)
}

import RingOpType._

/** This is for crypto implementation on FPGAs of Ultrascale family, each child class of RingVertex has a corresponding hardware implementation on Ultrascale device
 *
 * @param opType     type of operation
 * @param widthCheck check whether the input widths are valid, according to opType
 */
class RingVertex
(
  name: String, latency: Int,
  implS: Seq[RingInt] => Seq[RingInt], implH: Seq[UInt] => Seq[UInt],
  val opType: OperatorType,
  val widthsIn: Seq[Int], val widthsOut: Seq[Int], val widthCheck: Seq[Int] => Boolean
) extends ImplVertex[RingInt, UInt](name, latency, implS, implH) {

  def doWidthCheck = assert(widthCheck(widthsIn), s"$opType vertex: widthsIn = ${widthsIn.mkString(" ")}, widthsOut = ${widthsOut.mkString(" ")}")

  override def toString = s"$name"
}


class RingDag extends ImplDag[RingInt, UInt] {

  def setInput(name: String, width: Int) = {
    val in = RingVarVertex(name, width)
    addVertex(in)
    inputs += in
    RingPort(in, 0)
  }

  def setOutput(name: String, width: Int) = {
    val out = RingVarVertex(name, width)
    addVertex(out)
    outputs += out
    RingPort(out, 0)
  }

  // eliminate intermediate variables
  def eliminateIntermediates(): Unit = {
    val inters = vertexSet().toSeq.asInstanceOf[Seq[RingVertex]]
      .filter(_.opType == Var).filterNot(isIo)
    inters.foreach{ inter =>
      val source = sourcePortsOf(inter).head
      val target = targetPortsOf(inter).head
      val weight = getEdgeWeight(incomingEdgesOf(inter).head) + getEdgeWeight(outgoingEdgesOf(inter).head)
      addEdgeWithWeight(source, target, weight)
    }
    inters.foreach(removeVertex)
  }

  def breakBundles() = {
    eliminateIntermediates()
    val candidates = vertexSet().toSeq.asInstanceOf[Seq[RingVertex]]
      .filter(_.opType == Merge)
      .filter(merge => targetsOf(merge).asInstanceOf[Seq[RingVertex]].length == 1 && targetsOf(merge).asInstanceOf[Seq[RingVertex]].head.opType == Split)
    val pairs = candidates.map(merge => (merge, targetsOf(merge).head.asInstanceOf[RingVertex]))


  }

  def checkWidths(): Unit = vertexSet().toSeq.asInstanceOf[Seq[RingVertex]].foreach(_.doWidthCheck)
}

object RingDag {

  def apply(): RingDag = new RingDag()

}


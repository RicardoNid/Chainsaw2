package org.datenlord
package ring

import org.jgrapht.graph._
import spinal.core._

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

  def +(that: RingInt) = RingInt(value + that.value, width max that.width + 1)

  def square = this * this

  def slice(range: Range.Inclusive) = RingInt(value.slice(range), range.length)

  def shl(shift: Int) = RingInt(value << shift, width + shift)

}

object RingInt {
  def apply(constant: BigInt): RingInt = RingInt(constant, constant.bitLength)
}

abstract class RingNode {
  val name: String
  val latency: Int
  val implS: Seq[RingInt] => RingInt
  val implH: Seq[UInt] => UInt
  val widthsIn: Seq[Int] // widths in and width out
  val widthOut: Int

  def widthCheck: Boolean
}

class SignalNode(width: Int, val name: String) extends RingNode {
  override val latency = 0
  override val implS: Seq[RingInt] => RingInt = data => data.head
  override val implH: Seq[UInt] => UInt = data => data.head
  override val widthsIn = Seq(width)
  override val widthOut = width

  override def widthCheck = true
}

case class InputNode(width: Int, override val name: String) extends SignalNode(width, name)

case class OutputNode(width: Int, override val name: String) extends SignalNode(width, name)


object OperatorType extends Enumeration {
  val FullMult, LowMult, SquareMult, ConstantMult, Add, Shl, Slice = Value
  type OperatorType = Value
}

import OperatorType._

import device.MultiplicationByDspConfig
import arithmetic.MultplierMode._

class OperatorNode
(
  opType: OperatorType, widths: Seq[Int], val name: String,
  constant: RingInt = RingInt(BigInt(0)),
  shift: Int = 0,
  range: Range.Inclusive = 0 downto 0
) extends RingNode {

  override val latency = opType match {
    case FullMult => 8
    case LowMult => 8
    case SquareMult => 8
    case ConstantMult => ???
    case Add => 1
    case Shl => 0
    case Slice => 0
  }

  override val implS = (data: Seq[RingInt]) => opType match {
    case FullMult => data.reduce(_ * _)
    case LowMult => data.reduce(_ multLowBits _)
    case SquareMult => data.head.square
    case ConstantMult => data.head * constant
    case Add => data.reduce(_ + _)
    case Shl => data.head.shl(shift)
    case Slice => data.head.slice(range)
  }

  override val implH = opType match {
    case FullMult => MultiplicationByDspConfig(Full).asNode
    case LowMult => MultiplicationByDspConfig(Low).asNode
    case SquareMult => MultiplicationByDspConfig(Square).asNode
    case ConstantMult => ???
    case Add => ???
    case Shl => (data: Seq[UInt]) => data.head << shift
    case Slice => (data: Seq[UInt]) => data.head(range)
  }

  override val widthsIn = widths

  override val widthOut = opType match {
    case FullMult => widthsIn.sum
    case LowMult => widthsIn.head
    case SquareMult => widthsIn.head * 2
    case ConstantMult => widthsIn.head + constant.width
    case Add => widthsIn.max + 1
    case Shl => widthsIn.head + shift
    case Slice => range.length
  }

  override def widthCheck = opType match {
    case FullMult => widthsIn.forall(_ <= 32)
    case LowMult => widthsIn.forall(_ <= 34)
    case SquareMult => widthsIn.forall(_ <= 34)
    case ConstantMult => ???
    case Add => widthsIn.forall(_ <= 127)
    case Shl => true
    case Slice => true
  }

  override def toString = s"${opType}Node $name"
}

object NodeGen {
  def getSliceNode(node: RingNode, range: Range.Inclusive) =
    new OperatorNode(Slice, Seq(node.widthOut), s"${node.name}_${range.head}_downto_${range.last}", range = range)
}

import NodeGen._

class RingDelay(val latency: Int){
  override def toString = s"delay_$latency"
}

object RingDelay {
  def apply(latency: Int): RingDelay = new RingDelay(latency)
}

class RingDfg extends SimpleDirectedWeightedGraph[RingNode, RingDelay](classOf[RingDelay]) {

  def vertices = super.vertexSet().toSeq

  def edges = super.edgeSet().toSeq

  def inputs = vertices.filter(_.isInstanceOf[InputNode])

  def outputs = vertices.filter(_.isInstanceOf[OutputNode])

  def split(node: RingNode, split: Int) = {
    val rangeHigh = (node.widthOut - 1) downto split
    val rangeLow = (split - 1) downto 0
    val nodeHigh = getSliceNode(node, rangeHigh)
    val nodeLow = getSliceNode(node, rangeLow)
    addVertex(nodeHigh)
    addVertex(nodeLow)
    addEdge(node, nodeHigh, RingDelay(0))
    addEdge(node, nodeLow, RingDelay(0))
    (nodeHigh, nodeLow)
  }
}

object RingDfg {

  def apply(): RingDfg = new RingDfg()

  def main(args: Array[String]): Unit = {

    val karatsubaGraph = new RingDfg
    val x = InputNode(377, "x")
    karatsubaGraph.addVertex(x)
    karatsubaGraph.split(x, 258)
    println(karatsubaGraph)
  }
}

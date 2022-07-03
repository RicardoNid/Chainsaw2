package org.datenlord
package ring

import dfg._

import spinal.core._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

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
    //    println(width)
    println(s"points: ${splitPoints.mkString(" ")}")
    val widthsOut = (width +: splitPoints).zip(splitPoints :+ 0).map { case (width, low) => width - low }
    println(s"outs: ${widthsOut.mkString(" ")}")
    val segments = value.split(splitPoints)
    println(segments.map(_.bitLength).mkString(" "))
    segments.zip(widthsOut).map { case (value, width) => RingInt(value, width) }
  }


}

object RingInt {
  def apply(constant: BigInt): RingInt = RingInt(constant, constant.bitLength)
}

object OperatorType extends Enumeration {
  val FullMult, LowMult, SquareMult, ConstantMult, Add, Split, Merge = Value
  type OperatorType = Value
}

import arithmetic.MultplierMode._
import device.MultiplicationByDspConfig
import ring.OperatorType._


class RingVertex
(
  name: String, latency: Int,
  implS: Seq[RingInt] => Seq[RingInt], implH: Seq[UInt] => Seq[UInt],
  val opType: OperatorType,
  val widthsIn: Seq[Int], val widthsOut: Seq[Int], val widthCheck: Seq[Int] => Boolean
) extends ImplVertex[RingInt, UInt](name, latency, implS, implH) {
  override def toString = s"${opType}Node $name"
}

object RingIoVertex {
  def apply(name: String, width: Int) = new RingVertex(name, 0, (data: Seq[RingInt]) => data, (data: Seq[UInt]) => data, null, Seq(width), Seq(width), null)
}

case class RingPort(override val vertex: RingVertex, override val order: Int)
  extends ImplPort[RingInt, UInt](vertex, order) {

  def width = vertex.widthsOut(order)

  def split(splitPoints: Seq[Int])(implicit dag: RingDag) = {
    val splitVertex = SplitVertex(s"${vertex.name}_split", width, splitPoints)
    dag.addVertex(splitVertex)
    dag.addEdge(this, RingPort(splitVertex, 0))
    (0 until splitPoints.length + 1).map(RingPort(splitVertex, _))
  }

  def +^(that: RingPort, carry: RingPort = null)(implicit dag: RingDag): (RingPort, RingPort) = {
    val addVertex = AddVertex(s"${vertex.name}+${that.vertex.name}", Seq(width, that.width))
    dag.addVertex(addVertex)
    dag.addEdge(this, RingPort(addVertex, 0))
    dag.addEdge(that, RingPort(addVertex, 1))
    if (carry != null) dag.addEdge(carry, RingPort(addVertex, 2))
    (RingPort(addVertex, 0), RingPort(addVertex, 1))
  }

  def merge(tail: Seq[RingPort])(implicit dag: RingDag) = {
    val all = this +: tail
    val mergeVertex = MergeVertex(s"${vertex.name}_merge", all.map(_.width))
    dag.addVertex(mergeVertex)
    all.zipWithIndex.foreach { case (port, i) => dag.addEdge(port, RingPort(mergeVertex, i)) }
    RingPort(mergeVertex, 0)
  }
}

object MultVertex {
  def apply(name: String, opType: OperatorType, widthsIn: Seq[Int]) = {
    val latency = opType match {
      case FullMult => 8
      case LowMult => 8
      case SquareMult => 8
    }
    val implS = (data: Seq[RingInt]) => Seq(opType match {
      case FullMult => data.reduce(_ * _)
      case LowMult => data.reduce(_ multLowBits _)
      case SquareMult => data.head.square
    })
    val implH = opType match {
      case FullMult => MultiplicationByDspConfig(Full).asNode
      case LowMult => MultiplicationByDspConfig(Low).asNode
      case SquareMult => MultiplicationByDspConfig(Square).asNode
    }
    val widthOut = opType match {
      case FullMult => widthsIn.sum
      case LowMult => widthsIn.head
      case SquareMult => widthsIn.head * 2
    }

    def widthCheck = (widthsIn: Seq[Int]) => opType match {
      case FullMult => widthsIn.forall(_ <= 32)
      case LowMult => widthsIn.forall(_ <= 34)
      case SquareMult => widthsIn.forall(_ <= 34)
    }

    new RingVertex(name, latency, implS, implH, opType, widthsIn, Seq(widthOut), widthCheck)
  }
}

object AddVertex {
  def apply(name: String, widthsIn: Seq[Int]) = {
    val latency = 1
    val widthsOut = Seq(1, widthsIn.max)
    val opCount = widthsIn.length
    val implS = (data: Seq[RingInt]) => {
      if (opCount == 3) (data(0) +^ data(1) + data(2)).split(Seq(widthsIn.max))
      else (data(0) +^ data(1)).split(Seq(widthsIn.max))
    }
    val implH = (data: Seq[UInt]) => {
      val ret = if (opCount == 3) {
        val Seq(a, b, carry) = data
        a +^ b + carry
      } else {
        val Seq(a, b) = data
        a +^ b
      }
      Seq(ret.msb.asUInt, ret.takeLow(ret.getBitsWidth - 1).asUInt)
    }

    // 3->2
    def widthCheck = (widthsIn: Seq[Int]) => widthsIn.size == 3 && widthsIn.max <= 127

    new RingVertex(name, latency, implS, implH, Add, widthsIn, widthsOut, widthCheck)
  }
}

object SplitVertex {
  def apply(name: String, widthIn: Int, splitPoints: Seq[Int]) = {
    val latency = 0
    val widthsOut = (widthIn +: splitPoints).zip(splitPoints :+ 0).map { case (width, low) => width - low }
    // example: 110011101.split(Seq(6,3)) = Seq(110, 011, 101)
    val implS = (data: Seq[RingInt]) => data.head.split(splitPoints)
    val implH = (data: Seq[UInt]) => data.head.split(splitPoints).map(_.asUInt)

    def widthCheck = (widthsIn: Seq[Int]) => true

    new RingVertex(name, latency, implS, implH, Split, Seq(widthIn), widthsOut, widthCheck)
  }
}

object MergeVertex {
  def apply(name: String, widthsIn: Seq[Int]) = {
    val latency = 0
    val widthOut = widthsIn.sum
    // example: 110011101.split(Seq(6,3)) = Seq(110, 011, 101)
    val implS = (data: Seq[RingInt]) => {
      val newString = data.zip(widthsIn)
        .map { case (data, width) => data.value.toString(2).padToLeft(width, '0') }
        .mkString("")
      Seq(RingInt(BigInt(newString, 2), widthOut))
    }
    val implH = (data: Seq[UInt]) => Seq(data.reduce(_ @@ _))

    def widthCheck = (widthsIn: Seq[Int]) => true

    new RingVertex(name, latency, implS, implH, Split, widthsIn, Seq(widthOut), widthCheck)
  }
}


class RingDag extends ImplDag[RingInt, UInt] {

  def setInput(name: String, width: Int) = {
    val in = RingIoVertex(name, width)
    addVertex(in)
    inputs += in
    RingPort(in, 0)
  }

  def setOutput(name: String, width: Int) = {
    val out = RingIoVertex(name, width)
    addVertex(out)
    outputs += out
    RingPort(out, 0)
  }
}

object RingDag {

  def apply(): RingDag = new RingDag()

  def main(args: Array[String]): Unit = {

    def bigAdderGraph(width: Int) = {

      implicit val graph = new RingDag
      val x = graph.setInput("x", width)
      val y = graph.setInput("y", width)
      val z = graph.setOutput("z", width + 1)

      val splitPoints = (0 until (width - 1) / 127).reverse.map(i => (i + 1) * 127)
      val xs = x.split(splitPoints).reverse // low -> high
      val ys = y.split(splitPoints).reverse

      logger.info(s"x segments: ${xs.length}")

      val carries = ArrayBuffer[RingPort]()
      val sums = ArrayBuffer[RingPort]()

      xs.zip(ys).foreach { case (x, y) =>
        val (carry, sum) =
          if (carries.nonEmpty) x.+^(y, carries.last)
          else x +^ y
        carries += carry
        sums += sum
      }

      val ret = carries.last.merge(sums.reverse) // high -> low
      graph.addEdge(ret, z)

      graph.validate()
      graph
    }

    println(bigAdderGraph(377))
    val x = Random.nextBigInt(377)
    val y = Random.nextBigInt(377)
    val algo = bigAdderGraph(377).implS
    println(algo(Seq(RingInt(x, 377), RingInt(y, 377))))
    println(x + y)

  }


}

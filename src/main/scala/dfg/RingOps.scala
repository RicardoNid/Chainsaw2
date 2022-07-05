package org.datenlord
package dfg

import arithmetic.MultplierMode.{Full, Low, Square}
import device.MultiplicationByDspConfig
import dfg.OpType._

import spinal.core._

object MultVertex {

  def apply(name: String, opType: OpType, widthsIn: Seq[Int]) = {

    val config = opType match {
      case FullMult => MultiplicationByDspConfig(Full)
      case LowMult => MultiplicationByDspConfig(Low)
      case SquareMult => MultiplicationByDspConfig(Square)
    }

    val latency = config.latency

    val implS = (data: Seq[BigInt]) => Seq(opType match {
      case FullMult => data.product
      case LowMult => data.product % (BigInt(1) << widthsIn.max)
      case SquareMult => data.head * data.head
    })

    val widthOut = opType match {
      case FullMult => widthsIn.sum
      case LowMult => widthsIn.max
      case SquareMult => widthsIn.head * 2
    }

    val implH = (data: Seq[UInt]) => {
      require(data(0).getBitsWidth == widthsIn(0))
      require(data(1).getBitsWidth == widthsIn(1))
      val seq = config.asNode.apply(data)
      Seq(seq.head.resize(widthOut))
    }

    def widthCheck = (widthsIn: Seq[Int]) => widthsIn.forall(_ <= config.baseWidth)

    new RingVertex(name, latency, implS, implH, opType, widthsIn, Seq(widthOut), widthCheck)
  }
}

/** Adder vertex which has 3 inputs(x,y,carry) and 2 outputs(carry, sum), implemented by LUT2 and CARRY8
 *
 */
object AddVertex {
  def apply(name: String, widthsIn: Seq[Int]) = {
    require(widthsIn(0) == widthsIn(1))
    val latency = 1
    val widthsOut = Seq(1, widthsIn.max)
    val opCount = widthsIn.length

    val implS = (data: Seq[BigInt]) => {
      val ret = data.sum
      ret.split(Seq(widthsIn.max))
    }

    val implH = (data: Seq[UInt]) => {
      require(data(0).getBitsWidth == widthsIn(0))
      require(data(1).getBitsWidth == widthsIn(1))
      val ret =
        if (opCount == 3) data(0) +^ data(1) + data(2)
        else data(0) +^ data(1)
      Seq(ret.d(1).msb.asUInt, ret.d(1).takeLow(ret.getBitsWidth - 1).asUInt)
    }

    def widthCheck = (widthsIn: Seq[Int]) => widthsIn.max <= 127

    new RingVertex(name, latency, implS, implH, Add, widthsIn, widthsOut, widthCheck)
  }
}

object SubVertex {
  def apply(name: String, widthsIn: Seq[Int]) = {
    require(widthsIn(0) == widthsIn(1))
    val latency = 1
    val widthsOut = Seq(1, widthsIn.max)
    val opCount = widthsIn.length

    val implS = (data: Seq[BigInt]) => {
      def invert(value: Char) = if (value == '1') '0' else '1'

      val invertOperand = BigInt(data(1).toString(2).padToLeft(widthsIn(1), '0').map(invert), 2)
      val ret = if (opCount == 3) data(0) + invertOperand + data(2) else data(0) + invertOperand + 1
      ret.split(Seq(widthsIn.max))
    }

    val implH = (data: Seq[UInt]) => {
      val ret =
        if (opCount == 3) data(0) +^ ~data(1) + data(2)
        else data(0) +^ ~data(1) + U(1, 1 bits)
      Seq(ret.d(1).msb.asUInt, ret.d(1).takeLow(ret.getBitsWidth - 1).asUInt)
    }

    def widthCheck = (widthsIn: Seq[Int]) => widthsIn.max <= 127

    new RingVertex(name, latency, implS, implH, Sub, widthsIn, widthsOut, widthCheck)
  }
}

object SplitVertex {
  def apply(name: String, widthIn: Int, splitPoints: Seq[Int]) = {
    val latency = 0
    val widthsOut = (widthIn +: splitPoints).zip(splitPoints :+ 0).map { case (width, low) => width - low }
    // example: 110011101.split(Seq(6,3)) = Seq(110, 011, 101)
    val implS = (data: Seq[BigInt]) => {
      data.head.split(splitPoints)
    }
    val implH = (data: Seq[UInt]) => data.head.split(splitPoints).map(_.asUInt)

    def widthCheck = (widthsIn: Seq[Int]) => true

    new RingVertex(name, latency, implS, implH, Split, Seq(widthIn), widthsOut, widthCheck)
  }
}

object MergeVertex {
  def apply(name: String, widthsIn: Seq[Int]) = {
    val latency = 0
    val widthOut = widthsIn.sum
    val implS = (data: Seq[BigInt]) => {
      val newString = data.zip(widthsIn)
        .map { case (data, width) => data.toString(2).padToLeft(width, '0') }
        .mkString("")
      Seq(BigInt(newString, 2))
    }
    val implH = (data: Seq[UInt]) => Seq(data.reduce(_ @@ _))

    def widthCheck = (widthsIn: Seq[Int]) => true

    new RingVertex(name, latency, implS, implH, Merge, widthsIn, Seq(widthOut), widthCheck)
  }
}

object ResizeVertex {
  def apply(name: String, widthIn: Int, widthOut: Int) = {
    val latency = 0
    val implS = (data: Seq[BigInt]) => Seq(data.head % (BigInt(1) << widthOut))
    val implH = (data: Seq[UInt]) => Seq(data.head.resize(widthOut))

    def widthCheck = (widthsIn: Seq[Int]) => true

    new RingVertex(name, latency, implS, implH, Resize, Seq(widthIn), Seq(widthOut), widthCheck)
  }
}

/** This is for vertices which do no operations, this can be used as input, output or intermediate variables
 *
 */
object RingVarVertex {
  def apply(name: String, width: Int) =
    new RingVertex(name, 0, (data: Seq[BigInt]) => data, (data: Seq[UInt]) => data, Var, Seq(width), Seq(width), (widths: Seq[Int]) => true)
}
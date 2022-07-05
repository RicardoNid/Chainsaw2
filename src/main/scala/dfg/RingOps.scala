package org.datenlord
package dfg

import arithmetic.MultplierMode.{Full, Low, Square}
import device.MultiplicationByDspConfig
import spinal.core.UInt

import dfg.OpType._

object MultVertex {

  def apply(name: String, opType: OpType, widthsIn: Seq[Int]) = {

    val latency = opType match {
      case FullMult => 8
      case LowMult => 8
      case SquareMult => 8
    }

    val implS = (data: Seq[BigInt]) => Seq(opType match {
      case FullMult => data.product
      case LowMult => data.product % (BigInt(1) << widthsIn.max)
      case SquareMult => data.head * data.head
    })

    val implH = opType match {
      case FullMult => MultiplicationByDspConfig(Full).asNode
      case LowMult => MultiplicationByDspConfig(Low).asNode
      case SquareMult => MultiplicationByDspConfig(Square).asNode
    }

    val widthOut = opType match {
      case FullMult => widthsIn.sum
      case LowMult => widthsIn.max
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

/** Adder vertex which has 3 inputs(x,y,carry) and 2 outputs(carry, sum), implemented by LUT2 and CARRY8
 *
 */
object AddVertex {
  def apply(name: String, widthsIn: Seq[Int]) = {
    val latency = 1
    val widthsOut = Seq(1, widthsIn.max)
    val opCount = widthsIn.length

    val implS = (data: Seq[BigInt]) => {
      val ret = data.sum
      ret.split(Seq(widthsIn.max))
    }

    val implH = (data: Seq[UInt]) => {
      val ret =
        if (opCount == 3) data(0) +^ data(1) + data(2)
        else data(0) +^ data(1)
      Seq(ret.d(1).msb.asUInt, ret.d(1).takeLow(ret.getBitsWidth - 1).asUInt)
    }

    def widthCheck = (widthsIn: Seq[Int]) => widthsIn.max <= 127

    new RingVertex(name, latency, implS, implH, Add, widthsIn, widthsOut, widthCheck)
  }
}

object SplitVertex {
  def apply(name: String, widthIn: Int, splitPoints: Seq[Int]) = {
    val latency = 0
    val widthsOut = (widthIn +: splitPoints).zip(splitPoints :+ 0).map { case (width, low) => width - low }
    // example: 110011101.split(Seq(6,3)) = Seq(110, 011, 101)
    val implS = (data: Seq[BigInt]) => data.head.split(splitPoints)
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

/** This is for vertices which do no operations, this can be used as input, output or intermediate variables
 *
 */
object RingVarVertex {
  def apply(name: String, width: Int) =
    new RingVertex(name, 0, (data: Seq[BigInt]) => data, (data: Seq[UInt]) => data, Var, Seq(width), Seq(width), (widths:Seq[Int]) => true)
}
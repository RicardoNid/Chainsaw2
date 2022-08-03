package org.datenlord
package dfg

import arithmetic.MultplierMode.{Full, Low, Square}
import device.MultiplicationByDspConfig
import dfg.OpType._

import org.datenlord.dfg.Direction.{In, Out}
import spinal.core._

case class ArithInfo(width: Int, shift: Int) {
  val low = shift
  val high = low + width
  val range = (high - 1) downto low
}

/** This is for crypto implementation on FPGAs of Xilinx UltraScale family
 *
 * @param opType     type of operation
 * @param widthCheck check whether the input widths are valid, according to opType
 */
class RingVertex
(
  name: String, latency: Int,
  implS: Seq[BigInt] => Seq[BigInt], implH: Seq[UInt] => Seq[UInt],
  opType: OpType,
  val infosIn: Seq[ArithInfo], val infosOut: Seq[ArithInfo]
) extends DagVertex[BigInt, UInt](name, latency, opType, implS, implH) {

  def widthsIn = infosIn.map(_.width)

  def widthsOut = infosOut.map(_.width)

  def shiftsIn = infosIn.map(_.shift)

  def shiftsOut = infosOut.map(_.shift)

  override def in(portOrder: Int) = RingPort(this, portOrder, In)

  override def out(portOrder: Int) = RingPort(this, portOrder, Out)

  override def toString = s"$name"
}

object MultVertex {

  def apply(name: String, opType: OpType, infosIn: Seq[ArithInfo]) = {

    val widthsIn = infosIn.map(_.width)
    val shiftsIn = infosIn.map(_.shift)
    val lowWidth = (widthsIn.sum + 1) / 2

    val config = opType match {
      case FullMult => MultiplicationByDspConfig(Full)
      case LowMult => MultiplicationByDspConfig(Low)
      case SquareMult => MultiplicationByDspConfig(Square)
    }

    val latency = config.latency

    val implS = (data: Seq[BigInt]) => Seq(opType match {
      case LowMult => data.product % (BigInt(1) << lowWidth)
      case _ => data.product
    })

    val widthOut = opType match {
      case LowMult => lowWidth
      case _ => widthsIn.sum
    }

    val shiftOut = shiftsIn.sum

    val implH = (data: Seq[UInt]) => {
      require(data.forall(_.getBitsWidth <= config.baseWidth))
      val seq = config.implH.asNode.apply(data)
      Seq(seq.head.resize(widthOut)) // as the multiplier by dsp has a fixed width, resize is required
    }

    new RingVertex(name, latency, implS, implH, opType, infosIn, Seq(ArithInfo(widthOut, shiftOut)))
  }
}

/** Adder vertex which has 3 inputs(x,y,carry) and 2 outputs(carry, sum), implemented by LUT2 and CARRY8. Only when all shiftsIn are the same, this vertex can be implemented by itself, otherwise, it will be converted into bit matrix compressor.
 */
object Add2Vertex {
  def apply(name: String, opType: OpType, infosIn: Seq[ArithInfo]) = {

    val widthsIn = infosIn.map(_.width)
    val shiftsIn = infosIn.map(_.shift)

    val baseWidth = 127

    val latency = 1
    val widthsOut = Seq(1, widthsIn.max)
    val shiftsOut = Seq(shiftsIn.head + widthsIn.max, widthsIn.max)
    val infosOut = widthsOut.zip(shiftsOut).map { case (width, shift) => ArithInfo(width, shift) }
    val opCount = widthsIn.length

    val implS = (data: Seq[BigInt]) => {
      def invert(value: Char) = if (value == '1') '0' else '1'

      opType match {
        case Add2 => val ret = data.sum
          ret.split(Seq(widthsIn.max))
        case Sub2 =>
          val invertOperand = BigInt(data(1).toString(2).padToLeft(widthsIn(1), '0').map(invert), 2)
          val ret = if (opCount == 3) data(0) + invertOperand + data(2) else data(0) + invertOperand + 1
          ret.split(Seq(widthsIn.max))
      }
    }

    val implH = (data: Seq[UInt]) => {
      logger.info(s"implementing $opType with ${shiftsIn(0)} and ${shiftsIn(1)}")
      require(shiftsIn.forall(_ == shiftsIn.head))
      require(data.forall(_.getBitsWidth <= baseWidth))
      if (opCount == 3) require(data(2).getBitsWidth == 1) // carry
      val ret = opType match {
        case Add2 =>
          if (opCount == 3) data(0) +^ data(1) + data(2)
          else data(0) +^ data(1)
        case Sub2 =>
          if (opCount == 3) data(0) +^ ~data(1) + data(2)
          else data(0) +^ ~data(1) + U(1, 1 bits)
      }

      Seq(ret.d(1).msb.asUInt, ret.d(1).takeLow(ret.getBitsWidth - 1).asUInt)
    }

    new RingVertex(name, latency, implS, implH, opType, infosIn, infosOut)
  }
}

object SplitVertex {
  def apply(name: String, infoIn: ArithInfo, splitPoints: Seq[Int]) = {
    require(infoIn.width > splitPoints.max)
    val latency = 0
    val widthsOut = (infoIn.width +: splitPoints).zip(splitPoints :+ 0).map { case (width, low) => width - low }
    val shiftsOut = splitPoints.map(_ + infoIn.shift) :+ infoIn.shift
    val infosOut = widthsOut.zip(shiftsOut).map { case (width, shift) => ArithInfo(width, shift) }
    // example: 110011101.split(Seq(6,3)) = Seq(110, 011, 101), high to low
    val implS = (data: Seq[BigInt]) => data.head.split(splitPoints)
    val implH = (data: Seq[UInt]) => data.head.split(splitPoints).map(_.asUInt)

    new RingVertex(name, latency, implS, implH, Split, Seq(infoIn), infosOut)
  }
}

object MergeVertex {

  def apply(name: String, infosIn: Seq[ArithInfo]) = {
    // TODO: info requirement on merge
    // TODO: deprecate merge
    val widthsIn = infosIn.map(_.width)
    val shiftsIn = infosIn.map(_.shift)
    val latency = 0
    val widthOut = widthsIn.sum
    val shiftOut = shiftsIn.min
    val implS = (data: Seq[BigInt]) => {
      val newString = data.zip(widthsIn)
        .map { case (data, width) => data.toString(2).padToLeft(width, '0') }
        .mkString("")
      Seq(BigInt(newString, 2))
    }
    val implH = (data: Seq[UInt]) => Seq(data.reduce(_ @@ _))

    new RingVertex(name, latency, implS, implH, Merge, infosIn, Seq(ArithInfo(widthOut, shiftOut)))
  }
}

object ResizeVertex {
  def apply(name: String, infoIn: ArithInfo, widthOut: Int) = {
    val latency = 0
    val implS = (data: Seq[BigInt]) => Seq(data.head % (BigInt(1) << widthOut))
    val implH = (data: Seq[UInt]) => Seq(data.head.resize(widthOut))
    new RingVertex(name, latency, implS, implH, Resize, Seq(infoIn), Seq(ArithInfo(widthOut, infoIn.shift)))
  }
}

/** This is for vertices which do no operations, this can be used as input, output or intermediate variables
 *
 */
object RingVarVertex {
  def apply(name: String, info: ArithInfo) =
    new RingVertex(name, 0, (data: Seq[BigInt]) => data, (data: Seq[UInt]) => data, Var, Seq(info), Seq(info))
}
package org.datenlord
package dfg

import arithmetic.MultplierMode.{FULL, HALFLOW, SQUARE}
import device.MultiplicationByDspConfig
import dfg.OpType._

import org.datenlord.dfg.Direction.{In, Out}
import spinal.core._

case class ArithInfo(width: Int, shift: Int, sign: Boolean = true) {
  val low = shift
  val high = low + width
  val range = (high - 1) downto low

  def <<(shiftLeft: Int) = ArithInfo(width, shift + shiftLeft, sign)

  def unary_- = ArithInfo(width, shift, !sign)
}

/** This is for crypto implementation on FPGAs of Xilinx UltraScale family
 *
 * @param opType type of operation
 */
class RingVertex
(
  name: String, latency: Int,
  implH: Seq[UInt] => Seq[UInt],
  opType: OpType,
  val widthsIn: Seq[Int], val widthsOut: Seq[Int]
) extends DagVertex[UInt](name, latency, opType, implH) {

  override def in(portOrder: Int) = RingPort(this, portOrder, In)

  override def out(portOrder: Int) = RingPort(this, portOrder, Out)

  override def toString = s"$name"
}

object MultVertex {
  def apply(name: String, opType: OpType, widthsIn: Seq[Int]) = {

    val widthOut = opType match {
      case LowMult => (widthsIn.sum + 1) / 2
      case _ => widthsIn.sum
    }

    val config = opType match {
      case FullMult => MultiplicationByDspConfig(FULL)
      case LowMult => MultiplicationByDspConfig(HALFLOW)
      case SquareMult => MultiplicationByDspConfig(SQUARE)
    }

    val latency = config.latency

    val implH = (data: Seq[UInt]) => {
      require(data.forall(_.getBitsWidth <= config.baseWidth))
      val seq = config.implH.asNode.apply(data.map(_.resized))
      Seq(seq.head.resize(widthOut)) // as the multiplier by dsp has a fixed width, resize is required
    }

    new RingVertex(name, latency, implH, opType, widthsIn, Seq(widthOut))
  }
}

/** Adder vertex which has 3 inputs(x,y,carry) and 2 outputs(carry, sum), implemented by LUT2 and CARRY8. Only when all shiftsIn are the same, this vertex can be implemented by itself, otherwise, it will be converted into bit matrix compressor.
 */
object BaseAddSubVertex {
  def apply(name: String, opType: OpType, widthsIn: Seq[Int]) = {

    val widthsOut = Seq(1, widthsIn.max)

    val opCount = widthsIn.length

    val implH = (data: Seq[UInt]) => {
      val baseWidth = 127
      require(data.forall(_.getBitsWidth <= baseWidth), s"base: $baseWidth, yours: ${data.map(_.getBitsWidth).mkString(" ")}")
      if (opCount == 3) require(data(2).getBitsWidth == 1) // carry
      val ret = opType match {
        case BASEADD =>
          if (opCount == 3) data(0) +^ data(1) + data(2)
          else data(0) +^ data(1)
        case BASESUB =>
          if (opCount == 3) data(0) +^ ~data(1) + data(2)
          else data(0) +^ ~data(1) + U(1, 1 bits)
      }

      Seq(ret.d(1).msb.asUInt, ret.d(1).takeLow(ret.getBitsWidth - 1).asUInt)
    }

    new RingVertex(name, 1, implH, opType, widthsIn, widthsOut)
  }
}

object AddSubVertex {
  def apply(name: String, opType: OpType, widthsIn: Seq[Int]) = {

    val widthOut = if (opType == ADDC || opType == SUBC) widthsIn.max + 1 else widthsIn.max
    val widthsOut = Seq(widthOut)

    val implH = (data: Seq[UInt]) => {
      val ret = opType match {
        case ADD => data.head + data.last
        case SUB => data.head - data.last
        case ADDC => data.head +^ data.last
        case SUBC => data.head -^ data.last
      }
      Seq(ret.d(1))
    }

    new RingVertex(name, 1, implH, opType, widthsIn, widthsOut)
  }
}

object SplitVertex {
  def apply(name: String, widthIn: Int, splitPoints: Seq[Int]) = {
    require(widthIn > splitPoints.max)
    val latency = 0
    val widthsOut = (widthIn +: splitPoints).zip(splitPoints :+ 0).map { case (width, low) => width - low }
    // example: 110011101.split(Seq(6,3)) = Seq(110, 011, 101), high to low
    val implH = (data: Seq[UInt]) => data.head.split(splitPoints).map(_.asUInt)

    new RingVertex(name, latency, implH, Split, Seq(widthIn), widthsOut)
  }
}

object MergeVertex {
  def apply(name: String, widthsIn: Seq[Int]) = {
    val latency = 0
    val widthOut = widthsIn.sum
    val implH = (data: Seq[UInt]) => Seq(data.reduce(_ @@ _))
    new RingVertex(name, latency, implH, Merge, widthsIn, Seq(widthOut))
  }
}

object ResizeVertex {
  def apply(name: String, widthIn: Int, widthOut: Int) = {
    val latency = 0
    val implH = (data: Seq[UInt]) => Seq(data.head.resize(widthOut))
    new RingVertex(name, latency, implH, RESIZE, Seq(widthIn), Seq(widthOut))
  }
}

// FIXME: actually, it's not mux, but 1-bit mult
object MuxVertex {
  def apply(name: String, widthsIn: Seq[Int]): RingVertex = {
    require(widthsIn.length == 2 && widthsIn.last == 1)
    val latency = 0
    val implH = (data: Seq[UInt]) => Seq(Mux(data.last.asBool, data.head, data.last.getZero))
    new RingVertex(name, latency, implH, MUX, widthsIn, Seq(widthsIn.head))
  }
}

object AndVertex {
  def apply(name: String, widthIn: Int): RingVertex = {
    val latency = 0
    val implH = (data: Seq[UInt]) => Seq(data.head & data.last)
    new RingVertex(name, latency, implH, AND, Seq(widthIn), Seq(widthIn))
  }
}

object ShiftVertex {
  def apply(name: String, widthIn: Int, shiftLeft: Int) = {
    val implH = (data: Seq[UInt]) => if (shiftLeft >= 0) data.map(_ << shiftLeft) else data.map(_ >> -shiftLeft)
    val widthOut = widthIn + shiftLeft
    new RingVertex(name, 0, implH, SHIFT, Seq(widthIn), Seq(widthOut))
  }
}

// multi-operand adder implemented as a compressor tree
object CompressorVertex {
  def apply(name: String, infosIn: Seq[ArithInfo]) = {
    logger.info(s"compress $name:\n${infosIn.mkString(" ")}")
    val config = arithmetic.BmcConfig(infosIn)
    val op = config.op
    val implH = (data: Seq[UInt]) => op(data)
    val widthOut = config.widthOut
    logger.info(s"bmc vertex out $name ${config.widthOut}")
    new RingVertex(s"$name${config.widthOut}", config.fixedLatency, implH, COMPRESS, infosIn.map(_.width), Seq(widthOut, widthOut))
  }
}

object KaraVertex {
  def apply(name: String, widthsIn: Seq[Int]) = {
    val config = device.KaraBaseConfig()
    val op = config.op
    val implH = (data: Seq[UInt]) => op(data)
    val Seq(w0, w1, w2, w3) = widthsIn
    // TODO: better
    val widthsOut = Seq(w0 + w2, w0 + w2 + 1, w1 + w3)
    logger.info(s"vertex with ${w0 + w2}, ${w0 + w2 + 1}, ${w1 + w3}")
    new RingVertex(s"$name", 5, implH, KARA, widthsIn, widthsOut)
  }
}

/** This is for vertices which do no operations, this can be used as input, output or intermediate variables
 *
 */
object RingVarVertex {
  def apply(name: String, width: Int) =
    new RingVertex(name, 0, (data: Seq[UInt]) => data, Var, Seq(width), Seq(width))
}
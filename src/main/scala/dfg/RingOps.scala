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

object KeepShift {
  def apply(infos: Seq[ArithInfo]) = {
    val shift = infos.head.shift
    require(infos.forall(_.shift == shift))
    shift
  }
}

object MultVertex {
  def apply(name: String, opType: OpType, infosIn: Seq[ArithInfo]) = {

    val widthsIn = infosIn.map(_.width)
    val widthOut = opType match {
      case LowMult => (widthsIn.sum + 1) / 2
      case _ => widthsIn.sum
    }

    val shiftOut = KeepShift(infosIn)

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

    new RingVertex(name, latency, null, implH, opType, infosIn, Seq(ArithInfo(widthOut, shiftOut)))
  }
}

/** Adder vertex which has 3 inputs(x,y,carry) and 2 outputs(carry, sum), implemented by LUT2 and CARRY8. Only when all shiftsIn are the same, this vertex can be implemented by itself, otherwise, it will be converted into bit matrix compressor.
 */
object BaseAddSubVertex {
  def apply(name: String, opType: OpType, infosIn: Seq[ArithInfo]) = {

    val widthsIn = infosIn.map(_.width)
    val widthsOut = Seq(1, widthsIn.max)

    val shiftOut = KeepShift(infosIn)
    val shiftsOut = Seq(shiftOut, shiftOut)

    val infosOut = widthsOut.zip(shiftsOut).map { case (width, shift) => ArithInfo(width, shift) }
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

    new RingVertex(name, 1, null, implH, opType, infosIn, infosOut)
  }
}

object AddSubVertex {
  def apply(name: String, opType: OpType, infosIn: Seq[ArithInfo]) = {

    val widthsIn = infosIn.map(_.width)
    val widthOut = if (opType == ADDC || opType == SUBC) widthsIn.max + 1 else widthsIn.max
    val widthsOut = Seq(widthOut)

    val shiftOut = KeepShift(infosIn)
    val shiftsOut = Seq(shiftOut)

    val infosOut = widthsOut.zip(shiftsOut).map { case (width, shift) => ArithInfo(width, shift) }

    val implH = (data: Seq[UInt]) => {
      val ret = opType match {
        case ADD => data.head + data.last
        case SUB => data.head - data.last
        case ADDC => data.head +^ data.last
        case SUBC => data.head -^ data.last
      }
      Seq(ret.d(1))
    }

    new RingVertex(name, 1, null, implH, opType, infosIn, infosOut)
  }
}

object SplitVertex {
  def apply(name: String, infoIn: ArithInfo, splitPoints: Seq[Int]) = {
    require(infoIn.width > splitPoints.max)
    val latency = 0
    val widthsOut = (infoIn.width +: splitPoints).zip(splitPoints :+ 0).map { case (width, low) => width - low }
    logger.info(s"split widths out: ${widthsOut.mkString(" ")}")
    val shiftsOut = Seq.fill(splitPoints.length + 1)(infoIn.shift)
    val infosOut = widthsOut.zip(shiftsOut).map { case (width, shift) => ArithInfo(width, shift) }
    // example: 110011101.split(Seq(6,3)) = Seq(110, 011, 101), high to low
    val implH = (data: Seq[UInt]) => data.head.split(splitPoints).map(_.asUInt)

    new RingVertex(name, latency, null, implH, Split, Seq(infoIn), infosOut)
  }
}

object MergeVertex {

  def apply(name: String, infosIn: Seq[ArithInfo]) = {
    val widthsIn = infosIn.map(_.width)
    val shiftOut = KeepShift(infosIn)
    val latency = 0
    val widthOut = widthsIn.sum
    val implH = (data: Seq[UInt]) => Seq(data.reduce(_ @@ _))

    new RingVertex(name, latency, null, implH, Merge, infosIn, Seq(ArithInfo(widthOut, shiftOut)))
  }
}

object ResizeVertex {
  def apply(name: String, infoIn: ArithInfo, widthOut: Int) = {
    val latency = 0
    val implH = (data: Seq[UInt]) => Seq(data.head.resize(widthOut))
    new RingVertex(name, latency, null, implH, RESIZE, Seq(infoIn), Seq(ArithInfo(widthOut, infoIn.shift)))
  }
}

// FIXME: actually, it's not mux, but 1-bit mult
object MuxVertex {
  def apply(name: String, infosIn: Seq[ArithInfo]) = {
    val latency = 0
    val implH = (data: Seq[UInt]) => Seq(Mux(data.last.asBool, data.head, data.last.getZero))
    new RingVertex(name, latency, null, implH, MUX, infosIn, Seq(infosIn.head))
  }
}

object AndVertex {
  def apply(name: String, infoIn: ArithInfo) = {
    val latency = 0
    val implH = (data: Seq[UInt]) => Seq(data.head & data.last)
    new RingVertex(name, latency, null, implH, AND, Seq(infoIn), Seq(infoIn))
  }
}

object ShiftVertex {
  def apply(name: String, infoIn: ArithInfo, shiftLeft: Int) = {
    val implH = (data: Seq[UInt]) => if (shiftLeft >= 0) data.map(_ << shiftLeft) else data.map(_ >> -shiftLeft)
    val infoOut = ArithInfo(infoIn.width + shiftLeft, infoIn.shift)
    new RingVertex(name, 0, null, implH, SHIFT, Seq(infoIn), Seq(infoOut))
  }
}

object CompressorVertex {
  def apply(name: String, infosIn: Seq[ArithInfo]) = {
    logger.info(s"compress $name:\n${infosIn.mkString(" ")}")
    val config = arithmetic.BmcConfig(infosIn)
    val op = config.op
    val implH = (data: Seq[UInt]) => op(data)
    val infoOut = ArithInfo(config.widthOut, infosIn.head.shift)
    logger.info(s"bmc vertex out $name ${config.widthOut}")
    new RingVertex(s"$name${config.widthOut}", config.fixedLatency, null, implH, COMPRESS, infosIn, Seq(infoOut, infoOut))
  }
}

object KaraVertex {
  def apply(name: String, infosIn: Seq[ArithInfo]) = {
    val config = device.KaraBaseConfig()
    val op = config.op
    val implH = (data: Seq[UInt]) => op(data)
    val Seq(w0, w1, w2, w3) = infosIn.map(_.width)
    // TODO: better
    val widthsOut = Seq(w0 + w2, w0 + w2 + 1, w1 + w3)
    logger.info(s"vertex with ${w0 + w2}, ${w0 + w2 + 1}, ${w1 + w3}")
    val infosOut = widthsOut.map(ArithInfo(_, 0))
    new RingVertex(s"$name", 5, null, implH, KARA, infosIn, infosOut)
  }
}

/** This is for vertices which do no operations, this can be used as input, output or intermediate variables
 *
 */
object RingVarVertex {
  def apply(name: String, info: ArithInfo) =
    new RingVertex(name, 0, (data: Seq[BigInt]) => data, (data: Seq[UInt]) => data, Var, Seq(info), Seq(info))
}
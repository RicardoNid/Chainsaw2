package org.datenlord
package dfg

/** --------
 * implement all kinds of different vertices
 * -------- */

import arithmetic.MultplierMode._
import device.MultiplicationByDspConfig
import dfg.Direction.{In, Out}
import dfg.OpType._

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

/** --------
 * base operators which have optimized implementation for Xilinx Ultrascale
 * -------- */

/** binary add/sub with carryIn and carryOut and limited width, which can be used to build pipelined long add/sub, this is not exposed to the user
 */
object BaseBinaryAddSubVertex {
  def apply(name: String, opType: OpType, widthsIn: Seq[Int]) = {
    val withCarryIn = widthsIn.length == 3
    require(widthsIn.forall(_ <= binaryAddLimit))
    if (withCarryIn) require(widthsIn.last == 1)
    val widthsOut = Seq(1, widthsIn.max)
    val latency = 1
    val implH = (data: Seq[UInt]) => {
      val Seq(a, b, c) = data.padTo(3, null)
      val ret = opType match {
        case BASEADD => if (withCarryIn) a +^ b + c else a +^ b
        case BASESUB => if (withCarryIn) a +^ ~b + c else a +^ ~b + U(1, 1 bits)
      }
      Seq(ret.d(latency).msb.asUInt, ret.d(latency).takeLow(ret.getBitsWidth - 1).asUInt)
    }
    new RingVertex(name, latency, implH, opType, widthsIn, widthsOut)
  }
}

/** mult nodes which consume one or several dsp slices
 *
 */
object BaseMultVertex {
  // TODO: implement mult for HALFHIGH
  def apply(name: String, mode: MultiplierMode, widthsIn: Seq[Int]): RingVertex = {
    val config = MultiplicationByDspConfig(mode)
    require(widthsIn.forall(_ <= config.baseWidth))
    val widthOut = config.widthOut
    val latency = config.latency
    val implH = (data: Seq[UInt]) => {
      val seq = config.implH.asNode.apply(data.map(_.resized))
      Seq(seq.head.resize(widthOut)) // as the multiplier by dsp has a fixed width, resize is required
    }
    new RingVertex(name, latency, implH, OpType.fromMultMode(mode), widthsIn, Seq(widthOut))
  }
}

// TODO: Merge this with mults

/** implementation of karatsuba pattern on DSP slice, with optimization for DSP48E2, this is not exposed to the user
 *
 */
object BaseKaraVertex {
  def apply(name: String, widthsIn: Seq[Int]) = {
    val config = device.KaraBaseConfig()
    val implH = (data: Seq[UInt]) => config.implH.asNode.apply(data.map(_.resized))
    val Seq(w0, w1, w2, w3) = widthsIn
    val widthsOut = Seq(w0 + w2, w0 + w2 + 1, w1 + w3)
    logger.info(s"vertex with ${w0 + w2}, ${w0 + w2 + 1}, ${w1 + w3}")
    new RingVertex(s"$name", 5, implH, KARA, widthsIn, widthsOut)
  }
}

// TODO: implement Base Ternary Add/Sub

/** --------
 * general arithmetic operators which can be substituted by a subgraph consisting of base operators, by expression rewriting
 * -------- */

/** general add/sub with no limit on width, can be rewritten through optimization
 *
 */
object AddSubVertex {
  def apply(name: String, opType: OpType, widthsIn: Seq[Int]): RingVertex = {

    val widthOut = if (opType == ADDC || opType == SUBC) widthsIn.max + 1 else widthsIn.max
    val latency = 1
    val implH = (data: Seq[UInt]) => { // plain implementation
      val ret = opType match {
        case ADD => data.head + data.last
        case SUB => data.head - data.last
        case ADDC => data.head +^ data.last
        case SUBC => data.head -^ data.last
      }
      Seq(ret.d(latency))
    }
    new RingVertex(name, latency, implH, opType, widthsIn, Seq(widthOut))
  }
}

/** general mult no limit on width, can be rewritten through optimization
 *
 */
object MultVertex {
  // TODO: implement mult for HALFHIGH
  def apply(name: String, mode: MultiplierMode, widthsIn: Seq[Int]): RingVertex = {
    val widthOut = widthsIn.sum
    val latency = 1
    val implH = (data: Seq[UInt]) => {
      val prod = data.reduce(_ * _)
      val ret = mode match {
        case FULL => prod
        case HALFLOW => prod.takeLow((widthOut + 1) / 2).asUInt
        case SQUARE => prod
      }
      Seq(ret.d(1))
    }
    new RingVertex(name, latency, implH, OpType.fromMultMode(mode), widthsIn, Seq(widthOut))
  }
}

/** --------
 * simple operators
 * -------- */

// TODO: change the implementation to low to high pattern(input and output)
object SplitVertex {
  def apply(name: String, widthIn: Int, splitPoints: Seq[Int]): RingVertex = {
    require(widthIn > splitPoints.max)
    val widthsOut = (widthIn +: splitPoints).zip(splitPoints :+ 0).map { case (width, low) => width - low }
    val latency = 0
    val implH = (data: Seq[UInt]) => data.head.split(splitPoints).map(_.asUInt)
    new RingVertex(name, latency, implH, SPLIT, Seq(widthIn), widthsOut)
  }
}

object MergeVertex {
  def apply(name: String, widthsIn: Seq[Int]): RingVertex = {
    val widthOut = widthsIn.sum
    val latency = 0
    val implH = (data: Seq[UInt]) => Seq(data.reduce(_ @@ _))
    new RingVertex(name, latency, implH, MERGE, widthsIn, Seq(widthOut))
  }
}

object ResizeVertex {
  def apply(name: String, widthIn: Int, widthOut: Int) = {
    val latency = 0
    val implH = (data: Seq[UInt]) => Seq(data.head.resize(widthOut))
    new RingVertex(name, latency, implH, RESIZE, Seq(widthIn), Seq(widthOut))
  }
}

/** shift left/right, the shift value can be recovered by widthOut - widthIn
 *
 */
object ShiftVertex {
  def apply(name: String, widthIn: Int, shiftLeft: Int): RingVertex = {
    val implH = (data: Seq[UInt]) => if (shiftLeft >= 0) data.map(_ << shiftLeft) else data.map(_ >> -shiftLeft)
    val widthOut = widthIn + shiftLeft
    new RingVertex(name, 0, implH, SHIFT, Seq(widthIn), Seq(widthOut))
  }
}

// FIXME: actually, it's not mux, but 1-bit mult
// TODO: merge with MultVertex
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

/** multi-operand adder implemented as a compressor tree
 *
 */
object CompressorVertex {
  def apply(name: String, infosIn: Seq[ArithInfo]): RingVertex = {
    logger.info(s"compress $name:\n${infosIn.mkString(" ")}")
    val config = arithmetic.BitHeapCompressorConfig(infosIn)
    val implH = (data: Seq[UInt]) => config.implH.asNode.apply(data)
    val widthOut = config.widthOut
    logger.info(s"bmc vertex out $name ${config.widthOut}")
    new RingVertex(s"$name${config.widthOut}", config.latency, implH, COMPRESS, infosIn.map(_.width), Seq.fill(2)(widthOut))
  }
}

/** This is for vertices which do no operations, this can be used as input, output or intermediate variables
 *
 */
object RingVarVertex {
  def apply(name: String, width: Int) =
    new RingVertex(name, 0, (data: Seq[UInt]) => data, Var, Seq(width), Seq(width))
}
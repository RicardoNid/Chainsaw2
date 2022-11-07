package org.datenlord
package arithmetic

import xilinx.VivadoUtilRequirement
import spinal.core._
import spinal.core.sim.SimBoolPimper
import spinal.lib._

import scala.language.postfixOps

/** carry propagate binary/ternary adder of any width
  *
  * @param widthIn
  *   width of all input operands
  * @param mode
  *   binary / ternary
  * @param sub
  *   number of negative operands, negative operands should appear after positive ones
  */
case class CpaWithSignFlagConfig(widthIn: Int, mode: AdderType, sub: Int = 0) extends TransformDfg {

  override val name   = s"CPA with signFlag"
  override val opType = mode

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = mode match {
      case BinaryAdder      => data.sum
      case BinarySubtractor => data(0) - data(1)
      case TernaryAdder =>
        sub match {
          case 0 => data.sum
          case 1 => data(0) + data(1) - data(2)
        }
    }
    Seq(ret)
  }

  override val size = mode match {
    case BinaryAdder      => (2, 1)
    case BinarySubtractor => (2, 1)
    case TernaryAdder     => (3, 1)
  }

  override val widthsIn = Seq.fill(size._1)(widthIn)

  val widthOut = mode match {
    case BinaryAdder      => widthIn + 1
    case BinarySubtractor => widthIn
    case TernaryAdder =>
      sub match {
        case 0 => widthIn + 2
        case 1 => widthIn + 1
      }
  }

  override val widthsOut = Seq(widthOut)

  val slice = mode match { // number of sub-adders
    case BinaryAdder      => binaryAddLimit
    case BinarySubtractor => binaryAddLimit
    case TernaryAdder     => ternaryAddLimit
  }
  val coreCount = widthOut.divideAndCeil(slice)

  override def latency = coreCount

  override def implH = CpaWithSignFlag(this)

  override def utilRequirement = mode match {
    case BinaryAdder      => VivadoUtilRequirement(lut = widthOut, carry8 = coreCount * slice.divideAndCeil(8))
    case BinarySubtractor => VivadoUtilRequirement(lut = widthOut, carry8 = coreCount * slice.divideAndCeil(8))
    case TernaryAdder     => VivadoUtilRequirement(lut = widthOut, carry8 = coreCount * slice.divideAndCeil(8))
  }
}

case class CpaWithSignFlag(config: CpaWithSignFlagConfig) extends TransformModule[UInt, UInt] with SignFlagPort {

  import config._

  override val dataIn     = slave Flow Fragment(Vec(UInt(widthIn bits), inputPortWidth))
  override val dataOut    = master Flow Fragment(Vec(UInt(widthOut bits), outputPortWidth))
  override val isPositive = out Bool ()
  val extendedDataIn      = dataIn.fragment.map(_.resize(widthOut))

  val coreWidths = Seq.fill(widthOut)(1).grouped(slice).toSeq.map(_.sum) // width of each segment
  // get input words
  val slices    = coreWidths.scan(0)(_ + _).prevAndNext { case (prev, next) => (next - 1) downto prev }
  val dataWords = extendedDataIn.map(whole => slices.map(whole(_))).transpose
  // prepare output words
  val sumWords = coreWidths.map(w => UInt(w bits)) //

  val carriesStart = mode match {
    case BinaryAdder      => Seq(False)
    case BinarySubtractor => Seq(True)
    case TernaryAdder =>
      sub match {
        case 0 => Seq(False, False)
        case 1 => Seq(False, True)
      }
  }

  val carryOuts = Seq.iterate((carriesStart, 0), coreCount + 1) { case (carries, i) =>
    mode match {

      case BinaryAdder =>
        val cin       = carries.head
        val Seq(x, y) = dataWords(i).map(_.d(i))
        val ret       = x +^ y + cin.asUInt
        sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(coreCount - i)
        (Seq(ret.msb.d(1)), i + 1)
      case BinarySubtractor =>
        val cin       = carries.head
        val Seq(x, y) = dataWords(i).map(_.d(i))
        val ret       = x +^ ~y + cin.asUInt
        sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(coreCount - i)
        (Seq(ret.msb.d(1)), i + 1)
      case TernaryAdder =>
        val Seq(cin0, cin1) = carries                             // get input
        val Seq(x, y, z)    = dataWords(i).map(_.d(i))
        val core            = Compressor3to1(x.getBitsWidth, sub) // connection
        core.cIn0   := cin0
        core.cIn1   := cin1
        core.x      := x
        core.y      := y
        core.z      := z
        sumWords(i) := core.sumsOut.d(coreCount - i)
        (Seq(core.cOut0.d(1), core.cOut1.d(1)), i + 1) // pass output
    }
  }

  dataOut.fragment.head := sumWords.reverse.reduce(_ @@ _).resize(widthOut)
  mode match {
    case BinaryAdder      => isPositive := True
    case BinarySubtractor => isPositive := carryOuts.last._1.head
    case TernaryAdder     => isPositive := carryOuts.last._1.head ^ carryOuts.last._1.last
  }

  autoValid()
  autoLast()
}

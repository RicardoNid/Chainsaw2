package org.datenlord
package arithmetic

import arithmetic.MultplierMode._
import dfg.ArithInfo

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class BigConstantMultiplicationAnotherConfig(constant: BigInt, widthIn: Int, mode: MultiplierMode, widthTake: Int = 0, useCsd: Boolean = false)
  extends TransformBase {

  // digits of the constant, low to high

  val temp = if (useCsd) Csd.fromBigInt(constant).csd else constant.toString(2)
  val constantString = if (temp.startsWith("0")) temp.tail else temp
  logger.info(s"constant original: ${constant.toString(2)}")
  logger.info(s"constant used    : $constantString")

  val widthAll = constant.bitLength + widthIn
  val widthDrop = widthAll - widthTake

  val infos = constantString.reverse.zipWithIndex
    .filter(_._1 != '0') // skip 0s
    .map { case (digit, position) =>
      val sign = digit == '1'
      val info = mode match {
        case FULL => ArithInfo(widthIn, position, sign)
        case HALFLOW => ArithInfo(widthIn min (widthTake - position), position, sign)
        case HALFHIGH => ArithInfo((widthIn + position - widthDrop) max 0, widthDrop, sign)
      }
      info
    }
    .filterNot(_.width == 0)

  infos.foreach(info => println(s"info: $info"))

  val compressorConfig = BitHeapCompressorConfig(infos)

  // TODO: accurate errorbound
  def errorBound = {
    // sum up all the bits in lower part
    val error = (1 to widthDrop) // lower(dropped) part
      .map(i => (BigInt(1) << (i - 1)) * i) // weighted bits
      .sum // sum up
    error / (BigInt(1) << widthDrop) + 1
  }

  if(mode == HALFHIGH) logger.info(s"error bound of the MSB multiplication is $errorBound")

  def metric(yours: Seq[BigInt], golden: Seq[BigInt]): Boolean =
    yours.zip(golden).forall { case (y, g) =>
      val error = g - y
      logger.info(s"error of MSB mode is $error")
      error <= errorBound
    }

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val product = dataIn.asInstanceOf[Seq[BigInt]].map(_ * constant)
    mode match {
      case FULL => product
      // TODO: draw pictures for explanation
      case HALFLOW => product.map(_ % (BigInt(1) << widthTake))
      case HALFHIGH => product.map(_ >> widthDrop)
    }
  }

  override val size = (1, 1)

  override def latency = compressorConfig.latency

  override def implH = BigConstantMultiplicationAnother(this)
}

case class BigConstantMultiplicationAnother(config: BigConstantMultiplicationAnotherConfig) extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits)))
  val widthOut = if (mode == FULL) widthAll else widthTake
  override val dataOut = master Flow Fragment(Vec(UInt(widthOut bits)))

  val data = dataIn.fragment.head

  val operandsIn = infos.map { info =>
    mode match {
      case FULL => data
      case HALFLOW => data.takeLow(info.width).asUInt
      case HALFHIGH => data.takeHigh(info.width).asUInt
    }
  }

  val rows = compressorConfig.implH.asNode(operandsIn)
  val ret =
    if (useCsd) (rows(0) +^ rows(1)) - (rows(2) +^ rows(3))
    else rows.reduce(_ +^ _)

  mode match {
    case FULL => dataOut.fragment.head := ret.resized
    case HALFLOW => dataOut.fragment.head := ret(widthTake - 1 downto 0)
    case HALFHIGH => dataOut.fragment.head := ret(widthAll - 1 downto widthDrop)
  }

  autoValid()
  autoLast()
}

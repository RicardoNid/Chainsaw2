package org.datenlord
package arithmetic

import arithmetic.MultplierMode._
import dfg.ArithInfo

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class BigConstantMultiplicationByCompressorTreeConfig(constant: BigInt, widthIn: Int, mode: MultiplierMode, widthTake: Int = 0, useCsd: Boolean = false)
  extends TransformBase {

  // digits of the constant, low to high
  val temp = if (useCsd) Csd.fromBigInt(constant).csd else constant.toString(2) // get binary/CSD coded digits
  val constantString = if (temp.startsWith("0")) temp.tail else temp // remove the leading 0 of CSD
  logger.info(s"constant original: ${constant.toString(2)}")
  logger.info(s"constant used    : $constantString")

  val widthAll = constantString.length + widthIn // width of the full product
  val widthDrop = widthAll - widthTake // width of dropped bits
  logger.info(s"widthAll: $widthAll")
  logger.info(s"widthDrop: $widthDrop")

  // constructing the infos for bit heap
  val infos = constantString.reverse.zipWithIndex // for each digit in the constant
    .filter(_._1 != '0') // skip 0s
    .map { case (digit, position) =>
      val sign = digit == '1' // for CSD, '1' stands for 1 and '9' stands for -1
      val shift =
        if (mode == MSB) widthDrop max position
        else position // weight of truncated operands
      val width = mode match {
        case FULL => widthIn
        case LSB => (widthTake - position) min widthIn // see the diagram
        case MSB => (widthIn + position - widthDrop) min widthIn
      }
      ArithInfo(width, shift, sign)
    }.filterNot(_.width <= 0)

  infos.foreach(info => println(s"info: $info"))

  val compressorConfig = BitHeapCompressorUseInversionConfig(infos)

  // error bound and the data which lead to maximum error
  val (upper, lower, dataForUpper, dataForLower) =
    algos.TruncatedMult.constantMsbError(constant, widthIn, widthTake)

  if (mode == MSB) logger.info(s"error bound of the MSB multiplication is [$lower, $upper]")

  def metric(yours: Seq[BigInt], golden: Seq[BigInt]): Boolean =
    yours.zip(golden).forall { case (y, g) =>
      val error = g - y
      logger.info(s"error of MSB mode is $error")
      error >= lower && error <= upper
    }

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val product = dataIn.asInstanceOf[Seq[BigInt]].map(_ * constant)
    mode match {
      case FULL => product
      case LSB => product.map(_ % (BigInt(1) << widthTake))
      case MSB => product.map(_ >> widthDrop)
    }
  }

  override val size = (1, 1)

  override def latency = compressorConfig.latency

  override def implH = BigConstantMultiplicationAnother(this)
}

case class BigConstantMultiplicationAnother(config: BigConstantMultiplicationByCompressorTreeConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits)))
  val widthOut = if (mode == FULL) widthAll else widthTake
  override val dataOut = master Flow Fragment(Vec(UInt(widthOut bits)))

  val data = dataIn.fragment.head

  val operandsIn = infos.map { info =>
    mode match {
      case FULL => data
      case LSB => data.takeLow(info.width).asUInt
      case MSB => data.takeHigh(info.width).asUInt
    }
  }

  val ret = compressorConfig.implH.asNode(operandsIn).head

  mode match {
    case FULL => dataOut.fragment.head := ret.resized
    case LSB => dataOut.fragment.head := ret(widthTake - 1 downto 0)
    case MSB => dataOut.fragment.head := ret(widthAll - 1 downto widthDrop)
  }

  autoValid()
  autoLast()
}

package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** implement big constant multiplication by compressor tree
 *
 * @param mode      support truncated multiplications
 * @param widthTake output width for truncated multiplication
 * @param useCsd    reduce number of non-zero digits in constant by csd encoding
 * @see [[arithmetic.Csd]]
 */
case class BcmConfig(constant: BigInt, widthIn: Int, mode: OperatorType, widthTake: Int = 0, useCsd: Boolean = false)
  extends TransformDfg {

  override val name = "bcm"
  override val opType = mode
  override val widthsIn = Seq(widthIn)
  val widthOut = if (mode == FullMultiplier) widthIn + constant.bitLength else widthTake
  override val widthsOut = Seq(widthOut)

  val constantDigits: String = { // get digits of the constant, low to high
    val temp = if (useCsd) Csd.fromBigInt(constant).csd else constant.toString(2) // get binary/CSD coded digits
    if (temp.startsWith("0")) temp.tail else temp // remove the leading 0 of CSD
  }

  val widthAll = constantDigits.length + widthIn // width of the full product
  val widthDrop = widthAll - widthTake // width of dropped bits

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val product = dataIn.asInstanceOf[Seq[BigInt]].map(_ * constant)
    mode match {
      case FullMultiplier => product
      case LsbMultiplier => product.map(_ % (BigInt(1) << widthTake))
      case MsbMultiplier => product.map(_ >> widthDrop)
    }
  }

  override val size = (1, 1)

  /** --------
   * constructing the bit heap compressor
   * -------- */
  val infos = constantDigits.reverse.zipWithIndex // for each digit in the constant
    .filter(_._1 != '0') // skip 0s
    .map { case (digit, position) =>
      val sign = digit == '1' // for CSD, '1' stands for 1 and '9' stands for -1
      val shift =
        if (mode == MsbMultiplier) widthDrop max position
        else position // weight of truncated operands
      val width = mode match {
        case FullMultiplier => widthIn
        case LsbMultiplier => (widthTake - position) min widthIn // see the diagram
        case MsbMultiplier => (widthIn + position - widthDrop) min widthIn
      }
      ArithInfo(width, shift, sign)
    }.filterNot(_.width <= 0)
  val compressorConfig = BitHeapCompressorUseInversionConfig(infos)

  logger.info(
    s"\n----configuration report of big constant multiplier----" +
      s"\n\tmode: ${mode.getClass.getSimpleName.init}" +
      s"\n\twidthAll: $widthAll, widthTake: $widthTake, widthDrop: $widthDrop" +
      s"\n\tuse csd: $useCsd" +
      s"\n\tconstant sequence in use: $constantDigits" +
      s"\n\toperands in total: ${infos.length}"
  )

  // for MSB mode, get the error bound and the worst cases
  // using the worst case, we can have an error very close to the error bound
  val widthCoeff = constantDigits.length
  var upperBound, lowerBound, dataForUpper, dataForLower = BigInt(0)
  if (mode == MsbMultiplier) {
    (0 until widthIn).foreach { i => // accumulate the error bit by bit(low to high), as they are independent
      val widthDropped = (widthDrop - i) min widthCoeff max 0
      val constantDropped = // the constant which a bit should have been multiplied by
        if (useCsd) Csd(constantDigits).takeLow(widthDropped).evaluate
        else BigIntUtil(constant).takeLow(widthDropped)

      if (constantDropped >= BigInt(0)) {
        upperBound += constantDropped << i
        dataForUpper += BigInt(1) << i
      } else {
        lowerBound += constantDropped << i
        dataForLower += BigInt(1) << i
      }
    }
    upperBound = (upperBound >> widthDrop) + 1
    lowerBound = lowerBound >> widthDrop

    logger.info(
      s"\n----error analysis for big constant multiplier at MSB mode----" +
        s"\n\terror bound of MSB multiplication: [$lowerBound, $upperBound]" +
        s"\n\tlower bound achieved by $dataForLower" +
        s"\n\tupper bound achieved by $dataForUpper")
  }

  def metric(yours: Seq[BigInt], golden: Seq[BigInt]): Boolean =
    yours.zip(golden).forall { case (y, g) =>
      if (mode == MsbMultiplier) {
        val error = g - y
        println(s"error of MSB mode is $error")
        error >= lowerBound && error <= upperBound
      } else g == y
    }

  override def latency: Int = compressorConfig.latency + 1 // 2 for register duplication

  override def implH = Bcm(this)
}

case class Bcm(config: BcmConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits)))
  override val dataOut = master Flow Fragment(Vec(UInt()))

  if (skipComponentSim) { "golden hardware"
    val product = dataIn.fragment.head * constant
    val ret = mode match {
      case FullMultiplier => product
      case LsbMultiplier => product.takeLow(widthTake).asUInt
      case MsbMultiplier => product.takeHigh(widthTake).asUInt
    }
    dataOut.fragment.head := ret.d(latency)
  } else {
    // TODO: solve the high fan-out problem in a better way
    val data = dataIn.fragment.head
    val fanOutFactor = 3

    /** --------
     * register duplication for solving high fan-out
     * -------- */
    val dataBufs = Seq.fill(infos.length.divideAndCeil(10))(data.d(1))
    dataBufs.foreach(_.addAttribute("dont_touch", "yes"))
    val operandsIn = infos.zipWithIndex.map { case (info, i) => // get operands by bit heap compressor infos
      val dataBuf = dataBufs(i / 10)
      mode match {
        case FullMultiplier => dataBuf
        case LsbMultiplier => dataBuf.takeLow(info.width).asUInt
        case MsbMultiplier => dataBuf.takeHigh(info.width).asUInt
      }
    }

    val ret = compressorConfig.implH.asFunc(operandsIn).head.resize(widthAll) // using bit heap compressor

    mode match { // take bits of interest according to multiplication mode
      case FullMultiplier => dataOut.fragment.head := ret.resized
      case LsbMultiplier => dataOut.fragment.head := ret(widthTake - 1 downto 0)
      case MsbMultiplier => dataOut.fragment.head := ret(widthAll - 1 downto widthDrop)
    }
  }

  autoValid()
  autoLast()
}

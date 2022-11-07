package org.datenlord
package zprize

import org.datenlord.arithmetic.{ArithInfo, BitHeapCompressorUseInversionConfig, Csd}
import spinal.lib._
import spinal.core._
case class Bcm(constant: BigInt, widthIn: Int, mode: OperatorType, widthTake: Int = 0, useCsd: Boolean = false) extends ChainsawGenerator {

  override def name = "BigConstantMultiplier"

  val constantDigits: String = { // get digits of the constant, low to high
    val tempConstant = if (useCsd) Csd.fromBigInt(constant).csd else constant.toString(2) // get binary/CSD coded digits
    if (tempConstant.startsWith("0")) tempConstant.tail else tempConstant // remove the leading 0 of CSD
  }
  val widthAll  = constantDigits.length + widthIn // width of the full product
  val widthDrop = widthAll - widthTake            // width of dropped bits
  /** -------- golden model
   * --------
   */
  override def impl(dataIn: Seq[Any]) = {
    val product = dataIn.asInstanceOf[Seq[BigInt]].map(_ * constant)
    mode match {
      case FullMultiplier => product
      case LsbMultiplier  => product.map(_ % (BigInt(1) << widthTake))
      case MsbMultiplier  => product.map(_ >> widthDrop)
    }
  }

  /** -------- size information
   * --------
   */
  override var inputTypes = Seq(UIntInfo(widthIn))
  override var outputTypes = mode match { // take bits of interest according to multiplication mode
    case FullMultiplier => Seq(UIntInfo(widthAll))
    case LsbMultiplier  => Seq(UIntInfo(widthTake))
    case MsbMultiplier  => Seq(UIntInfo(widthAll - widthDrop))
  }

  /** -------- constructing the bit heap compressor
   * --------
   */
  val infos = constantDigits.reverse.zipWithIndex // for each digit in the constant
    .filter(_._1 != '0') // skip 0s
    .map { case (digit, position) =>
      val sign = digit == '1' // for CSD, '1' stands for 1 and '9' stands for -1
      val shift =
        if (mode == MsbMultiplier) widthDrop max position
        else position // weight of truncated operands
      val width = mode match {
        case FullMultiplier => widthIn
        case LsbMultiplier  => (widthTake - position) min widthIn // see the diagram
        case MsbMultiplier  => (widthIn + position - widthDrop) min widthIn
      }
      ArithInfo(width, shift, sign)
    }
    .filterNot(_.width <= 0)

  logger.info(
    s"\n----configuration report of big constant multiplier----" +
      s"\n\tmode: ${mode.getClass.getSimpleName.init}" +
      s"\n\twidthAll: $widthAll, widthTake: $widthTake, widthDrop: $widthDrop" +
      s"\n\tuse csd: $useCsd" +
      s"\n\tconstant sequence in use: $constantDigits" +
      s"\n\toperands in total: ${infos.length}"
  )

  val compressorConfig = BitHeapCompressorUseInversionConfig(infos)

  /** -------- timing information
   * --------
   */
  override var inputFormat  = inputNoControl
  override var outputFormat = outputNoControl
  override var latency      = compressorConfig.latency + 1

  // for MSB mode, get the error bound and the worst cases
  // using the worst case, we can have an error very close to the error bound
  val widthCoefficient                                   = constantDigits.length
  var upperBound, lowerBound, dataForUpper, dataForLower = BigInt(0)
  if (mode == MsbMultiplier) {
    (0 until widthIn).foreach { i => // accumulate the error bit by bit(low to high), as they are independent
      val widthDropped = (widthDrop - i) min widthCoefficient max 0
      val constantDropped = // the constant which a bit should have been multiplied by
        if (useCsd) Csd(constantDigits).takeLow(widthDropped).evaluate
        else BigIntUtil(constant).takeLow(widthDropped)

      if (constantDropped >= BigInt(0)) {
        upperBound += constantDropped << i
        dataForUpper += BigInt(1)     << i
      } else {
        lowerBound += constantDropped << i
        dataForLower += BigInt(1)     << i
      }
    }
    upperBound = (upperBound >> widthDrop) + 1
    lowerBound = lowerBound  >> widthDrop

    logger.info(
      s"\n----error analysis for big constant multiplier at MSB mode----" +
        s"\n\terror bound of MSB multiplication: [$lowerBound, $upperBound]" +
        s"\n\tlower bound achieved by $dataForLower" +
        s"\n\tupper bound achieved by $dataForUpper"
    )
  }

  /** -------- implementation
   * --------
   */
  override def implH = new ChainsawModule(this) {
    val data         = uintDataIn.head
    val fanOutFactor = 3

    /** -------- register duplication for solving high fan-out
     * --------
     */
    val dataBufs = Seq.fill(infos.length.divideAndCeil(10))(data.d(1))
    dataBufs.foreach(_.addAttribute("dont_touch", "yes"))
    val operandsIn = infos.zipWithIndex.map { case (info, i) => // get operands by bit heap compressor infos
      val dataBuf = dataBufs(i / 10)
      mode match {
        case FullMultiplier => dataBuf
        case LsbMultiplier  => dataBuf.takeLow(info.width).asUInt
        case MsbMultiplier  => dataBuf.takeHigh(info.width).asUInt
      }
    }

    var ret = compressorConfig.implH.asFunc(operandsIn).head // using bit heap compressor
    logger.info(s"ret width: ${ret.getBitsWidth}")
    ret = ret.resize(widthAll)

    mode match { // take bits of interest according to multiplication mode
      case FullMultiplier => uintDataOut.head := ret.resized
      case LsbMultiplier  => uintDataOut.head := ret(widthTake - 1 downto 0)
      case MsbMultiplier  => uintDataOut.head := ret(widthAll - 1 downto widthDrop)
    }
  }

  def metric(yours: Seq[Any], golden: Seq[Any]): Boolean =
    yours.asInstanceOf[Seq[BigInt]].zip(golden.asInstanceOf[Seq[BigInt]]).forall { case (y, g) =>
      if (mode == MsbMultiplier) {
        val error = g - y
        error >= lowerBound && error <= upperBound
      } else g == y
    }
}

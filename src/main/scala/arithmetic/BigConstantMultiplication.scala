package org.datenlord
package arithmetic

import arithmetic.McmType._
import arithmetic.MultplierMode._
import dfg.ArithInfo

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/** Big constant multiplier implemented by [[MultiConstantMultiplication]] and [[BitHeapCompressor]]
 *
 * @param mode      full, MSB, and LSB mode are supported
 * @param widthTake for MSB/LSB case, it is the width of the final result
 */
case class BigConstantMultiplicationConfig(constant: BigInt, widthIn: Int, mode: MultiplierMode, widthTake: Int = 0)
  extends TransformBase {
  if (mode != FULL) require(widthTake > 0, "for MSB/LSB mode, param widthTake is necessary")
  val widthAll = widthIn + constant.bitLength // for full multiplication
  val widthDrop = widthAll - widthTake // number of dropped bits for MSB/LSB implementation
  val widthOut = mode match {
    case FULL => widthAll
    case HALFLOW => widthTake
    case HALFHIGH => widthTake
  }

  // TODO: a more accurate error bound
  val errorBound = {
    // sum up all the bits in lower part
    val error = (1 to widthDrop) // lower(dropped) part
      .map(i => (BigInt(1) << (i - 1)) * i) // weighted bits
      .sum // sum up
    error / (BigInt(1) << widthDrop) + 1
  }

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

  val solver = PAG // the MCM solver we use
  // TODO: adder limit & solver limit should be parameterized
  val solverLimit = 30 // maximum single coefficient width that the solver support
  // FIXME: result is wrong when using adderLimit != 126
  val adderLimit = 94
  val coeffWords = constant.toWords(solverLimit) // segments of the constant, low to high
  val dataInWordWidth = adderLimit - solverLimit // maximum width of adders involved

  // information of segments of dataIn
  val dataWidths = Seq.fill(widthIn)(1).grouped(dataInWordWidth).toSeq.map(_.sum)
  val dataPositions = dataWidths.scan(0)(_ + _).init

  /** --------
   * getting tiles
   * -------- */
  // for each segment of dataIn, an ArrayBuffer is prepared to contain its corresponding coeffWords
  val coeffs = Seq.fill(dataWidths.length)(ArrayBuffer[BigInt]())
  // for all pairs of dataIn segment and coeffWord, its position and width can be assured before instantiation
  val arithInfos = ArrayBuffer[ArithInfo]()

  Seq.tabulate(dataPositions.length, coeffWords.length) { // build a mesh by tabulating
    (i, j) => // traversing grids in the mesh
      // get information on current grid
      val dataPosition = dataPositions(i)
      val dataWidth = dataWidths(i)
      val coeffWord = coeffWords(j)
      // position/width = position/width of data segment + position/width of coeffWord
      val position = dataPosition + j * solverLimit
      val width = dataWidth + coeffWord.bitLength
      val pass = mode match {
        case FULL => true // full multiplication takes all
        case HALFLOW => position < widthTake // LSB multiplication takes the ones on the lower half
        //        case HALFHIGH => position + width >= widthDrop - 1 // MSB multiplication takes the ones that have influence on the higher half
        case HALFHIGH => position + width >= 281 // MSB multiplication takes the ones that have influence on the higher half
      }
      logger.info(s"range: $position->${position + width}, drop $widthDrop, pass: $pass, coeff: $coeffWord")
      if (pass) {
        arithInfos += ArithInfo(width, position)
        coeffs(i) += coeffWord
      }
  }

  coeffs.flatten.zip(arithInfos).foreach { case (coeff, info) => logger.info(s"position: ${info.shift}, width: ${info.width}, coeff: $coeff") }

  // for each segment of dataIn, implement a MCM by the coeffWords allocated
  val adderGraphConfigs = dataWidths.zip(coeffs)
    .map { case (width, coeffForOneSegment) => MultiConstantMultiplicationConfig(coeffForOneSegment, width, PAG) }
  // for all the output from MCMs, sum them up by a BHC
  val compressorConfig = BitHeapCompressorConfig(arithInfos)

  // calculate the latency
  val adderGraphLatency = adderGraphConfigs.map(_.latency).max
  val compressorLatency = compressorConfig.latency

  override def latency: Int = adderGraphLatency + compressorLatency

  override def implH = BigConstantMultiplication(this)

  logger.info(s"data segment number: ${dataWidths.length}, coeff segment number: ${coeffWords.length}, " +
    s"take ${arithInfos.length}/${dataWidths.length * coeffWords.length} pairs for mode $mode, " +
    s"latency: $latency")
}

case class BigConstantMultiplication(config: BigConstantMultiplicationConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits), 1))
  override val dataOut = master Flow Fragment(Vec(UInt(widthOut bits), 1))

  val dataWords = dataIn.fragment.head.asBits.asBools // Bits -> Seq[Bool]
    .grouped(dataInWordWidth).toSeq // get segments
    .map(_.asBits.asUInt) // Seq[Bool] -> UInt

  // stage1: MCM
  val partialProds = dataWords.zip(adderGraphConfigs)
    .flatMap { case (dataWord, config) =>
      config.implH.asNode(Seq(dataWord))
        .map(_.d(adderGraphLatency - config.latency)) // padding the latency
    }.map(_.resized)

  // stage2: BHC
  val rows = compressorConfig.implH.asNode(partialProds)

  // TODO: output should be registered, to show the critical path on final binary adder
  // stage3: CPA
  val ret = rows.reduce(_ +^ _)

  // take
  mode match {
    case FULL => dataOut.fragment.head := ret.resized
    case HALFLOW => dataOut.fragment.head := ret(widthTake - 1 downto 0)
    case HALFHIGH => dataOut.fragment.head := ret(widthAll - 1 downto widthDrop)
  }

  autoValid()
  autoLast()
}
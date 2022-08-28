package org.datenlord
package algos

import arithmetic.Csd
import arithmetic.MultplierMode._

import scala.util.Random

object TruncatedMult {

  /** estimate the error introduced by dropping lower bits of the input
   *
   * @param widthIn
   * @param constant
   * @param widthTake
   */
  def constantMsbError(constant: BigInt, widthIn: Int, widthTake: Int): (BigInt, BigInt, BigInt, BigInt) = {
    val csd = Csd.fromBigInt(constant)
    logger.info(s"csd: $csd")
    val widthOut = widthIn + constant.bitLength
    val widthDrop = widthOut - widthTake

    val widthCoeff = csd.csd.length

    var upper = BigInt(0)
    var lower = BigInt(0)

    var dataForUpper = BigInt(0)
    var dataForLower = BigInt(0)

    (0 until widthIn).foreach { i => // bit by bit(low to high), as they are independent
      val widthDropped = (widthDrop - i) min widthCoeff max 0
      val constantTruncated = csd.takeLow(widthDropped).evaluate // the constant which a bit really multiplied by

      if (constantTruncated >= BigInt(0)) {
        upper += constantTruncated << i
        dataForUpper += BigInt(1) << i
      } else {
        lower += constantTruncated << i
        dataForLower += BigInt(1) << i
      }
    }

    upper = (upper >> widthDrop) + 1
    lower = lower >> widthDrop

    (upper, lower, dataForUpper, dataForLower)
  }

  def main(args: Array[String]): Unit = {
    constantMsbError(algos.ZPrizeMSM.MPrime, 378, 378)
  }
}

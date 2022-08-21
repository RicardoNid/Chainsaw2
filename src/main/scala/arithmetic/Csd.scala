package org.datenlord
package arithmetic

import scala.annotation.tailrec
import scala.math.pow
import scala.util.Random
import algos.ZPrizeMSM._

/** canonic signed digit and corresponding methods
 *
 * @param csd CSD number represented by a string, 9 stands for -1
 */
case class Csd(csd: String) {
  def evaluate: BigInt = csd.reverse.zipWithIndex
    .map { case (digit, i) =>
      digit match {
        case '0' => BigInt(0)
        case '1' => BigInt(1) << i
        case '9' => -(BigInt(1) << i)
      }
    }.sum

  def weight = csd.count(_ != '0') // number of nonzero digits

  override def toString = csd
}

object Csd {

  def getWeight(bigInt: BigInt) = bigInt.toString(2).count(_ != '0')

  // TODO: use optimal csd coding
  def fromBigInt(bigInt: BigInt) = {
    val raw = bigInt.toString(2).reverse + "0" // LSB -> MSB with 0 padded

    val pattern = "11+0".r

    @tailrec
    def process(raw: String): String = {
      pattern.findFirstIn(raw) match {
        case None => raw.reverse
        case Some(x) => process(raw.replaceFirst(x, "9" + "0" * (x.length - 2) + "1"))
      }
    }

    val ret = Csd(process(raw)) // MSB -> LSB
    assert(ret.evaluate == bigInt)
    ret
  }

  def main(args: Array[String]): Unit = {

    println(Csd.fromBigInt(baseModulus))
    println(Csd.getWeight(baseModulus))
    println(Csd.fromBigInt(baseModulus).weight)

  }

}
package org.datenlord
package algos

import scala.math.ceil

object Karatsuba {

  // (aN + b) * (cN + d) = acN^2 + (ad + bc)N + bd

  var multTimes = 0
  var addTimes = 0

  def baseMult(x: BigInt, y: BigInt)(implicit baseWidth: Int): BigInt = {
    require(x.bitLength <= baseWidth && y.bitLength <= baseWidth, s"required width: $baseWidth, while x: ${x.bitLength}, y: ${y.bitLength}")
    multTimes += 1
    x * y
  }

  def karatsuba(width: Int, baseWidth: Int = 18, x: BigInt, y: BigInt): BigInt = {

    val golden = x * y
    val outerN = ceil(width.toDouble / baseWidth).toInt
    implicit val base = baseWidth

    def recursiveTask0(N: Int, x: BigInt, y: BigInt): BigInt = {
      if (N == 1) baseMult(x, y)
      else {
        // split x,y into high & low, according to a multiple of basewidth
        val highN = N / 2
        val lowN = (N + 1) / 2
        val split = lowN * baseWidth
        val (xHigh, xLow) = x.splitAt(split)
        val (yHigh, yLow) = y.splitAt(split)
        // (aN + b) * (cN + d) = acN^2 + (ad + bc)N + bd
        val ac = recursiveTask0(highN, xHigh, yHigh)
        val all = {
          val ab = xHigh + xLow
          val cd = yHigh + yLow
          val (abHigh, abLow) = ab.splitAt(split)
          val (cdHigh, cdLow) = cd.splitAt(split)
          recursiveTask0(lowN, abLow, abHigh) +
            ((abHigh * cdLow + cdHigh * abLow) << split) + (abHigh * cdHigh << (split * 2))
        }
        val bd = recursiveTask0(lowN, xLow, yLow)
        val adbc = all - ac - bd
        (ac << (split * 2)) + (adbc << split) + bd
      }
    }

    def recursiveTask1(width: Int, x: BigInt, y: BigInt): BigInt = {
      if (width <= baseWidth) baseMult(x, y)
      else {
        // split x,y into high & low, according to a multiple of basewidth
        val split = (width + 1) / 2
        val widthNext = split + 1
        val (xHigh, xLow) = x.splitAt(split)
        val (yHigh, yLow) = y.splitAt(split)
        val ac = recursiveTask1(widthNext, xHigh, yHigh)
        val all = recursiveTask1(widthNext, xHigh + xLow, yHigh + yLow)
        val bd = recursiveTask1(widthNext, xLow, yLow)
        val adbc = all - ac - bd
        (ac << (split * 2)) + (adbc << split) + bd
      }
    }

    //    val ret = recursiveTask0(outerN, x, y)
    val ret = recursiveTask1(width, x, y)
    assert(golden == ret, s"\ngolden: $golden, \nyours : $ret")
    println(s"multTimes = $multTimes")
    ret
  }

  def karatusbaLowOnly(width: Int, baseWidth: Int = 18, x: BigInt, y: BigInt) = {
    val golden = (x * y) % (BigInt(1) << width)
    val outerN = ceil(width.toDouble / baseWidth).toInt
    implicit val base = baseWidth

    val split = (outerN - 1) * baseWidth
    val (xHigh, xLow) = x.splitAt(split)
    val (yHigh, yLow) = y.splitAt(split)
    val xLast = x.splitAt(baseWidth)._2
    val yLast = y.splitAt(baseWidth)._2

    val part0 = karatsuba(split, baseWidth, xLow, yLow)
    val part1 = (baseMult(xHigh, yLast) + baseMult(yHigh, xLast)) << split
    val ret = (part0 + part1) % (BigInt(1) << width)

    assert(golden == ret, s"\ngolden: $golden, \nyours : $ret")
    println(s"multTimes = $multTimes")
    ret
  }
}

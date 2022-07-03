package org.datenlord
package algos

import breeze.numerics.ceil

import scala.util.Random

object Karatsuba {

  // (aN + b) * (cN + d) = acN^2 + (ad + bc)N + bd

  var multTimes = 0
  var addTimes = 0

  def getMargin(width: Int, baseWidth: Int) = {
    var current = baseWidth

    def next = (current - 1) * 2

    while (next < width) current = next
    current
  }

  def printAndClear(): Unit = {
    println(s"multTimes = $multTimes, addTimes = $addTimes")
    multTimes = 0
    addTimes = 0
  }

  def baseMult(x: BigInt, y: BigInt)(implicit baseWidth: Int): BigInt = {
    require(x.bitLength <= baseWidth && y.bitLength <= baseWidth, s"required width: $baseWidth, while x: ${x.bitLength}, y: ${y.bitLength}")
    multTimes += 1
    x * y
  }

  def bigAdd(x: BigInt, y: BigInt, plus: Boolean = true)(implicit baseWidth: Int): BigInt = {
    val overlap = x.toString(2).reverse.zip(y.toString(2).reverse).dropWhile(_ == ('0', '0')).length
    addTimes += (overlap + baseWidth - 1) / baseWidth
    if (plus) x + y else x - y
  }

  def bigSub(x: BigInt, y: BigInt)(implicit baseWidth: Int) =
    bigAdd(x, y, plus = false)

  def karatsuba(width: Int, baseWidth: Int, x: BigInt, y: BigInt, mode: String = "mult"): BigInt = {

    val golden = mode match {
      case "low" => (x * y) % (BigInt(1) << width)
      case _ => x * y
    }

    implicit val base: Int = baseWidth

    /**
     * @see
     */
    def recursiveTask(width: Int, x: BigInt, y: BigInt): BigInt = {
      if (width <= baseWidth) baseMult(x, y)
      else {
        val split = (width + 1) / 2
        val widthHigh = width - split
        val widthLow = split
        val widthCorss = widthHigh max widthLow
        val (xHigh, xLow) = x.split(split)
        val (yHigh, yLow) = y.split(split)
        mode match {
          case "mult" =>
            val ac = recursiveTask(widthHigh, xHigh, yHigh)
            val all = recursiveTask(widthCorss + 1, bigAdd(xHigh, xLow), bigAdd(yHigh, yLow))
            val bd = recursiveTask(widthLow, xLow, yLow)
            val adbc = bigSub(bigSub(all, ac), bd)
            bigAdd(bigAdd(ac << (split * 2), adbc << split), bd)
          case "square" =>
            val bd = recursiveTask(widthLow, xLow, xLow)
            val cb = karatsuba(widthCorss, baseWidth, xHigh, xLow)
            val ac = recursiveTask(widthHigh, xHigh, xHigh)
            bigAdd(ac << (split * 2), bigAdd(cb << (split + 1), bd))
          case "low" =>
            val bd = karatsuba(widthLow, baseWidth, xLow, yLow)
            val cb = recursiveTask(widthCorss, yHigh, xLow)
            val ad = recursiveTask(widthCorss, xHigh, yLow)
            bigAdd(bd, bigAdd(cb, ad) << split)
        }
      }
    }

    val ret = mode match {
      case "low" => recursiveTask(width, x, y) % (BigInt(1) << width)
      case _ => recursiveTask(width, x, y)
    }

    assert(golden == ret, s"\ngolden: $golden, \nyours : $ret")
    ret
  }

  def karatsubaLowOnly(width: Int, baseWidth: Int = 18, x: BigInt, y: BigInt) =
    karatsuba(width, baseWidth, x, y, "low")

  def karatsubaSquare(width: Int, baseWidth: Int = 18, x: BigInt): BigInt =
    karatsuba(width, baseWidth, x, x, "square")

  def anotherKaratsuba(width: Int, baseWidth: Int, x: BigInt, y: BigInt, mode: String = "mult") = {

    implicit val base: Int = baseWidth

    def preTask(width: Int, x: Seq[(BigInt, Int)], y: Seq[(BigInt, Int)]): (Seq[(BigInt, Int)], Seq[(BigInt, Int)]) = {
      if (width <= baseWidth) (x, y)
      else {
        val split = (width + 1) / 2
        val widthHigh = width - split
        val widthLow = split
        val widthCorss = widthHigh max widthLow

        val xSplits = x.map(_._1.split(split))
        val xHigh = xSplits.map(_._1)
        val xLow = xSplits.map(_._2)

        val ySplits = y.map(_._1.split(split))
        val yHigh = ySplits.map(_._1)
        val yLow = ySplits.map(_._2)

        mode match {
          case "mult" =>
            val xComb = xHigh.zip(xLow).map { case (h, l) => bigAdd(h, l) }
            val yComb = yHigh.zip(yLow).map { case (h, l) => bigAdd(h, l) }
            val high = preTask(widthHigh, xHigh.map((_, widthHigh)), yHigh.map((_, widthHigh)))
            val cross = preTask(widthCorss + 1, xComb.map((_, widthCorss + 1)), yComb.map((_, widthCorss + 1)))
            val low = preTask(widthLow, xLow.map((_, widthLow)), yLow.map((_, widthLow)))
            (high._1 ++ cross._1 ++ low._1, high._2 ++ cross._2 ++ low._2)
        }
      }
    }

    def postTask(products: Seq[(BigInt, Int)]): Seq[(BigInt, Int)] = {
      if (products.length == 1) products
      else {
        mode match {
          case "mult" =>
            val groups = products.grouped(3).toSeq
            groups.map { three =>
              val Seq(high, corss, low) = three.map(_._1)
              val Seq(widthHigh, widthCross, widthLow) = three.map(_._2)
              val adbc = bigSub(bigSub(corss, high), low)
              val ret = bigAdd(bigAdd(high << (widthLow * 2), adbc << widthLow), low)
              (ret, widthHigh + widthLow)
            }
        }
      }
    }

    val ops = preTask(width, Seq((x, width)), Seq((y, width)))
    val products = ops._1.zip(ops._2).map{ case (x, y) => (baseMult(x._1, y._1), x._2)}
    val ret = postTask(products)
    ret.head._1
  }

  def main(args: Array[String]): Unit = {
    println(Random.nextBigInt(19))
  }
}

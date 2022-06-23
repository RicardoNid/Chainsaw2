package org.datenlord
package algos

object Karatsuba {

  // (aN + b) * (cN + d) = acN^2 + (ad + bc)N + bd

  var multTimes = 0
  var addTimes = 0

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

  def karatsuba(width: Int, baseWidth: Int = 18, x: BigInt, y: BigInt, mode: String = "mult"): BigInt = {

    val golden = mode match {
      case "low" => (x * y) % (BigInt(1) << width)
      case _ => x * y
    }

    implicit val base: Int = baseWidth

    def recursiveTask(width: Int, x: BigInt, y: BigInt): BigInt = {
      if (width <= baseWidth) baseMult(x, y)
      else {
        val split = (width + 1) / 2
        val widthNext = split + 1
        val (xHigh, xLow) = x.splitAt(split)
        val (yHigh, yLow) = y.splitAt(split)
        mode match {
          case "mult" =>
            val ac = recursiveTask(widthNext, xHigh, yHigh)
            val all = recursiveTask(widthNext, bigAdd(xHigh, xLow), bigAdd(yHigh, yLow))
            val bd = recursiveTask(widthNext, xLow, yLow)
            val adbc = bigSub(bigSub(all, ac), bd)
            bigAdd(bigAdd(ac << (split * 2), adbc << split), bd)
          case "square" =>
            val bd = recursiveTask(widthNext, xLow, xLow)
            val cb = karatsuba(widthNext, baseWidth, xHigh, xLow)
            val ac = recursiveTask(widthNext, xHigh, xHigh)
            bigAdd(ac << (split * 2), bigAdd(cb << (split + 1), bd))
          case "low" =>
            val bd = karatsuba(widthNext, baseWidth, xLow, yLow)
            val cb = recursiveTask(widthNext, yHigh, xLow)
            val ad = recursiveTask(widthNext, xHigh, yLow)
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
}

package org.datenlord
package algos

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class KaratsubaTest extends AnyFlatSpec {

  val testCount = 1000
  val width = 256
  val baseWidth = 34

  val xs = (0 until testCount).map(_ => nextBigInt(width))
  val ys = (0 until testCount).map(_ => nextBigInt(width))

  "karatsuba" should "work" in {

    //    xs.zip(ys).foreach { case (x, y) => Karatsuba.karatsuba(width, baseWidth, x, y) }
    xs.zip(ys).foreach { case (x, y) =>
      Karatsuba.karatsuba(width, baseWidth, x, y)
      Karatsuba.multTimes = 0
    }

  }
}

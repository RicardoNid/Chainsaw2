package org.datenlord
package arithmetic

import org.datenlord.arithmetic.MultplierMode.{Full, Low, Square}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class KaratsubaByGraphTest extends AnyFlatSpec {

  val testCount = 1000
  val width = 377
  val data = (0 until testCount * 2).map(_ => Random.nextBigInt(width))
  val xs = data.take(testCount)
  val ys = data.takeRight(testCount)


  val configMult = KaratsubaByGraphConfig(width, Full)
  //  val configLow = KaratsubaByGraphConfig(width, Low)
  //  val configSquare = KaratsubaByGraphConfig(width, Square)

  "Karatsuba " should "work for full multiplication" in TransformTest.test(configMult.implH, data)
}

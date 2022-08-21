package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class CsdTest extends AnyFlatSpec {

  val testCount = 1000
  val testWidth = 377
  val testData = (0 until testCount).map(_ => Random.nextBigInt(testWidth))

  "Csd" should "work correctly" in testData.foreach(Csd.fromBigInt)

  it should "work efficiently" in {
    val weightBefore = testData.map(_.toString(2).count(_ != '0')).sum
    val weightAfter = testData.map(Csd.fromBigInt(_).weight).sum
    logger.info(s"average compression rate = ${weightBefore.toDouble / weightAfter}")
  }

}

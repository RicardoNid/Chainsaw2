package org.datenlord
package arithmetic

import arithmetic.MultplierMode._

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BigConstantMultiplicationAnotherTest extends AnyFlatSpec {

  Random.setSeed(42)

  val dataWidth = 377
  val data = (0 until 1000).map(_ => Random.nextBigInt(dataWidth))

  val smallDataWidth = 16
  val constant = Random.nextBigInt(smallDataWidth)
  val smallData = (0 until 1000).map(_ => Random.nextBigInt(smallDataWidth))

  behavior of "BigConstantMultiplicationAnother"

  val configSmallMSB = BigConstantMultiplicationAnotherConfig(constant, smallDataWidth, HALFHIGH, smallDataWidth, useCsd = true)
  val configSmallLSB = BigConstantMultiplicationAnotherConfig(constant, smallDataWidth, HALFLOW, smallDataWidth, useCsd = true)
  val configSmallFull = BigConstantMultiplicationAnotherConfig(constant, smallDataWidth, FULL, useCsd = true)

  it should "work correctly toy system" in {
    TransformTest.test(configSmallFull.implH, smallData, name = "toy_full")
    TransformTest.test(configSmallMSB.implH, smallData, configSmallMSB.metric, "toy_high")
    TransformTest.test(configSmallLSB.implH, smallData, name = "toy_low")
  }
}

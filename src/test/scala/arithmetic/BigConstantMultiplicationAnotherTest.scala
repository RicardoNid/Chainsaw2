package org.datenlord
package arithmetic

import arithmetic.MultplierMode._

import org.datenlord.algos.ZPrizeMSM.{MPrime, baseModulus}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BigConstantMultiplicationAnotherTest extends AnyFlatSpec {

  // TODO: merge BigConstantMultiplication and BigConstantMultiplicationAnother
  Random.setSeed(42)

  val testCount = 1000
  val smallDataWidth = 16
  val constant = Random.nextBigInt(smallDataWidth)
  val smallData = Seq.fill(testCount)(BigInt(smallDataWidth, Random))

  behavior of "toy system"

  it should "work correctly for FULL mode" in {
    logger.info(s"testing $constant")
    val configSmallFull = BigConstantMultiplicationByCompressorTreeConfig(constant, smallDataWidth, FULL, useCsd = true)
    TransformTest.test(configSmallFull.implH, smallData, name = "toy_full")
  }

  it should s"work correctly for LSB mode" in {
    logger.info(s"testing $constant")
    val configSmallLSB = BigConstantMultiplicationByCompressorTreeConfig(constant, smallDataWidth, LSB, smallDataWidth, useCsd = true)
    TransformTest.test(configSmallLSB.implH, smallData, name = "toy_lsb")
  }

  it should s"work correctly for MSB mode" in {
    (0 until 100).foreach { _ =>
      val constant = Random.nextBigInt(smallDataWidth)
      logger.info(s"testing $constant")
      val config = BigConstantMultiplicationByCompressorTreeConfig(constant, smallDataWidth, MSB, smallDataWidth, useCsd = true)
      TransformTest.test(config.implH, smallData, metric = config.metric, name = "toy_msb")
    }
  }

  val dataWidth = 377
  val data = (0 until 1000).map(_ => Random.nextBigInt(dataWidth))

  behavior of "BLS-377 configurations"

  def config1 = BigConstantMultiplicationByCompressorTreeConfig(MPrime, widthIn = dataWidth + 1, MSB, widthTake = dataWidth + 1, useCsd = true)

  it should "work for BLS-377 modulus under MSB mode" in
    TransformTest.test(config1.implH, data ++ Seq(config1.dataForUpper, config1.dataForLower), config1.metric)

  ignore should "impl for BLS-377 modulus for high-bits" in VivadoImpl(config1.implH, "ZPRIZE_modulus_BCM_high")

  behavior of "low-bits version, for the third multiplication in barrett"

  //  def config2 = BigConstantMultiplicationConfig(baseModulus, widthIn = dataWidth + 1, HALFLOW, widthTake = dataWidth + 2)
  def config2 = BigConstantMultiplicationByCompressorTreeConfig(baseModulus, widthIn = dataWidth + 1, LSB, widthTake = dataWidth + 2, useCsd = true)

  it should "work for BLS-377 modulus for low-bits" in TransformTest.test(config2.implH, data)

  ignore should "impl for BLS-377 modulus for low-bits" in VivadoImpl(config2.implH, "ZPRIZE_modulus_BCM_low")

}

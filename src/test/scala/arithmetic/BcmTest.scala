package org.datenlord
package arithmetic

import algos.ZPrizeMSM.{MPrime, baseModulus}
import arithmetic.MultplierMode._

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BcmTest extends AnyFlatSpec {

  Random.setSeed(42)

  val testCount = 1000
  val smallDataWidth = 16
  val constant = Random.nextBigInt(smallDataWidth)
  val smallData = Seq.fill(testCount)(BigInt(smallDataWidth, Random))

  behavior of "toy system"

  it should "work correctly for FULL mode" in {
    logger.info(s"testing $constant")
    val configSmallFull = BcmConfig(constant, smallDataWidth, FULL, useCsd = true)
    TransformTest.test(configSmallFull.implH, smallData, name = "toy_full")
  }

  it should s"work correctly for LSB mode" in {
    logger.info(s"testing $constant")
    val configSmallLSB = BcmConfig(constant, smallDataWidth, LSB, smallDataWidth, useCsd = true)
    TransformTest.test(configSmallLSB.implH, smallData, name = "toy_lsb")
  }

  it should s"work correctly for MSB mode" in {
    (0 until 100).foreach { _ =>
      val constant = Random.nextBigInt(smallDataWidth)
      logger.info(s"testing $constant")
      val config = BcmConfig(constant, smallDataWidth, MSB, smallDataWidth, useCsd = true)
      TransformTest.test(config.implH, smallData, metric = config.metric, name = "toy_msb")
    }
  }

  val dataWidth = 377
  val data = Seq.fill(testCount)(BigInt(dataWidth, Random))

  behavior of "BLS-377 configurations"

  def configMSB = BcmConfig(MPrime, widthIn = dataWidth + 1, MSB, widthTake = dataWidth + 5, useCsd = true)

  it should "work for BLS-377 modulus under MSB mode" in
    TransformTest.test(configMSB.implH, Seq(configMSB.dataForLower, configMSB.dataForUpper) ++ data, configMSB.metric)

  ignore should "impl for BLS-377 modulus for high-bits" in VivadoImpl(configMSB.implH, "ZPRIZE_modulus_BCM_high")

  behavior of "low-bits version, for the third multiplication in barrett"

  def configLSB = BcmConfig(baseModulus, widthIn = dataWidth + 1, LSB, widthTake = dataWidth + 2, useCsd = true)

  it should "work for BLS-377 modulus for low-bits" in TransformTest.test(configLSB.implH, data)

  ignore should "impl for BLS-377 modulus for low-bits" in VivadoImpl(configLSB.implH, "ZPRIZE_modulus_BCM_low")

}

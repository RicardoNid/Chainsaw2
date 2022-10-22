package org.datenlord
package arithmetic

import ip.pippenger.ZPrizeMSM.{baseModulus, MPrime}

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

/** this test is designed for [[algos.ModularMult.barrett]] application
 *
 */
class BigConstantMultiplicationTest extends AnyFlatSpec {

  //  Random.setSeed(42)

  val dataWidth = 377
  val data = (0 until 1000).map(_ => Random.nextBigInt(dataWidth))


  behavior of "full version"

  //  def config0 = BigConstantMultiplicationConfig(baseModulus, dataWidth, FULL)
  def config0 = BigConstantMultiplicationConfig(MPrime, dataWidth + 1, FullMultiplier)

  it should "work for BLS-377 modulus" in TransformTest.test(config0.implH, data)

  ignore should "impl for BLS-377 modulus" in VivadoImpl(config0.implH, "ZPRIZE_modulus_BCM")

  behavior of "high-bits version, for the second multiplication in barrett"

  //  def config1 = BigConstantMultiplicationConfig(MPrime, widthIn = dataWidth + 1, HALFHIGH, widthTake = dataWidth + 1)
  //  def config1 = BigConstantMultiplicationByCompressorTree(MPrime, widthIn = dataWidth + 1, HALFHIGH, widthTake = dataWidth + 1, useCsd = true)
  // FIXME: wrong when widthTake = dataWidth + 3
  def config1 = BcmConfig(MPrime, widthIn = dataWidth + 1, MsbMultiplier, widthTake = dataWidth + 4, useCsd = true)

  it should "work for BLS-377 modulus for high-bits" in TransformTest.test(config1.implH, config1.dataForUpper +: data, config1.metric)

  it should "impl for BLS-377 modulus for high-bits" in VivadoImpl(config1.implH, "ZPRIZE_modulus_BCM_high")

  behavior of "low-bits version, for the third multiplication in barrett"

  //  def config2 = BigConstantMultiplicationConfig(baseModulus, widthIn = dataWidth + 1, HALFLOW, widthTake = dataWidth + 2)
  def config2 = BcmConfig(baseModulus, widthIn = dataWidth + 1, LsbMultiplier, widthTake = dataWidth + 2, useCsd = true)

  it should "work for BLS-377 modulus for low-bits" in TransformTest.test(config2.implH, data)

  it should "impl for BLS-377 modulus for low-bits" in VivadoImpl(config2.implH, "ZPRIZE_modulus_BCM_low")
}

package org.datenlord
package arithmetic

import algos.ZPrizeMSM.{baseModulus, MPrime}
import arithmetic.MultplierMode._

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

/** this test is designed for [[algos.ModularMult.barrett]] application
 *
 */
class BigConstantMultiplicationTest extends AnyFlatSpec {

  //  Random.setSeed(42)

  val dataWidth = 377
  val data = (0 until 1000).map(_ => Random.nextBigInt(dataWidth))

  def config0 = BigConstantMultiplicationConfig(baseModulus, dataWidth, FULL)

  "Big Constant Multiplier" should "work for BLS-377 modulus" in TransformTest.test(config0.implH, data)

  ignore should "impl for BLS-377 modulus" in VivadoImpl(config0.implH, "ZPRIZE_modulus_BCM")

  behavior of "high-bits version, for the second multiplication in barrett"

  logger.info(s"constant width: ${MPrime.bitLength}")

  def config1 = BigConstantMultiplicationConfig(MPrime, widthIn = dataWidth + 1, HALFHIGH, widthTake = dataWidth + 1)

  it should "work for BLS-377 modulus for high-bits" in TransformTest.test(config1.implH, data, config1.metric)

  it should "impl for BLS-377 modulus for high-bits" in VivadoImpl(config1.implH, "ZPRIZE_modulus_BCM_high")

  behavior of "low-bits version, for the third multiplication in barrett"

  def config2 = BigConstantMultiplicationConfig(baseModulus, widthIn = dataWidth + 1, HALFLOW, widthTake = dataWidth + 2)

  it should "work for BLS-377 modulus for low-bits" in TransformTest.test(config2.implH, data)

  it should "impl for BLS-377 modulus for low-bits" in VivadoImpl(config2.implH, "ZPRIZE_modulus_BCM_low")
}

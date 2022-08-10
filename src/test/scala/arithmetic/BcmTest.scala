package org.datenlord
package arithmetic

import algos.ZPrizeMSM.baseModulus

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BcmTest extends AnyFlatSpec {

  Random.setSeed(42)

  val dataWidth = 377
  val coeffWidth = 63
  val data = (0 until 1000).map(_ => Random.nextBigInt(dataWidth))
  val coeff = Random.nextBigInt(coeffWidth - 1) + (BigInt(1) << (coeffWidth - 1))
  val config = BcmConfig(baseModulus, dataWidth)

  "Big Constant Multiplier" should "work for BLS-377 modulus" in TransformTest.test(config.implH, data)

  it should "synth for BLS-377 modulus" in VivadoSynth(config.implH, "ZPRIZE_modulus_BCM")

}

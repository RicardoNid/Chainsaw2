package org.datenlord
package arithmetic

import algos.ZPrizeMSM.baseModulus

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ConstantBigMultiplierConfigTest extends AnyFlatSpec {

  Random.setSeed(42)

  val dataWidth = 377
  val coeffWidth = 63
  val data = (0 until 1000).map(_ => Random.nextBigInt(dataWidth))
  val coeff = Random.nextBigInt(coeffWidth - 1) + (BigInt(1) << (coeffWidth - 1))

  "Constant Big Multiplier" should "work for BLS-377 modulus" in {
    val config = ConstantBigMultiplierConfig(baseModulus, dataWidth)
    //    val config = ConstantBigMultiplierConfig(coeff, dataWidth)
    //    TransformTest.test(config.implH, data, name = "CBM")
    VivadoSynth(config.implH, name = "CBM")
  }

}

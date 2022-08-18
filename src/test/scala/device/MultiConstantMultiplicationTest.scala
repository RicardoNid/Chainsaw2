package org.datenlord
package device

import algos.ZPrizeMSM.{NPrime, baseModulus}
import arithmetic.MultiConstantMultiplicationConfig
import arithmetic.McmType._

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class MultiConstantMultiplicationTest extends AnyFlatSpec {

  val dataWidth = 126 - 31
  val config0 = MultiConstantMultiplicationConfig(baseModulus.toWords(31), dataWidth, SPIRAL)
  val config1 = MultiConstantMultiplicationConfig(baseModulus.toWords(31), dataWidth, PAG)
  val config2 = MultiConstantMultiplicationConfig(NPrime.toWords(31), dataWidth, SPIRAL)
  val config3 = MultiConstantMultiplicationConfig(NPrime.toWords(31), dataWidth, PAG)

  val data = (0 until 10000).map(_ => Random.nextBigInt(dataWidth))

  "MCM" should "work for BLS-377 modulus" in {
    TransformTest.test(config0.implH, data, name = "SpiralMcmBase")
    TransformTest.test(config1.implH, data, name = "PagMcmBase")
    TransformTest.test(config2.implH, data, name = "SpiralMcmPrime")
    TransformTest.test(config3.implH, data, name = "PagMcmPrime")
  }

  ignore should "synth for BLS-377 modulus" in {
    VivadoSynth(config0.implH, "SpiralMcmBase")
    VivadoSynth(config1.implH, "PagMcmBase")
    VivadoSynth(config2.implH, "SpiralMcmPrime")
    VivadoSynth(config3.implH, "PagMcmPrime")
  }

  it should "generate a kernel of 16 neighbors" in {
    val coeffs = (0 until 16).map(_ => Random.nextBigInt(8))
    val config = MultiConstantMultiplicationConfig(coeffs, 8, PAG)
    val data8 = (0 until 1000).map(_ => Random.nextBigInt(8))
    TransformTest.test(config.implH, data8, name = "example")
    VivadoSynth(config.implH)
  }

  it should "show" in println((algos.ZPrizeMSM.baseModulus - 1).toString(2).reverse.takeWhile(_ == '0').size)

}

package org.datenlord
package device

import algos.ZPrizeMSM.{NPrime, baseModulus}
import arithmetic.McmConfig
import arithmetic.McmType._

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class McmTest extends AnyFlatSpec {

  val dataWidth = 126 - 31
  val config0 = McmConfig(baseModulus.toWords(31), dataWidth, SPIRAL)
  val config1 = McmConfig(baseModulus.toWords(31), dataWidth, PAG)
  val config2 = McmConfig(NPrime.toWords(31), dataWidth, SPIRAL)
  val config3 = McmConfig(NPrime.toWords(31), dataWidth, PAG)

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
}

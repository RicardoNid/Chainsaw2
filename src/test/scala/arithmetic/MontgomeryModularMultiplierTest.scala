package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class MontgomeryModularMultiplierTest extends AnyFlatSpec {

  val modulus = algos.ZPrizeMSM.baseModulus
  val width = modulus.bitLength
  val data = (0 until 10000).map(_ => Random.nextBigInt(width) % modulus)
  val config0 = ModularMultConfig(modulus)
  val config1 = ModularMultConfig(modulus, square = true)

  // TODO: some case will failed when input < R(rather than < modulus)
  "ModularMult" should "work" in TransformTest.test(config0.implH, data, name = "testModularMult")
  it should "work for squaring" in TransformTest.test(config1.implH, data, name = "testModularSquare")

  it should "synth" in VivadoSynth(config0.implH, name = "modularMult")
  it should "synth for squaring" in VivadoSynth(config1.implH, name = "modularSquare")

}

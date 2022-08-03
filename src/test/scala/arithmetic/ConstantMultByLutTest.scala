package org.datenlord
package arithmetic

import algos.ZPrizeMSM.{NPrime, baseModulus}

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ConstantMultByLutTest extends AnyFlatSpec {

  val testCount = 1000
  val constants0 = baseModulus.toWords(31)
  val constants1 = NPrime.toWords(31)
  val testData = (0 until testCount * constants0.length).map(_ => Random.nextBigInt(127 - 32) % baseModulus)
  val config0 = Mcm(constants0, 127 - 32)
  val config1 = Mcm(constants1, 127 - 32)

  "ConstantMultByLut" should "work" in {
    TransformTest.test(config0.implH, testData)
    TransformTest.test(config1.implH, testData)
  }

  it should "outperform naive implementation" in {

    val report0 = VivadoSynth(config0.implH, "baseModulusBySpiral")
    val report1 = VivadoSynth(config0.naiveImplH, "baseModulusByVivado")

    println(report0)
    println(report1)
    println(report0.util / report1.util)
  }
}

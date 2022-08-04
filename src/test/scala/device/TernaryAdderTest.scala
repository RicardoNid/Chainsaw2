package org.datenlord
package device

import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

class TernaryAdderTest extends AnyFlatSpec {

  val testWidth = 126
  val testCount = 1000
  val testDataRaw = (0 until testCount * 3).map(_ => Random.nextBigInt(testWidth - 2))
  val testData = testDataRaw.grouped(3).toSeq.flatMap(group => Seq(group.sum, group(0), group(1)))
  val configs = (0 until 3).map(TernaryAdderConfig(testWidth, _))

  "ternary adder" should "work for all configurations" in configs.foreach(config => TransformTest.test(config.implH, testData))

  val utilRequirement = VivadoUtilRequirement(lut = testWidth + 1, carry8 = testWidth + 2 / 8)
  val fmaxRequirement = 800 MHz

  ignore should "synth for all configurations" in configs.foreach(config =>
    VivadoSynth(config.implH).require(utilRequirement, fmaxRequirement))

}

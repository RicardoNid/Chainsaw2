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
  val configs0 = (0 until 3).map(TernaryAdderConfig(testWidth, _))
  val configs1 = (0 until 3).map(TernaryAdderSignedConfig(testWidth+1, _))

  "ternary adder" should "work for all unsigned configurations" in configs0.foreach(config => TransformTest.test(config.implH, testData))
  it should "work for all signed configurations" in configs1.foreach(config =>
    TransformTest.test(config.implH, testData.zipWithIndex.map { case (int, i) => if (i % 2 == 0) int else -int }))

  val utilRequirement = VivadoUtilRequirement(lut = testWidth + 1, carry8 = testWidth + 2 / 8) // for unsigned
  val fmaxRequirement = 800 MHz

  ignore should "synth for all unsigned configurations" in configs0.foreach(config =>
    VivadoSynth(config.implH).require(utilRequirement, fmaxRequirement))

  ignore should "synth for all signed configurations" in configs1.foreach(config =>
    VivadoSynth(config.implH))

}

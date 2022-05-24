package org.datenlord

import flowConverters.PermutationByRamConfig

import breeze.math.Complex
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random

class TransformSystemTest extends AnyFlatSpec {

  val uintType = HardType(UInt(4 bits))
  val complexType = HardType(ComplexFix(5 exp, -14 exp))

  val perm = PermutationByRamConfig(Seq(0, 3, 1, 2), 1, uintType)
  val lut = arithmetic.LUTConfig((0 until 16).map(Complex(_, 0)), complexType)

  val system = (lut ⊗ 4) ° perm

  "system" should "work" in {
    val data = system.getRandomDataIn(() => Random.nextBigInt(4))
    system.implStageByStage(data.take(4))
    TransformTest.test(system.implForTest(uintType, complexType), data)
  }

  "system folded" should "work" in {
    val systemFolded = system.fitTo(0.5)
    val dataFolded = systemFolded.getRandomDataIn(() => Random.nextBigInt(4))
    systemFolded.implStageByStage(dataFolded.take(4))
    TransformTest.test(systemFolded.implForTest(uintType, complexType), dataFolded)
  }
}

package org.datenlord
package flowConverters

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random

class FlowConversionTest extends AnyFlatSpec {

  val config: TransformBase = TransformConfigForTest((2, 2), 1)
  val repeat = Repetition(Seq(SpaceRepetition(4)), TimeRepetition(2))

  val reuse0 = Reuse(2, 1, 1, 1)
  val reuse1 = Reuse(1, 1, 2, 1)
  val reuse2 = Reuse(2, 2, 1, 1)
  val reuse3 = Reuse(4, 1, 1, 1)

  val formatA = MeshFormat(config, repeat, reuse0)
  val formatB = MeshFormat(config, repeat, reuse1)
  val formatC = MeshFormat(config, repeat, reuse2)
  val formatD = MeshFormat(config, repeat, reuse3)

  val dataType = HardType(UInt(4 bits))

  "flow converter based on forward register allocation" should "work" in {
    println(formatC.outputFlow)
    println(formatD.inputFlow)
    val config = FlowConversion(formatC, formatD).getConverterConfig(dataType)
    assert(config.isInstanceOf[ForwardRegisterConverterConfig[UInt]])
    TransformTest.test(config.implH, config.getRandomDataIn(() => Random.nextBigInt(4)))
  }

  "flow converter based on RAM-based permutation" should "work" in {
    println(formatA.outputFlow)
    println(formatB.inputFlow)
    val config = FlowConversion(formatA, formatB).getConverterConfig(dataType)
    assert(config.isInstanceOf[PermutationByRamConfig[UInt]])
    TransformTest.test(config.implH, config.getRandomDataIn(() => Random.nextBigInt(4)))
  }
}

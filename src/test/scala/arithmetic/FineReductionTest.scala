package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FineReductionTest extends AnyFlatSpec {

  val testCount = 10000
  val upperBound = 10
  val M = algos.ZPrizeMSM.baseModulus

  val config = FineReduction(M, upperBound)

  logger.info(s"modulus: $M")

  val data = Seq.fill(testCount)(BigInt(config.widthIn, Random)).filter(_ <= upperBound * M)

  "fine reduction module" should "work" in TransformTest.test(config.implH, data)

  it should "impl" in VivadoImpl(config.implH, "reduction377")

}

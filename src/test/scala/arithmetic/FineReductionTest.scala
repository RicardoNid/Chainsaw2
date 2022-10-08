package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FineReductionTest extends AnyFlatSpec {

  val testCount = 10000
  val upperBounds = 2 to 10
  val M = algos.ZPrizeMSM.baseModulus

  def testFineReduction(upperBound: Int) = {
    val config = FineReduction(M, upperBound)
    val data = Seq.fill(testCount)(BigInt(config.widthIn, Random)).filter(_ <= upperBound * M)
    TransformTest.test(config.implH, data)
  }

  "fine reduction module" should "work" in upperBounds.foreach(testFineReduction)

  it should "impl" in VivadoImpl(FineReduction(M, 10).implH, "reduction377")

}

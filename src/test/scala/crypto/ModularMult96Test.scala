package org.datenlord
package crypto

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ModularMult96Test extends AnyFlatSpec {

  val gen = ModularMult96
  gen.setAsNaive()

  "montMult96" should "work" in ChainsawTest.test(
    gen = gen,
    data = Seq.fill(100)(BigInt(96, Random)),
    testName = "testMont96"
  )

  val permGen = Permutation(5, 5, 4, 8)
  permGen.setAsNaive()

  "intrlv" should "work" in ChainsawTest.test(
    gen = permGen,
    data = Seq.fill(16 * 1024)(BigInt(8, Random)),
    testName = "testPerm"
  )

}

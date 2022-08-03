package org.datenlord
package device

import org.scalatest.flatspec.AnyFlatSpec
import algos.ZPrizeMSM.baseModulus

import org.datenlord.arithmetic.Mcm

import scala.util.Random

class McmTest extends AnyFlatSpec {

  val dataWidth = 126
  val coeffWidth = 377
  val data = (0 until 1000).map(_ => Random.nextBigInt(dataWidth))

  "MCM" should "work for BLS-377 modulus" in {
    val config = Mcm(baseModulus.toWords(31), 126)
    TransformTest.test(config.implH, data, name = "MCM")
  }

}

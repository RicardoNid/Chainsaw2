package org.datenlord
package arithmetic

import ip.pippenger.{Barrett377, ZPrizeMSM}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class Barrett377Test extends AnyFlatSpec {

  val k = 377
  val M = ZPrizeMSM.baseModulus
  val testCaseCount = 1000
  val data = (0 until testCaseCount * 2).map(_ => Random.nextBigInt(k))

  //  val metric = (yours: Seq[BigInt], golden:Seq[BigInt]) => {
  //    val quotient = (golden.head - yours.head) / M
  //    val remainder = yours.head % M
  //    val trueRemainder = golden.head
  //    logger.info(s"\ntrue remainder: $trueRemainder\nyour remainder: $remainder\ndiff: $quotient")
  //    true
  //  }

  val graph = Barrett377(k, M)

  "barrett graph" should "gen" in RtlGen(Barrett377(k, M).toTransform)

  it should "work" in TransformTest.test(Barrett377(k, M).toTransform, data, name = "barrettModularMultiplier")

  it should "impl" in VivadoImpl(Barrett377(k, M).toTransform, "barrettModularMultiplier")

}

package org.datenlord
package arithmetic

import ip.pippenger.ZPrizeMSM
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class CsdTest extends AnyFlatSpec {

  val testCount = 10000
  val testWidth = 377
  val testData = Seq.fill(testCount)(BigInt(testWidth, Random))

  "Csd" should "work correctly" in testData.foreach( data => Csd(data))

  it should "work efficiently" in {
    val weightBefore = testData.map(_.toString(2).count(_ != '0')).sum
    val weightAfter = testData.map(data => Csd(data).weight).sum
    logger.info(s"average compression rate = ${weightBefore.toDouble / weightAfter}")
  }

  it should "work for BLS-377" in {

    val modulusBefore = ZPrizeMSM.baseModulus.toString(2).count(_ != '0')
    val modulusAfter = Csd(ZPrizeMSM.baseModulus).weight
    val mPrimeBefore = ZPrizeMSM.MPrime.toString(2).count(_ != '0')
    val mPrimeAfter = Csd(ZPrizeMSM.MPrime).weight

    logger.info(s"modulus: $modulusBefore->$modulusAfter, mprime: $mPrimeBefore->$mPrimeAfter")
  }
}

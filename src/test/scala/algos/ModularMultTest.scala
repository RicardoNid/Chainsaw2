package org.datenlord
package algos

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class ModularMultTest extends AnyFlatSpec {

  Random.setSeed(41)

  val lN = 377
  val testCount = 1000
  val moduli = ZPrizeMSM.baseModulus

  val as = (0 until testCount).map(_ => Random.nextBigInt(lN) % moduli)
  val bs = (0 until testCount).map(_ => Random.nextBigInt(lN) % moduli)

  "mmm algo" should "work" in {
    as.zip(bs).foreach { case (a, b) => ModularMult.mmm(a, b, moduli, lN) }
  }

  "barrett algo" should "work" in {
    val bound0 = as.zip(bs).map { case (a, b) => ModularMult.barrett(a, b, moduli, lN) }.max
    logger.info(s"no-error passed with bound $bound0")
    val bound1 = as.zip(bs).map { case (a, b) => ModularMult.barrett(a, b, moduli, lN, msbOnly = true)}.max
    logger.info(s"error passed with bound: $bound1")
  }

  "hrmmm algo" should "work" in {
    as.zip(bs).foreach { case (a, b) => ModularMult.hrmmm(a, b, moduli, lN, 16) }
  }
}
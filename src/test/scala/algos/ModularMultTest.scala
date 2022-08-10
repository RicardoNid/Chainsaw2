package org.datenlord
package algos

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class ModularMultTest extends AnyFlatSpec {

  Random.setSeed(41)

  val lN = 8
  val testCount = 1000
  val moduli = Random.nextBigInt(lN - 1) + (BigInt(1) << (lN - 1))

  val as = (0 until testCount).map(_ => Random.nextBigInt(lN) % moduli)
  val bs = (0 until testCount).map(_ => Random.nextBigInt(lN) % moduli)

  "mmm algo" should "work" in {
    as.zip(bs).foreach { case (a, b) => ModularMult.mmm(a, b, moduli, lN) }
  }

  "barrett algo" should "work" in {
    as.zip(bs).foreach { case (a, b) => ModularMult.barrett(a, b, moduli, lN)}
  }

  "hrmmm algo" should "work" in {
    as.zip(bs).foreach { case (a, b) => ModularMult.hrmmm(a, b, moduli, lN, 16) }
  }
}
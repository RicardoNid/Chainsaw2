package org.datenlord
package algos

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class MontMultTest extends AnyFlatSpec {

  val lN = 256
  val testCount = 1000
  val moduli = BigInt("52435875175126190479447740508185965837690552500527637822603658699938581184513", 10)

  val as = BigInt(Random.nextString(lN).map(_ % 2).mkString(""), 2)
  val bs = BigInt(Random.nextString(lN).map(_ % 2).mkString(""), 2)

  "mmm algo" should "work" in {

    MontMult.mmm(as, bs, moduli)

  }

}

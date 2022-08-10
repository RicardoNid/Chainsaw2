package org.datenlord


import org.scalatest.flatspec.AnyFlatSpec

import scala.language.postfixOps
import scala.util.Random

class UtilTest extends AnyFlatSpec {

  val testCount = 10000
  val data = (0 until testCount).map { _ =>
    val sign = Random.nextInt(2) * 2 - 1 // (1,-1)
    sign * Random.nextBigInt(100)
  }

  "BigInt Utils" should "convert BigInt from and to 2s complement correctly" in
    data.foreach { bi =>
      val string = bi.to2sComplement
      val ret = bi.from2sComplement(string)
      assert(bi == ret, s"golden: $bi, yours: $ret")
    }

  it should "split signed BigInt correctly" in data.foreach { bi =>
    val split = 50
    val (high, low) = bi.split(split)
    val ret = (high << split) + low
    assert(ret == bi, s"golden: $bi, yours: $ret")
  }
}

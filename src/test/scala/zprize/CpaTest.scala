package org.datenlord
package zprize

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class CpaTest extends AnyFlatSpec {

  "cpa" should "work under 4 different pipeline modes" in {
    val m2s = Cpa(BinaryAdder, 0, Seq.fill(4)(10), M2S)
    val s2s = Cpa(BinaryAdder, 0, Seq.fill(4)(10), S2S)
    val s2m = Cpa(BinaryAdder, 0, Seq.fill(4)(10), S2M)
    val m2m = Cpa(BinaryAdder, 0, Seq.fill(4)(10), M2M)

    val data = Seq.fill(1000)(BigInt(10, Random))
    ChainsawTest.test(s2s, data)
    ChainsawTest.test(s2m, data)
    ChainsawTest.test(m2s, data)
    ChainsawTest.test(m2m, data)
  }
}

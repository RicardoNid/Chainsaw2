package org.datenlord
package dataFlow

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class StreamPermutationTest extends AnyFlatSpec {

  "StreamPermutationTest" should "work" in {

    val n = 12
    val w = 4
    val data = (0 until n).toList
    val perm = Seq(5, 3, 0, 4, 10, 8, 7, 2, 1, 11, 9, 6)

    val config = StreamPermutationConfig(perm, w, 5)
    TransformTest.bitAccurateTest(StreamPermutation(config), (data ++ data).map(BigInt(_)))
  }

}

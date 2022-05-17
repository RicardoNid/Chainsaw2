package org.datenlord
package flowConverters

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class StreamPermutationTest extends AnyFlatSpec {

  "StreamPermutationTest" should "work" in {

    val n = 12
    val w = 4
    val data = (0 until n).toList
    val perms = (0 until 5).map(_ => Random.shuffle(data))

    perms.foreach { perm =>
      val config = StreamPermutationConfig(perm, w, 5)
      TransformTest.test(StreamPermutation(config), (data ++ data).map(BigInt(_)))
    }
  }

}

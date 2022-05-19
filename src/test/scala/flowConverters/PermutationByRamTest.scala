package org.datenlord
package flowConverters

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class PermutationByRamTest extends AnyFlatSpec {

  "StreamPermutationTest" should "work for a simple case" in {
    val n = 4
    val data = (0 until n).toList
    val perm = Random.shuffle(data)
    val config = PermutationByRamConfig(perm, 2, 4)
    TransformTest.test(config.implH, (data ++ data).map(BigInt(_)))
  }

  "StreamPermutationTest" should "work for all folds" in {
    /*
      TODO: this failed because:
        1. decomposition algo is not efficient, even for 12 * 12 mapping matrix
        2. current implementation of Benes Network doesn't support arbitrary size permutation
   */
    val n = 12
    val data = (0 until n).toList
    val perm = Random.shuffle(data)
    val config = PermutationByRamConfig(perm, 1, 4)
    TransformTest.testAllFolds(config, (data ++ data ++ data).map(BigInt(_)))
  }


}

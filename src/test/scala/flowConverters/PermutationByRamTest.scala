package org.datenlord
package flowConverters

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random

class PermutationByRamTest extends AnyFlatSpec {

  val dataType = HardType(UInt(16 bits))

  "StreamPermutationTest" should "work for a simple case" in {
    val n = 4
    val data = (0 until n).toList
    val perm = Random.shuffle(data)
    val config = PermutationByRamConfig(perm, 2, dataType)
    TransformTest.test(config.implH, (data ++ data).map(BigInt(_)))
  }

  it should "work for large size" in {
    val col = 7
    val row = 8
    val fold = 8
    val data = 0 until row * col
    val perm = Seq.tabulate(col, row)((i, j) => i * row + j).transpose.flatten
    val config = PermutationByRamConfig(perm, fold, dataType)
    TransformTest.test(config.implH, (data ++ data).map(BigInt(_)))
  }

  "StreamPermutationTest" should "work for all folds" in {

    val n = 108
    val data = (0 until n).toList
    val perm = Random.shuffle(data)
    logger.info(s"your perm = ${perm.mkString(" ")}")
    val config = PermutationByRamConfig(perm, 3, dataType)

    /*
  TODO: this failed because:
    1. decomposition algo is not efficient, even for 12 * 12 mapping matrix
    2. current implementation of Benes Network doesn't support arbitrary size permutation
*/
    TransformTest.test(config.implH, (data ++ data ++ data).map(BigInt(_)))
    //    TransformTest.testAllFolds(config, (data ++ data ++ data).map(BigInt(_)))
  }


}

package org.datenlord
package flowConverters

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class PermutationByRamAnotherTest extends AnyFlatSpec {

  behavior of "permutation by ram"


  "StreamPermutationTest" should "work for a simple case" in {
    val n = 8
    val data = (0 until n).toList
    val perm = Permutation(Random.shuffle(data))
    val dut = PermutationByRamAnother(perm, 4, 4)
    ChainsawTest.test(dut, (data ++ data).map(BigInt(_)), testName = "permByRam4_2")
  }

  it should "work for a larger case" in {
    val n = 64
    val data = (0 until n).toList
    val perm = Permutation(Random.shuffle(data))
    val dut = PermutationByRamAnother(perm, 8, 8)
    ChainsawTest.test(dut, (data ++ data).map(BigInt(_)), testName = "permByRam4_2")
  }

  it should "work for an even larger case" in {
    val n = 4096
    val data = (0 until n).toList
    val perm = Permutation(Random.shuffle(data))
    val dut = PermutationByRamAnother(perm, 64, 20)
    ChainsawTest.test(dut, (data ++ data).map(BigInt(_)), testName = "permByRam4_2")
  }

}

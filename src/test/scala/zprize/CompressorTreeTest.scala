package org.datenlord
package zprize

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class CompressorTreeTest extends AnyFlatSpec {

  behavior of "compressor tree"

  //  Random.setSeed(42)

  it should "have a correct naive model" in {
    val operands = Seq( // a testcase including weighted, signed and delayed input
      OperandInfo(16, 0, positive = true, 0),
      OperandInfo(16, 5, positive = true, 0),
      OperandInfo(16, 9, positive = true, 0),
      OperandInfo(16, 12, positive = false, 0),
      OperandInfo(16, 15, positive = true, 0)
    )
    verbose = 1
    val compressorTree = CompressorTree(operands)
    //        compressorTree.setAsNaive()
    val data = Seq.fill(400)(BigInt(16, Random))

    ChainsawTest.test(compressorTree,
      data,
      metric = ChainsawMetric.carrySaveMetric(compressorTree.compensation),
      testName = "testCompressorTreeNaive")

    //    ChainsawImpl(compressorTree, withRequirement = true)
  }


}

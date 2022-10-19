package org.datenlord
package zprize

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class CompressorTreeTest extends AnyFlatSpec {

  behavior of "compressor tree"

  it should "have a correct naive model" in {
    val operands = Seq( // a testcase including weighted, signed and delayed input
      OperandInfo(10, 1, positive = true, 0),
      OperandInfo(8, 2, positive = true, 1),
      OperandInfo(9, 0, positive = false, 2),
      OperandInfo(11, 1, positive = true, 3)
    )
    val compressorTree = CompressorTree(operands)
        compressorTree.setAsNaive()
    val data = Seq.fill(400)(BigInt(8, Random))

    ChainsawTest.test(compressorTree,
      data,
      metric = ChainsawMetric.carrySaveMetric,
      testName = "testCompressorTreeNaive")

    //    ChainsawImpl(compressorTree, withRequirement = true)
  }


}

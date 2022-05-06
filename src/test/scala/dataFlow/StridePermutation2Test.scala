package org.datenlord
package dataFlow

import org.scalatest.flatspec.AnyFlatSpec

class StridePermutation2Test extends AnyFlatSpec {


  "MTN" should "work" in {
    val widths = Seq(1, 2, 3)
    val configs = widths.map(width => MTNConfig(width, 2 * width))
    configs.foreach { config =>
      val data = (0 until config.N).map(BigInt(_))
      TransformTest.testTransformModule(MTN(config), Seq.fill(4)(data).flatten)
      logger.info(s"test on ${config.N}-point MTN, passed")
    }
  }

  "SPN" should "work" in {

    val config = SPNConfig(2, 1, 2)
    val data = (0 until config.N).map(BigInt(_))
    TransformTest.testTransformModule(SPN(config), Seq.fill(4)(data).flatten)
    logger.info(s"test on ${config.N}-point SPN, passed")

  }

  "StridePermutation2" should "work" in {
    println("case 1")
    val config = StridePermutation2Config(5, 4, 2, 5)
    val data = (0 until config.N).map(BigInt(_))
    TransformTest.testTransformModule(StridePermutation2(config), Seq.fill(4)(data).flatten)
  }

}

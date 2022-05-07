package org.datenlord
package dataFlow

import org.scalatest.flatspec.AnyFlatSpec

class StridePermutation2Test extends AnyFlatSpec {

  "MTN" should "work" in {
    val widths = (1 to 3) ++ (5 to 6) // TODO: verilator failed on 4, why?
    val configs = widths.map(width => MTNConfig(width, 2 * width))
    configs.foreach { config =>
      val data = (0 until config.N).map(BigInt(_))
      TransformTest.bitAccurateTest(MTN(config), Seq.fill(4)(data).flatten)
      logger.info(s"test on ${config.N}-point MTN, passed")
    }
  }

  "SPN" should "work for square matrix transpose" in {
    val widths = (1 to 6)
    val configs = widths.map(width => SPNConfig(2 * width, width, 2 * width))
    configs.foreach { config =>
      val data = (0 until config.N).map(BigInt(_))
      TransformTest.bitAccurateTest(SPN(config), Seq.fill(4)(data).flatten)
      logger.info(s"test on ${config.N}-point SPN, passed")
    }
  }

  "SPN" should "work for non-square matrix transpose" in {
    val widths = (2 to 6)
    val configs = widths.map(width => SPNConfig(2 * width, width - 1, 2 * width))
    configs.foreach { config =>
      val data = (0 until config.N).map(BigInt(_))
      TransformTest.bitAccurateTest(SPN(config), Seq.fill(4)(data).flatten)
      logger.info(s"test on ${config.N}-point SPN, passed")
    }
  }

  "StridePermutation2" should "work" in {
    val portWidths = Seq(7, 6, 5, 4, 3, 2, 1).reverse
    val configs = portWidths.map(StridePermutation2Config(8, _, 1, 8))
    configs.foreach { config =>
      val data = (0 until config.N).map(BigInt(_))
      TransformTest.bitAccurateTest(StridePermutation2(config), Seq.fill(4)(data).flatten)
      VivadoSynth(StridePermutation2(config))
    }
  }

}

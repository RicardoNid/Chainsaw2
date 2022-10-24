package org.datenlord
package dsp

import org.datenlord.{ChainsawMetric, ChainsawTest, ComplexFixInfo, VivadoImpl}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class TwiddleTest extends AnyFlatSpec {

  val data = Seq.fill(100)(Random.nextComplex())

  "twiddle factor multiplier" should "work for all special/common cases" in (0 until 16).foreach { i =>
    ChainsawTest.test(Twiddle(16, i, ComplexFixInfo(2, 13), 16),
      data,
      metric = ChainsawMetric.complexAbs(1e-2)
    )
  }

  it should "impl for all special/common cases" in (4 to 6).foreach { i =>
    // 4 for case0, 6 for case1, 5 for case2(common case)
    VivadoImpl(Twiddle(16, i, ComplexFixInfo(2, 13), 16).implH)
  }
}

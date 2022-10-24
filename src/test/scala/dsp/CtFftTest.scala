package org.datenlord
package dsp

import org.datenlord.{ChainsawImpl, ChainsawMetric, ChainsawTest, ComplexFixInfo, logger}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class CtFftTest extends AnyFlatSpec {

  val dataType = ComplexFixInfo(5, 12)
  val coeffWidth = 16

  // TODO: more testcases for different factors/scales

  "ctFftCore" should "work for forward direction" in {
    val N = 16
    val factors = Seq(4, 4)
    val scales = Seq(1, 1)
    val data = Seq.fill(N * 100)(Random.nextComplex())
    val gen = CtFftCore(N, inverse = false, dataType, coeffWidth, factors, scales)
    ChainsawTest.test(gen,
      data,
      metric = ChainsawMetric.fftByMean(1e-2)
    )
  }

  it should "work for inverse direction" in {
    val N = 64
    val factors = Seq(8, 8)
    val scales = Seq(2, 1)
    val data = Seq.fill(N * 100)(Random.nextComplex())
    val gen = CtFftCore(N, inverse = true, dataType, coeffWidth, factors, scales)
    ChainsawTest.test(gen,
      data,
      metric = ChainsawMetric.fftByMean(1e-2)
    )
  }

  it should "impl" in {
    val N = 64
    val factors = Seq(8, 8)
    val scales = Seq(2, 1)
    val gen = CtFftCore(N, inverse = true, dataType, coeffWidth, factors, scales)
    ChainsawImpl(gen, name = "ctfft64")
  }

  "ctFft" should "work for FTN" in {
    val N = 512
    val pF = 64
    val factors = Seq(8, 8, 8)
    val scales = Seq(2, 2, 1)
    val data = Seq.fill(N * 10)(Random.nextComplex())
    val gen0 = CtFft(N, inverse = false, dataType, coeffWidth, factors, scales, pF)
    val gen1 = CtFft(N, inverse = true, dataType, coeffWidth, factors, scales, pF)
    logger.info(s"latency: ${gen0.latency}")
    logger.info(s"latency: ${gen1.latency}")
    ChainsawTest.test(gen0, data, metric = ChainsawMetric.fftByMean(1e-2))
    ChainsawTest.test(gen1, data, metric = ChainsawMetric.fftByMean(1e-2))
  }

  it should "impl for FTN" in {
    val N = 512
    val pF = 64
    val factors = Seq(8, 8, 8)
    val scales = Seq(2, 2, 1)
    val gen0 = CtFft(N, inverse = false, dataType, coeffWidth, factors, scales, pF)
    ChainsawImpl(gen0, name = "ctfft64")
  }

}

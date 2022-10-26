package org.datenlord
package dsp

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.stats.mean
import org.datenlord.{ChainsawImpl, ChainsawMetric, ChainsawTest, ComplexFixInfo, logger}
import org.scalatest.flatspec.AnyFlatSpec
import ChainsawMetric._

import scala.util.Random

class CtFftTest extends AnyFlatSpec {

  val dataType = ComplexFixInfo(5, 12)
  val coeffWidth = 16

  def fftByMean(epsilon: Double) = ChainsawMetric(
    elementWise = complexBound(epsilon),
    frameWise = (yours: Seq[Any], golden: Seq[Any]) => {
      val yourV = new DenseVector(yours.tail.map(_.asInstanceOf[Complex]).toArray) // leave DC part alone
      val goldenV = new DenseVector(golden.tail.map(_.asInstanceOf[Complex]).toArray)
      val errorV = yourV - goldenV
      val pass = mean(abs(errorV)) < epsilon
      logger.info(s"errorMax = ${max(abs(errorV))}, errorMean = ${mean(abs(errorV))}")
      pass
    }
  )

  // TODO: more testcases for different factors/scales

  "ctFftCore" should "work for forward direction" in {
    val N = 16
    val factors = Seq(4, 4)
    val scales = Seq(1, 1)
    val data = Seq.fill(N * 100)(Random.nextComplex())
    val gen = CtFftCore(N, inverse = false, dataType, coeffWidth, factors, scales)
    ChainsawTest.test(gen,
      data,
      metric = fftByMean(1e-2)
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
      metric = fftByMean(1e-2)
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
    ChainsawTest.test(gen0, data, metric = fftByMean(1e-2))
    ChainsawTest.test(gen1, data, metric = fftByMean(1e-2))
  }

  it should "synth for FTN" in {
    val N = 512
    val pF = 64
    val factors = Seq(8, 8, 8)
    val scales = Seq(2, 2, 1)
    val gen0 = CtFft(N, inverse = false, dataType, coeffWidth, factors, scales, pF)
    ChainsawSynth(gen0, name = "ctfft64")
  }

}

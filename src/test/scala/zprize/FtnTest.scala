package org.datenlord
package zprize

import breeze.math.Complex
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FtnTest extends AnyFlatSpec {

  //  symbolFrameFormat.generateWaveform("symbols", "x")

  val intrlvS2P = S2P(10, 40, 1)
  val intrlvP2S = P2S(40, 10, 1)
  val frameData = Seq.fill(40 * 2)(BigInt(1, Random))

  "s2p" should "work" in ChainsawTest.test(intrlvS2P, frameData, testName = "testS2P")

  "p2s" should "work" in ChainsawTest.test(intrlvP2S, frameData, testName = "testP2S")

  "ftn" should "draw the frame format" in {
    rawFrameFormat.generateWaveform("raw", "")
  }

  "convFtn" should "work" in ChainsawTest.test(ConvFtn, data = raw, golden = coded, testName = "testConvFtn")

  // "IntrlvFtn" should "work" in ChainsawTest.test(IntrlvFtn, data = coded, golden = interleaved, testName = "testIntrlvFtn")

  "qammodFtn" should "gen" in RtlGen(QammodFtn.implH, "qammodFtn")

  it should "work" in ChainsawTest.test(QammodFtn,
    data = interleaved,
    golden = symbols,
    metric = ChainsawMetric.ComplexAbs(1e-2),
    // draw = ChainsawDraw.scatterPlot,
    testName = "testQammodFtn")

  val ifftData = Seq.fill(2048)(Random.nextComplex())

  val factors = Seq(8, 8, 8)
  val scales = Seq(2, 2, 1)
  val ifftCoreGen = CtFft(512, true, fftType, 16, factors, scales, 64)

  "hsifft" should "work" in ChainsawTest.testChain(
    gens = Seq(HsIfftPre,ifftCoreGen, HsIfftPostWrapped),
    data = ifftData,
    metrics = Seq(
      ChainsawMetric.ComplexAbs(1e-2),
      ChainsawMetric.ComplexAbs(1e-2),
      ChainsawMetric.DoubleAbs(1e-2)
    )
  )

  val ifftFtnData = Seq.fill(252 * 2 * 8)(Random.nextComplex())

  "ifftftn" should "work" in ChainsawTest.test(IfftFtn,
    ifftFtnData,
    metric = ChainsawMetric.DoubleAbs(1e-2))

  "ctfft" should "work" in ChainsawTest.test(CtFft(512, true, symbolType, 16, factors, scales, 64),
    data = ifftData,
    metric = ChainsawMetric.FftByMean(1e-2)
  )


  behavior of "impls"

  ignore should "impl for s2p" in ChainsawImpl(intrlvS2P, name = "implS2P")

  ignore should "impl for convFtn" in ChainsawImpl(ConvFtn, name = "implConvFtn")

  val fft512_64 = Fft(512, 64, 8, inverse = false, fftType)

  // "spiral fft" should "impl" in ChainsawImpl(fft512_64, name = "implFft512_64")
}

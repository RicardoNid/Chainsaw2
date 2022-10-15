package org.datenlord
package zprize

import breeze.math.Complex
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FtnTest extends AnyFlatSpec {

  symbolFrameFormat.generateWaveform("symbols", "x")

  val intrlvS2P = S2P(N1, N1 * 64, 1)
  val intrlvP2S = P2S(N1 * 64, N1, 1)
  val frameData = Seq.fill(N1 * 64 * 2)(BigInt(1, Random))

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

  val fft512_64 = Fft(512, 64, 8, inverse = false, fftType)

  behavior of "impls"

  ignore should "impl for s2p" in ChainsawImpl(intrlvS2P, name = "implS2P")

  ignore should "impl for convFtn" in ChainsawImpl(ConvFtn, name = "implConvFtn")

  "fft" should "impl" in ChainsawImpl(fft512_64, name = "implFft512_64")
}

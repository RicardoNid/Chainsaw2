package org.datenlord
package ip.ftn

import flowConverters.{P2S, S2P}
import org.datenlord.ChainsawImpl
import org.datenlord.ip.ftn._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ftnTest extends AnyFlatSpec {

  val intrlvS2P = S2P(10, 40, 1)
  val intrlvP2S = P2S(40, 10, 1)
  val frameData = Seq.fill(40 * 2)(BigInt(1, Random))

  "s2p" should "work" in ChainsawTest.test(intrlvS2P, frameData, testName = "testS2P")

  "p2s" should "work" in ChainsawTest.test(intrlvP2S, frameData, testName = "testP2S")

  "convFtn" should "work" in ChainsawTest.test(ConvFtn,
    data = raw,
    golden = coded,
    testName = "testConvFtn")

  "intrlvFtn" should "work" in ChainsawTest.test(IntrlvFtn,
    data = coded,
    golden = interleaved,
    metric = intrlvMetric,
    testName = "testIntrlvFtn")

  "qammodFtn" should "work" in ChainsawTest.test(QammodFtn,
    data = interleaved,
    golden = symbols,
    metric = ChainsawMetric.complexAbs(1e-2),
    // draw = ChainsawDraw.scatterPlot,
    testName = "testQammodFtn")

  val ifftData = Seq.fill(4096)(Random.nextComplex())

  val factors = Seq(8, 8, 8)
  val scales = Seq(2, 2, 1)
  val ifftCoreGen = CtFft(512, true, fftType, 16, factors, scales, 64)

  "ifftftn" should "work" in ChainsawTest.test(IfftFtn,
    data = symbols,
    golden = ifftOut,
    metric = fftMetric(1e-2),
    silentTest = true,
    testName = "testIfftFtn")

  "ctfft" should "work" in ChainsawTest.test(CtFft(512, inverse = true, fftType, 16, factors, scales, 64),
    data = ifftData,
    metric = ChainsawMetric.fftByMean(1e-2)
  )

  val tx = ConvFtn + IntrlvFtn + QammodFtn + IfftFtn

  "tx" should "work" in ChainsawTest.test(tx,
    data = raw,
    golden = ifftOut,
    metric = fftMetric(1e-2),
    testName = "testTx"
  )


  behavior of "impls"

  it should "impl for convFtn" in ChainsawImpl(ConvFtn, name = "implConvFtn")

  it should "impl for intrlvFtn" in ChainsawImpl(IntrlvFtn, name = "implIntrlvFtn")

  it should "impl for qammodFtn" in ChainsawImpl(QammodFtn, name = "implQammodFtn")

  it should "impl for ifftFtn" in ChainsawImpl(IfftFtn, name = "implQammodFtn")

  it should "impl for tx"  in ChainsawImpl(tx, name = "implTx")

}

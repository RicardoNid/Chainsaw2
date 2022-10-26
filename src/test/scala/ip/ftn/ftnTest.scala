package org.datenlord
package ip.ftn

import org.datenlord.dsp.CtFft
import flowConverters.S2P
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ftnTest extends AnyFlatSpec {

  "frame formats" should "show" in {
    println("show formats")
    //    println(rawFrameFormat)
    //    println(codedFrameFormat)
    //    println(symbolFrameFormat)
    //    println(fftFrame)
    //    println(paddedFrameFormat)
    println(txRxFrameFormat)
  }

  /** --------
   * tx tests
   * -------- */

  "convFtn" should "work" in ChainsawTest.test(ConvFtn,
    data = raw,
    golden = coded,
    testName = "testConvFtn")

  "intrlvFtn" should "work" in ChainsawTest.test(IntrlvFtn(false),
    data = coded,
    golden = interleaved,
    testName = "testIntrlvFtn")

  "qammodFtn" should "work" in ChainsawTest.test(QammodFtn,
    data = interleaved,
    golden = symbols,
    metric = complexMetricFtn(1e-2, 1e-2),
    testName = "testQammodFtn")

  val complexData = Seq.fill(4096)(Random.nextComplex())

  "ifftftn" should "work" in ChainsawTest.test(HsIfftFtn,
    data = symbols,
    golden = ifftOut,
    metric = doubleMetricFtn(1e-2, 1e-2),
    testName = "testIfftFtn")

  /** --------
   * rx tests
   * -------- */

  "rvFftPre" should "work" in ChainsawTest.test(RvFftPre,
    data = Seq.fill(widthWithCp * 8)(Random.nextDouble()),
    metric = complexMetricFtn(1e-2, 1e-2),
    testName = "testRvFftPre")

  "rvFftPost" should "work" in ChainsawTest.test(RvfftPost,
    data = Seq.fill(fftSize * 8)(Random.nextComplex()),
    metric = complexMetricFtn(1e-2, 1e-2),
    testName = "testRvFftPost")

  val scales = Seq(2, 2, 1)
  val factors = Seq(8, 8, 8)
  val ifftCoreGen = CtFft(512, inverse = false, fftType, 16, factors, scales, 64)
  val combinedGen = RvFftPre -> ifftCoreGen -> RvfftPost

  "rvFftFtn" should "work" in ChainsawTest.test(RvFftftn,
    data = fftIn,
    golden = fftOut,
    metric = complexMetricFtn(1e-2, 1e-2),
    testName = "testRvFftFtn")

  "qamdemodFtn" should "work" in ChainsawTest.test(QamdemodFtn,
    data = equalizationOut,
    golden = qamdemodOut,
    metric = bitMetricFtn(1e-2),
    testName = "testQamdemodFtn")

  "deIntrlvFtn" should "work" in ChainsawTest.test(IntrlvFtn(true),
    data = qamdemodOut,
    golden = deinterleaveOut,
    testName = "testDeIntrlvFtn"
  )

  "viterbiFtn" should "work" in ChainsawTest.test(ViterbiFtn, // FIXME: correct it for bubble
    data = deinterleaveOut,
    golden = viterbiOut,
    metric = bitMetricFtn(1e-2),
    testName = "testViterbiFtn"
  )

  /** --------
   * system-level test
   * -------- */

  val tx = ConvFtn -> IntrlvFtn(inverse = false) -> QammodFtn -> HsIfftFtn

  "tx" should "work" in ChainsawTest.test(tx,
    data = raw,
    golden = ifftOut,
    metric = doubleMetricFtn(1e-2, 1e-2),
    testName = "testTx"
  )

  val rx = QamdemodFtn -> DeintrlvFtn -> ViterbiFtn

  "rx" should "work" in ChainsawTest.test(rx,
    data = equalizationOut,
    golden = viterbiOut,
    metric = bitMetricFtn(1e-2),
    testName = "testTx"
  )

  behavior of "synths"

  /** --------
   * modules
   * -------- */

  it should "synth for convFtn" in ChainsawSynth(ConvFtn, name = "synthConvFtn")
  it should "synth for intrlvFtn" in ChainsawSynth(IntrlvFtn(inverse = false), name = "synthIntrlvFtn")
  it should "synth for qammodFtn" in ChainsawSynth(QammodFtn, name = "synthQammodFtn")
  it should "synth for hsIfftFtn" in ChainsawSynth(HsIfftFtn, name = "synthIfftFtn")

  it should "synth for viterbiFtn" in ChainsawSynth(ViterbiFtn, name = "synthViterbiFtn")
  it should "synth for deintrlvFtn" in ChainsawSynth(IntrlvFtn(inverse = true), name = "synthDeintrlvFtn")
  it should "synth for qamdemodFtn" in ChainsawSynth(QamdemodFtn, name = "synthQamdemodFtn")
  it should "synth for rvFftFtn" in ChainsawSynth(RvFftftn, name = "synthRvFftFtn")

  it should "synth for rvFftPre" in ChainsawSynth(RvFftPre, name = "synthRvFftPre")
  it should "synth for rvFftPost" in ChainsawSynth(RvfftPost, name = "synthRvFftPost")


  /** --------
   * system-level
   * -------- */
  it should "synth for tx" in ChainsawSynth(tx, name = "implTx")

  it should "synth for rx" in ChainsawSynth(rx, name = "synthRx")

  //  val s2p = S2P(64, 256, fftType.bitWidth)
  //  val rxFull = RvFftftn -> s2p -> QamdemodFtn -> DeintrlvFtn -> ViterbiFtn
  //  it should "synth for rxFull" in ChainsawSynth(rxFull, name = "synthRxFull")

}

package org.datenlord
package zprize

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class pngKaratsubaTest extends AnyFlatSpec {

  val width = 256
  val karaGen = Karatsuba(width)
  karaGen.toPng(s"kara$width")
  val data = Seq.fill(100)(BigInt(width, Random))

  behavior of "Karatsuba"

  it should "gen with concrete vertices" in RtlGen(karaGen.implH, "karaGenWithCompressor")

  it should "work with concrete vertices" in ChainsawTest.test(karaGen, data, metric = ChainsawMetric.carrySaveMetric(karaGen.compensation))

  it should "synth with compressor as naive" in {
    naiveSet += "CompressorTree"
    ChainsawSynth(karaGen, "karaGenWithoutCompressor")
    naiveSet.clear()
  }

  it should "synth with concrete vertices" in ChainsawSynth(karaGen, "karaGenWithCompressor")

  it should "impl with concrete vertices" in ChainsawImpl(karaGen, "karaGenWithCompressor")

}

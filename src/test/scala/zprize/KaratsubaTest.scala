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

  it should "work with vertices as naive" in {
    karaGen.setVerticesAsNaive()
    logger.warn(naiveSet.mkString(" "))
    ChainsawTest.test(karaGen, data)
    naiveSet.clear()
  }

  it should "work with compressor as naive" in {
    naiveSet += "CompressorTree"
    ChainsawTest.test(karaGen, data)
    naiveSet.clear()
  }

  it should "work with concrete vertices" in ChainsawTest.test(karaGen, data)

  it should "synth with compressor as naive" in {
    naiveSet += "CompressorTree"
    ChainsawSynth(karaGen)
    naiveSet.clear()
  }

  it should "impl with concrete vertices" in {
    ChainsawImpl(karaGen)
  }

}

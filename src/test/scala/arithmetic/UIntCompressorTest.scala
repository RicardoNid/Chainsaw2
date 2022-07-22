package org.datenlord
package arithmetic

import org.datenlord.dfg.ArithInfo
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class UIntCompressorTest extends AnyFlatSpec {

  val testCount = 1000
  val testWidth = 115
  val operandsCount = 20

  val infos = Seq.fill(operandsCount)(ArithInfo(testWidth, 0))
  val operands = (0 until testCount).flatMap(_ => infos.map(info => Random.nextBigInt(info.width)))
  val config = UIntCompressorConfig(infos)

  "UInt Compressor" should "work" in TransformTest.test(config.implH, operands)

  it should "synth" in VivadoSynth(config.implH, "UIntCompressor")

}

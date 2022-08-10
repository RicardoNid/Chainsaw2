package org.datenlord
package arithmetic

import dfg.ArithInfo

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BmcTest extends AnyFlatSpec {

  val testCount = 1000
  val testWidth = 127
  //  val testWidth = 15
  val operandsCount = 50
  //  val operandsCount = 10

  val infos = (0 until operandsCount).map(_ => ArithInfo(testWidth, Random.nextInt(10) + 1))
  val operands = (0 until testCount).flatMap(_ => infos.map(info => Random.nextBigInt(info.width)))
  //  val operands = (0 until testCount).flatMap(_ => infos.map(info => BigInt(0)))
  val config = BmcConfig(infos)

  "Bit Matrix Compressor" should "work" in TransformTest.test(config.implH, operands, config.metric)

  it should "synth" in VivadoSynth(config.implH, "UIntCompressor")
  it should "impl" in VivadoImpl(config.implH, "UIntCompressor")
  ignore should "synth by naive impl" in VivadoSynth(config.naiveImplH, "UIntSum")
}

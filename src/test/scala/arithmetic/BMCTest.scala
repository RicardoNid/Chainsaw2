package org.datenlord
package arithmetic

import dfg.ArithInfo

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BMCTest extends AnyFlatSpec {

  val testCount = 1000
  val testWidth = 377
  val operandsCount = 40

  val infos = Seq.fill(operandsCount)(ArithInfo(testWidth, 0))
  val operands = (0 until testCount).flatMap(_ => infos.map(info => Random.nextBigInt(info.width)))
  val config = BmcConfig(infos)

  "UInt Compressor" should "work" in TransformTest.test(config.implH, operands, config.metric)

  ignore should "synth" in VivadoSynth(config.implH, "UIntCompressor")
  ignore should "synth by naive impl" in VivadoSynth(config.naiveImplH, "UIntSum")

}

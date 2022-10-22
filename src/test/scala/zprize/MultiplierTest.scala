package org.datenlord
package zprize

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class MultiplierTest extends AnyFlatSpec {

  behavior of "multiplier"

  verbose = 1

  val mult34 = Multiplier(Seq(34,34))

  it should "work correctly for different widths" in{
    val widths = 20 until 34
    widths.foreach { width =>
      val mult = Multiplier(Seq(width, width))
      ChainsawTest.test(mult,
        data = Seq.fill(200)(BigInt(width, Random)))
    }
  }

  it should "met the performance requirement" in
    ChainsawImpl(mult34, "mult34", withRequirement = true)

  it should "synth for mode 1" in
    ChainsawSynth(Multiplier(Seq(18,18)), "mult18", withRequirement = true)

}

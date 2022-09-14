package org.datenlord
package arithmetic

import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

class Karatsuba377Test extends AnyFlatSpec {

  val testCaseCount = 1000
  val data378 = (0 until testCaseCount * 2).map(_ => Random.nextBigInt(378))

  behavior of "karatsuba377"

  it should "show" in Karatsuba377(FullMultiplier).validate().toPng("karashow")
  it should "work" in TransformTest.test(Karatsuba377(FullMultiplier).toTransform, data378, name = "kara377Work")

  val requirement377 = VivadoUtilRequirement(dsp = 162, lut = 20000)

  it should "synth" in VivadoSynth(Karatsuba377(FullMultiplier).toTransform, "kara377impl")
      .require(requirement377, 500 MHz)
  it should "impl" in VivadoImpl(Karatsuba377(FullMultiplier).toTransform, "kara377impl")
      .require(requirement377, 500 MHz)
}
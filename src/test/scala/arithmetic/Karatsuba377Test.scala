package org.datenlord
package arithmetic

import MultplierMode._

import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

class Karatsuba377Test extends AnyFlatSpec {

  val testCaseCount = 1000
  val data378 = (0 until testCaseCount * 2).map(_ => Random.nextBigInt(378))

  behavior of "karatsuba377"

  it should "work" in TransformTest.test(Karatsuba377(FULL).toTransform, data378)

  it should "work for low-bits" in TransformTest.test(Karatsuba377(HALFLOW).toTransform, data378)

  val requirement377 = VivadoUtilRequirement(dsp = 162, lut = 20000)

  it should "synth" in {
    //    VivadoSynth(Karatsuba377().toTransform, "kara377synth").require(requirement377, 800 MHz)
    VivadoImpl(Karatsuba377(FULL).toTransform, "kara377impl")
      .require(requirement377, 600 MHz)
  }

  it should "synth for low-bits" in {
    VivadoImpl(Karatsuba377(HALFLOW).toTransform, "kara377impl")
      .require(requirement377, 600 MHz)
  }
}

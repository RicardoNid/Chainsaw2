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

  "karatsuba377" should "work" in {
    Karatsuba377().validate().toPng("karatsuba377after")
    TransformTest.test(Karatsuba377().toTransform, data378)
  }

  ignore should "synth" in {
    val requirement377 = VivadoUtilRequirement(dsp = 162, lut = 20000)
    VivadoSynth(Karatsuba377().toTransform, "kara377synth").require(requirement377, 800 MHz)
    VivadoImpl(Karatsuba377().toTransform, "kara377impl").require(requirement377, 800 MHz)
  }
}

package org.datenlord
package arithmetic

import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

class Karatsuba96Test extends AnyFlatSpec {

  val testCaseCount = 1000
  val data96 = (0 until testCaseCount * 2).map(_ => Random.nextBigInt(96))

  "kara96" should "work" in {
    Karatsuba96().validate().toPng()
    TransformTest.test(Karatsuba96().toTransform, data96, name = "kara96Test")
  }

  ignore should "synth" in {
    val requirement96 = VivadoUtilRequirement(dsp = 18, lut = 2000)
    VivadoImpl(Karatsuba96().toTransform, "kara96").require(requirement96, 800 MHz)
  }

}

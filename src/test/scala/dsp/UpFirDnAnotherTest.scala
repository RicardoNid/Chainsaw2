package org.datenlord
package dsp

import org.datenlord.xilinx.VivadoUtilRequirement
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random

class UpFirDnAnotherTest extends AnyFlatSpec {

  val data = (0 until 100).map(_ => Random.nextDouble())
  val coeffs = (0 until 144).map(_ => Random.nextDouble())
  val typeIn = HardType(SFix(0 exp, -13 exp))

  val config = UpFirDnAnotherConfig(8, 2, coeffs, typeIn)

  behavior of "upfirdn"

  it should "work" in TransformTest.test(config.implH, data)

  val utilRequirement = VivadoUtilRequirement(dsp = 300)

  it should "synth" in VivadoSynth(config.implH).require(utilRequirement, 500 MHz)

}

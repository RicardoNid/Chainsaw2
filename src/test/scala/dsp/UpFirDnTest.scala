package org.datenlord
package dsp

import intel.QuartusFlow
import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

class UpFirDnTest extends AnyFlatSpec {

  val data = (0 until 100).map(_ => Random.nextDouble())
  val coeffs = (0 until 144).map(_ => Random.nextDouble())
  val typeIn = HardType(SFix(0 exp, -13 exp))
  val typeOut = HardType(SFix(log2Up(coeffs.length) exp, typeIn().minExp exp))

  val config = UpFirDnAnotherConfig(4, 2, coeffs, typeIn, typeOut)

  behavior of "upfirdn"

  it should "work" in TransformTest.test(config.implH, data)

  val utilRequirement = VivadoUtilRequirement(dsp = 300)

  it should "synth in xilinx device" in VivadoSynth(config.implH).require(utilRequirement, 500 MHz)
  it should "synth in intel device" in new QuartusFlow(config.implH).impl()

}

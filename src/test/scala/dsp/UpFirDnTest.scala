package org.datenlord
package dsp

import intel.QuartusFlow
import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

class UpFirDnTest extends AnyFlatSpec {


  val coeffCount = 16
  val coeffs = Seq.fill(coeffCount)(Random.nextDouble())

  val typeIn = HardType(SFix(0 exp, -13 exp))
  val typeOut = HardType(SFix(log2Up(coeffs.length) exp, typeIn().minExp exp))

  val config = UpFirDnConfig(4, 2, coeffs, typeIn, typeOut)

  val validData = Seq.fill(100)(Random.nextDouble())

  // tail padding >= length of coeffs
  val tailPad = Seq.fill(config.subFilterTaps)(0.0)

  val data = validData ++ tailPad

  behavior of "upfirdn"

  it should "work" in TransformTest.test(config.implH, data)

  val utilRequirement = VivadoUtilRequirement(dsp = 300)

  it should "synth in xilinx device" in VivadoSynth(config.implH).require(utilRequirement, 500 MHz)
  it should "synth in intel device" in new QuartusFlow(config.implH).impl()

}

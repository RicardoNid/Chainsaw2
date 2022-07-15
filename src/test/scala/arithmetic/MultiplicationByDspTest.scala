package org.datenlord
package arithmetic

import arithmetic.MultplierMode._
import device.MultiplicationByDspConfig
import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.IntToBuilder

import scala.util.Random

class MultiplicationByDspTest extends AnyFlatSpec {

  val testCount = 1000
  val config0 = MultiplicationByDspConfig(Full)
  val config1 = MultiplicationByDspConfig(Low)
  val config2 = MultiplicationByDspConfig(Square)

  val data16 = (0 until testCount * 2).map(_ => Random.nextBigInt(32))
  val data17Square = (0 until testCount).flatMap { _ =>
    val data = Random.nextBigInt(34)
    Seq.fill(2)(data)
  }

  "Multiplication by DSP Slice for Xilinx Ultrascale" should "work for full multiplication" in TransformTest.test(config0.implH, data16, name = "Mult34ForFull")
  it should "work for low-bit multiplication" in TransformTest.test(config1.implH, data16, name = "Mult36ForLow")
  it should "work for squaring" in TransformTest.test(config2.implH, data17Square, name = "Mult36ForSquare")

  val utilRequirement = VivadoUtilRequirement(dsp = 3)
  val fmaxRequirement = 800 MHz

  it should "synth for full multiplication " in VivadoSynth(config0.implH, "Mult34ForFull").require(utilRequirement, fmaxRequirement)
  it should "synth for low-bit multiplication " in VivadoSynth(config1.implH, "Mult36ForLow").require(utilRequirement, fmaxRequirement)
  it should "synth for squaring " in VivadoSynth(config2.implH, "Mult36ForSquare").require(utilRequirement, fmaxRequirement)
}

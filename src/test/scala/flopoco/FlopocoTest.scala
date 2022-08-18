package org.datenlord
package flopoco

import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.IntToBuilder

import scala.util.Random

class FlopocoTest extends AnyFlatSpec {

  val unsignedData = (0 until 100).map(_ => Random.nextBigInt(64))
  val multConfig = IntMultiplierConfig(96, 96, maxDSP = 18, signed = false)

  // FIXME: when bits are in fact SInt, negative stimulus can't be fed
  "IntMultiplier" should "sim" in TransformTest.test(multConfig.implH, unsignedData)

  ignore should "synth" in VivadoSynth(multConfig.implH, "mult").require(VivadoUtilRequirement(dsp = 3), 800 MHz)

  val multiAddConfig = IntMultiAdderConfig(64, 100, signed = false)

  "IntMultiAdder" should "sim" in TransformTest.test(multiAddConfig.implH, unsignedData)

  ignore should "synth" in VivadoSynth(multiAddConfig.implH, "multiAdd")

  val dspConfig = DSPBlockConfig(Seq(16, 26, 42), xSigned = false, ySigned = false, 1, 1)

  // TODO: verification for DSPBlock
  // FIXME: pre-addition inference failed

  behavior of "DSPBlock"

  ignore should "synth" in VivadoSynth(dspConfig.implH, "dspAbcd")

  behavior of "IntConstMultShiftAddOptTernary"

  val config24_18 = IntConstMultShiftAddOptTernaryConfig(24, Random.nextBigInt(18))

  it should "sim" in TransformTest.test(
    config24_18.implH,
    data = (0 until 100).map(_ => Random.nextBigInt(24)),
    name = "testTernaryPAG"
  )

  it should "synth" in VivadoImpl(config24_18.implH, name = "TernaryPAG")



}

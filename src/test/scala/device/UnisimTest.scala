package org.datenlord
package device

import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim.{SimConfig, _}

import scala.language.postfixOps

class UnisimTest extends AnyFlatSpec {

  "CARR8" should "synth" in VivadoSynth(CARRY8(), "carry8").require(VivadoUtilRequirement(lut = 0, carry8 = 1), fmaxRequirement = 100 MHz)

  "LUT6" should "synth" in VivadoSynth(LUT6_2(BigInt(0)), "lut6").require(VivadoUtilRequirement(lut = 1, carry8 = 0), fmaxRequirement = 100 MHz)

  // TODO: primitives need more encapsulation like this
  case class Lut6Dut(init: BigInt) extends Component {
    val dataIn = in Bits (6 bits)
    val dataOut = out Bits (2 bits)
    val lut6 = LUT6_2(init)
    lut6.I0 := dataIn(0)
    lut6.I1 := dataIn(1)
    lut6.I2 := dataIn(2)
    lut6.I3 := dataIn(3)
    lut6.I4 := dataIn(4)
    lut6.I5 := dataIn(5)
    dataOut(0) := lut6.O5
    dataOut(1) := lut6.O6
  }

  it should "work" in SimConfig.withFstWave.compile {
    Lut6Dut((BigInt(1) << 32) + 1)
  }.doSim { dut =>
    (0 until 64).foreach { i =>
      dut.dataIn #= BigInt(i)
      sleep(1)
      println(s"lookup $i ${dut.dataOut.toBigInt.toString(2)}")
    }
  }

  // FIXME: behavior model of carry8 need modification

}

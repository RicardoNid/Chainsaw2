package org.datenlord
package device

import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps

class Dsp48Test extends AnyFlatSpec {

  case class DSP48TestModule() extends Component {
    val a, d = in UInt (16 bits)
    val b = in UInt (26 bits)
    val c = in UInt (46 bits)
    val r = out UInt (47 bits)

    r := Dsp48.adbc(a, b, c, d)
  }

  val requirement = VivadoUtilRequirement(dsp = 1, lut = 0)

  // FIXME: pre-addition inference failed, why?
  ignore should "always synth to a single DSP slice" in VivadoSynth(DSP48TestModule(), "test0").require(requirement, 800 MHz)

}

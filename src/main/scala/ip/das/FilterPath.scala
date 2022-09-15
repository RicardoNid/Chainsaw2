package org.datenlord
package ip.das

import dsp.AlgebraicMode._
import dsp.RotationMode._
import dsp._
import intel.QuartusFlow

import breeze.numerics.constants.Pi
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class FilterPath(implicit staticConfig: DasStaticConfig) extends Component {

  val constants: DasConstants = staticConfig.genConstants()

  import constants._

  // I/O
  val flowIn = in(DasFlow(adcDataType, subFilterCount))
  val flowOut = out(DasFlow(normalizedPhaseType, subFilterCount))

  assert(~(flowIn.modeChange && ~flowIn.pulseChange)) // pulseChange must be asserted when modeChange is asserted

  val cordicIteration = 10
  val cordicFraction = 14

  // submodule configs
  val realFirConfig = UpFirDnConfig(1, subFilterCount, realCoeffGroups(5e6), adcDataType, firOutDataType)
  val imagFirConfig = UpFirDnConfig(1, subFilterCount, imagCoeffGroups(5e6), adcDataType, firOutDataType)
  val cordicConfig = CordicConfig(CIRCULAR, VECTORING, cordicIteration, cordicFraction)

  assert(realFirConfig.latency == imagFirConfig.latency)
  val latency = realFirConfig.latency + cordicConfig.latency + 1 // 1 for multiplication

  // filter path
  val realFirRets = realFirConfig.implH.asFunc(flowIn.payload)
    .map(_ >> 4) // normalization for CORDIC
  val imagFirRets = imagFirConfig.implH.asFunc(flowIn.payload) // FIXME: this becomes the inverse of expected ret, why?
    .map(_ >> 4).map(sf => -sf) // normalization for CORDIC

  val both = realFirRets.zip(imagFirRets).map { case (real, imag) =>
    val core = cordicConfig.implH
    core.dataIn.fragment := Vec(real.truncated, imag.truncated, SFConstant(0.0, cordicConfig.phaseType))
    core.skipControl()
    core.dataOut.fragment
  }

  val intensities = both.map(_.head)
  val phases = both.map(_.last)

  val normalizedPhases = phases.map(_ * SFConstant(1 / Pi, HardType(SFix(0 exp, -17 exp))))
    .map(_.truncate(normalizedPhaseType).d(1))

    flowOut := flowIn.pipeWith(Vec(normalizedPhases), latency)
}

object FilterPath {
  def main(args: Array[String]): Unit = {
    new QuartusFlow(FilterPath(DasStaticConfig()), "filterpath").impl()
  }
}

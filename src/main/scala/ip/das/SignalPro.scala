package org.datenlord
package ip.das

import dsp.AlgebraicMode.CIRCULAR
import dsp.RotationMode.VECTORING
import dsp.{CordicConfig, UpFirDnConfig}

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DasFlow(hardType: HardType[SFix], portWidth: Int) extends Bundle {
  val payload = Vec(hardType(), portWidth)
  val valid = Bool()
  val pulseChange = Bool()
  val modeChange = Bool()
  val index = UInt(10 bits) // for sim only
}

case class SignalPro(staticConfig: DasStaticConfig) extends Component {

  val constants: DasConstants = staticConfig.genConstants()

  import constants._

  // hardtypes
  val adcDataType = HardType(SFix(0 exp, -13 exp))
  val firOutDataType = HardType(SFix(4 exp, -13 exp))
  val cordicIteration = 12
  val cordicFraction = 16

  // submodule configs
  val realFirConfig = UpFirDnConfig(1, subFilterCount, realCoeffGroups(5e6), adcDataType, firOutDataType)
  val imagFirConfig = UpFirDnConfig(1, subFilterCount, imagCoeffGroups(5e6), adcDataType, firOutDataType)
  val cordicConfig = CordicConfig(CIRCULAR, VECTORING, cordicIteration, cordicFraction)

  assert(realFirConfig.latency == imagFirConfig.latency)
  val latency = realFirConfig.latency + cordicConfig.latency

  // I/O
//  val flowIn = in(DasFlow(adcDataType, subFilterCount))
//  val flowOut = out(DasFlow(cordicConfig.phaseType, subFilterCount))

  val pulseChange = in Bool()
  val modeChange = in Bool()
  val validIn = in Bool()
  val indexIn = in UInt (10 bits) // for debug only

  val validOut = out Bool()
  val indexOut = out UInt (10 bits) // for debug only

  val dataIn = in Vec(adcDataType, subFilterCount)

  assert(~(modeChange && ~pulseChange)) // pulseChange must be asserted when modeChange is asserted

  // filter path
  val realFirRets = realFirConfig.implH.asFunc(dataIn)
    .map(_ >> 4) // normalization for CORDIC
  val imagFirRets = imagFirConfig.implH.asFunc(dataIn) // FIXME: this becomes the inverse of expected ret, why?
    .map(_ >> 4).map(sf => -sf) // normalization for CORDIC

  val both = realFirRets.zip(imagFirRets).map { case (real, imag) =>
    val core = cordicConfig.implH
    core.dataIn.fragment := Vec(real.truncated, imag.truncated, SFConstant(0.0, cordicConfig.phaseType))
    core.skipControl()
    core.dataOut.fragment
  }

  val intensities = both.map(_.head)
  val phases = both.map(_.last)
  val dataOut = out(Vec(phases.map(_ >> 0)))
  validOut := validIn.validAfter(latency)
  indexOut := Delay(indexIn, latency, init = U(0, 10 bits))

}

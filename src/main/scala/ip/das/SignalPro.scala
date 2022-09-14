package org.datenlord
package ip.das

import org.datenlord.dsp.AlgebraicMode.CIRCULAR
import org.datenlord.dsp.RotationMode.VECTORING
import org.datenlord.dsp.{CordicConfig, UpFirDnConfig}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class SignalPro(staticConfig: DasStaticConfig) extends Component {

  val constants: DasConstants = staticConfig.genConstants()

  import constants._

  // hardtypes
  val adcDataType = HardType(SFix(0 exp, -13 exp))
  val firOutDataType = HardType(SFix(4 exp, -13 exp))
  val cordicIteration = 12
  val cordicFraction = 16

  // submodule configs
  val realFirConfig = UpFirDnConfig(1, subFilterCount, realCoeffGroups.values.head, adcDataType, firOutDataType)
  val imagFirConfig = UpFirDnConfig(1, subFilterCount, imagCoeffGroups.values.head, adcDataType, firOutDataType)
  val cordicConfig = CordicConfig(CIRCULAR, VECTORING, cordicIteration, cordicFraction)

  val latency = realFirConfig.latency //  + cordicConfig.latency

  // I/O
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
    .map(_ >> 0)
  //    .map(_ >> 4) // normalization for CORDIC
  val imagFirRets = imagFirConfig.implH.asFunc(dataIn)
    .map(_ >> 4) // normalization for CORDIC

  val both = realFirRets.zip(imagFirRets).map { case (real, imag) =>
    val core = cordicConfig.implH
    core.dataIn.fragment := Vec(real.truncated, imag.truncated, SFConstant(0.0, cordicConfig.phaseType))
    core.skipControl()
    core.dataOut.fragment
  }

  val intensities = both.map(_.head)
  val phases = both.map(_.last)
  val dataOut = out(Vec(realFirRets))
  validOut := validIn.validAfter(latency)
  indexOut := Delay(indexIn, latency, init = U(0, 10 bits))

}

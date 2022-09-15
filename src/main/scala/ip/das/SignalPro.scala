package org.datenlord
package ip.das

import dsp.AlgebraicMode.CIRCULAR
import dsp.RotationMode.VECTORING
import dsp.{CordicConfig, UpFirDnConfig}

import spinal.core
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DasFlowAnother(hardType: HardType[SFix], portWidth: Int) extends Bundle {
  val payload = Vec(hardType(), portWidth)
  val valid = Bool()
  val pulseChange = Bool()
  val modeChange = Bool()
  val index = UInt(10 bits) // for sim only

  def pipeWith(payload:Vec[SFix], cycle:Int) = {
    val ret = DasFlowAnother(HardType(payload.head), portWidth)
    ret.payload := payload
    ret.valid := valid.validAfter(cycle)
    ret.pulseChange := pulseChange.validAfter(cycle)
    ret.modeChange := modeChange.validAfter(cycle)
    ret.index := Delay(index, cycle, init = U(0, 10 bits))
    ret
  }
}

case class SignalPro(staticConfig: DasStaticConfig) extends Component {

  /** --------
   * constants
   -------- */
  val constants = staticConfig.genConstants()
  import constants._

  val phaseStoredType = HardType(SFix(4 exp, -16 exp))
  val phaseSumType = HardType(SFix(4 + log2Up(gaugePointsMax) exp, -16 exp))

  /** --------
   * sub-modules
   -------- */
  val filterPath = FilterPath(staticConfig)
  val phaseDiff = PhaseDiff(staticConfig)
  val pulseUnwrap = PulseUnwrap(staticConfig)
  val phaseMean = PhaseMean(staticConfig)

  /** --------
   * parameters path
   -------- */
  // parameters input
  val gaugePointsIn = in UInt (log2Up(gaugePointsMax.divideAndCeil(subFilterCount) + 1) bits)
  val pulsePointsIn = in UInt (log2Up(pulsePointsMax.divideAndCeil(subFilterCount) + 2) bits)
  val gaugeReverseIn = in SFix(0 exp, -17 exp)
  // allocate parameters
  phaseDiff.gaugePointsIn := gaugePointsIn
  pulseUnwrap.pulsePointsIn := pulsePointsIn
  phaseMean.gaugePointsIn := gaugePointsIn
  phaseMean.gaugeReverseIn := gaugeReverseIn

  /** --------
   * datapath
   -------- */
  val flowIn = in cloneOf filterPath.flowIn
  filterPath.flowIn := flowIn
  phaseDiff.flowIn := filterPath.flowOut
  pulseUnwrap.flowIn := phaseDiff.flowOut
  phaseMean.flowIn := pulseUnwrap.flowOut

  val flowOut = out cloneOf phaseMean.flowOut
  flowOut := phaseMean.flowOut
}
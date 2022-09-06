package org.datenlord
package ip.das

import intel.QuartusFlow

import org.datenlord.matlab.MatlabFeval
import spinal.core._
import spinal.lib.{master, slave}

import scala.language.postfixOps

case class PhasePath(config: DasConfig) extends Component {

  // import parameters from config
  import config._

  val phaseType = HardType(SFix(1 exp, -14 exp)) // [-1, 1] for normalized phase
  val phaseDiffType = HardType(SFix(2 exp, -14 exp)) // [-2, 2] for phase difference
  val phaseStoredType = HardType(SFix(6 exp, -8 exp)) // [-32, 32) for unwrapped phase
  val phaseUnwrappedType = HardType(SFix(6 exp, -14 exp))

  // configuration
  val gaugePointsIn = in UInt (log2Up(gaugePointsMax + 5) bits)
  val pulsePointsIn = in UInt (log2Up(pulsePointsMax + 5) bits)
  val meanPointsIn = in UInt (log2Up(meanPointsMax + 5) bits)
  // data
  val flowIn = slave(DasFlow(phaseType))
  val flowOut = master(DasFlow(phaseUnwrappedType))
  val valid = out Bool()

  // sub-modules
  val phaseDiff = PhaseDiff(gaugePointsMax, phaseType, phaseDiffType)
  val pulseUnwrap = PulseUnwrap(pulsePointsMax, phaseUnwrappedType, phaseStoredType)
  val phaseMean = PhaseMean(gaugePointsMax, gaugePointsMin, phaseUnwrappedType)
  val meanUnwrap = MeanUnwrap(meanPointsMax, phaseUnwrappedType, phaseStoredType)

  // connection
  phaseDiff.flowIn := flowIn
  phaseDiff.gaugePointsIn := gaugePointsIn

  pulseUnwrap.flowIn := phaseDiff.flowOut
  pulseUnwrap.pulsePointsIn := pulsePointsIn

  phaseMean.flowIn := pulseUnwrap.flowOut
  phaseMean.gaugePointsIn := gaugePointsIn

  meanUnwrap.flowIn := phaseMean.flowOut
  meanUnwrap.meanPointsIn := meanPointsIn
  meanUnwrap.validIn := phaseMean.valid

  flowOut := meanUnwrap.flowOut
  valid := meanUnwrap.validOut

  val latency = 1 + 2 + 2 + 2
}

//object PhasePath {
//  def main(args: Array[String]): Unit = {
//    //    VivadoSynth(PhasePath())
//    new QuartusFlow(PhasePath(DasConfig(samplingFreq = 1e9)), "phasePath").impl()
//  }
//}
package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class PhaseMean() extends Component {

  val gaugePointsMax = 1000 // 96 ~ 1000/4.8 ~ 50m, 8 points/0.4m per step
  val unWrappedPhaseType = HardType(SFix(4 exp, -8 exp))
  val coeffType = HardType(SFix(-2 exp, -18 exp))
  val sumMaxExp = unWrappedPhaseType().maxExp + log2Up(gaugePointsMax)
  val sumType = HardType(SFix(sumMaxExp exp, unWrappedPhaseType().minExp exp))

  val flowIn = slave(DasFlow(unWrappedPhaseType()))
  val flowOut = master(DasFlow(unWrappedPhaseType()))
  val valid = out Bool()
  val gaugePointsIn = in UInt (log2Up(gaugePointsMax + 5) bits)

  val coeffs = 0.01 +: (8 to 1000 by 8).map(i => 1 / i.toDouble)
  val coeffHard = coeffs.map(SFConstant(_, coeffType))
  val coeffRom = Mem(coeffs.map(SFConstant(_, coeffType)))

  // state
  val gaugePoints = RegNextWhen(gaugePointsIn, flowIn.modeChange)

  val gaugeCounter = DynamicCounter(gaugePoints)
  gaugeCounter.increment()
  when(flowIn.pulseChange)(gaugeCounter.clear())

  val sum = Reg(sumType)
  when(gaugeCounter.willOverflow.validAfter(1))(sum := flowIn.payload)
    .elsewhen(flowIn.pulseChange.validAfter(1))(sum := flowIn.payload)
    .otherwise(sum := sum + flowIn.payload)

  val coeff = coeffRom.readSync(gaugePoints >> 3)
  val mean = (sum * coeff)
    .truncate(unWrappedPhaseType).d(1)

  flowOut.payload := mean
  flowOut.pulseChange := flowIn.pulseChange
  flowOut.modeChange := flowIn.modeChange
  valid := gaugeCounter.willOverflow.validAfter(2) // 1 for addition, 1 for multiplication
}


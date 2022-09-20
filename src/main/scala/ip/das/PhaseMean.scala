package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class PhaseMean(gaugePointsMax: Int, gaugePointsMin: Int, typeIn: HardType[SFix]) extends Component {

  // coeffType defined by range, example: if (min, max) = (100, 1000), SFix(-7+1 exp, ...) is enough as the coeffMax = 0.01
  val coeffType = HardType(SFix(-log2Up(gaugePointsMin) + 1 exp, -18 exp))
  val sumMaxExp = typeIn().maxExp + log2Up(gaugePointsMax)
  val sumType = HardType(SFix(sumMaxExp exp, typeIn().minExp exp))

  val flowIn = slave(DasFlow(typeIn))
  val flowOut = master(DasFlow(typeIn))
  val valid = out Bool()
  val gaugePointsIn = in UInt (log2Up(gaugePointsMax + 5) bits)

  val coeffs = Seq.fill(gaugePointsMin / 4)(0.0) ++ (gaugePointsMin to gaugePointsMax by 4).map(i => 1 / i.toDouble)
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

  val coeff = coeffRom.readSync(gaugePoints >> 2)
  val mean = (sum * coeff)
    .truncate(typeIn).d(1)

  flowOut.payload := mean
  flowOut.pulseChange := flowIn.pulseChange
  flowOut.modeChange := flowIn.modeChange
  valid := gaugeCounter.willOverflow.validAfter(2) // 1 for addition, 1 for multiplication
}


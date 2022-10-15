package org.datenlord
package ip.das

import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class PhaseMean(implicit staticConfig: DasStaticConfig)
  extends Component {

  val constants = staticConfig.genConstants()

  import constants._

  val flowIn = in(DasFlow(phaseUnwrapType, subFilterCount))
  val flowOut = out(DasFlow(phaseUnwrapType, 1))

  val sumMaxExp = phaseUnwrapType().maxExp + log2Up(gaugePointsMax.divideAndCeil(subFilterCount))
  val sumMinExp = phaseUnwrapType().minExp
  val typeSum = HardType(SFix(sumMaxExp exp, sumMinExp exp))

  val gaugePointsIn = in UInt (log2Up(gaugePointsMax.divideAndCeil(subFilterCount) + 1) bits)
  val gaugeReverseIn = in SFix(0 exp, -17 exp)
  val gaugePoints = RegNextWhen(gaugePointsIn, flowIn.modeChange)
  val gaugeReverse = RegNextWhen(gaugeReverseIn, flowIn.modeChange)

  val sumCounter = DynamicCounter(gaugePoints)
  when(flowIn.pulseChange)(sumCounter.clear())
  sumCounter.increment()

  val partialSums = Reg(Vec(typeSum, 2))
  partialSums.zip(flowIn.payload)
    .foreach { case (sum, data) =>
      sum := Mux(sumCounter.value === U(0, sumCounter.width bits), data.truncate(typeSum), sum + data)
    }

  val pipeline = (data: SFix, _: Int) => data.d(1)

  val sum = partialSums
    .reduceBalancedTree(_ + _, pipeline) // combine all sub channels
  val mean = (sum * gaugeReverse).truncate(phaseUnwrapType).d(1)

  val latency = 3 // 1 for sub channel summing, 1 for summing up all sub channels, 1 for multiplication

  flowOut.payload := Vec(mean)
  flowOut.pulseChange := flowIn.pulseChange.validAfter(latency)
  flowOut.modeChange := flowIn.modeChange.validAfter(latency)
  flowOut.index := Delay(flowIn.index, latency, init = U(0, 10 bits))
  flowOut.valid := sumCounter.willOverflow.validAfter(latency)

}
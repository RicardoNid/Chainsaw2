package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class PhaseDiff(typeIn: HardType[SFix], typeOut: HardType[SFix]) extends Component {

  val gaugePointsMax = 1000

  val flowIn = slave(DasFlow(typeIn))
  val flowOut = master(DasFlow(typeOut))
  val gaugePointsIn = in UInt (log2Up(gaugePointsMax + 5) bits)

  // state
  val gaugePoints = RegNextWhen(gaugePointsIn, flowIn.modeChange)

  val gaugeCounter = DynamicCounter(gaugePoints)
  gaugeCounter.increment()
  when(flowIn.pulseChange)(gaugeCounter.clear())

  val firstGauge = out(RegInit(True))
  firstGauge.setWhen(flowIn.pulseChange)
  firstGauge.clearWhen(firstGauge && gaugeCounter.willOverflow)

  val buffer = Mem(typeIn, gaugePointsMax)

  buffer.write(gaugeCounter, flowIn.payload)
  val fakeAddr = U(1) + gaugeCounter // 1 for read sync latency
  val readAddr = Mux(fakeAddr >= gaugePoints, fakeAddr - gaugePoints, fakeAddr)
  val bufferOut = buffer.readSync(readAddr)

  val ret = (flowIn.payload -^ bufferOut).d(1)

  flowOut.payload := ret
  flowOut.pulseChange := flowIn.pulseChange.validAfter(1)
  flowOut.modeChange := flowIn.modeChange.validAfter(1)

}

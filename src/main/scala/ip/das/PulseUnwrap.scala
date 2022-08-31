package org.datenlord
package ip.das

import dsp.UnwrapConfig

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class PulseUnwrap() extends Component {
  val pulsePointsMax = 8e5.toInt
  // TODO: use different precision on input data and data stored
  val unWrappedPhaseType = HardType(SFix(4 exp, -4 exp))
  // TODO: a "stored phase type"

  val flowIn = slave(DasFlow(unWrappedPhaseType()))
  val flowOut = master(DasFlow(unWrappedPhaseType()))

  val pulsePointsIn = in UInt (log2Up(pulsePointsMax + 5) bits)
  val pulsePoints = RegNextWhen(pulsePointsIn, flowIn.modeChange)

  val pulseCounter = DynamicCounter(pulsePoints)
  pulseCounter.increment()
  when(flowIn.modeChange)(pulseCounter.clear())

  val firstPulse = RegInit(True)
  firstPulse.setWhen(flowIn.modeChange.validAfter(2))
  firstPulse.clearWhen(pulseCounter.willOverflow.validAfter(2)) // readSync latency

  val buffer = Mem(unWrappedPhaseType, pulsePointsMax)

  val ret = unWrappedPhaseType()

  buffer.write(pulseCounter, ret)
  val fakeAddr = U(3) + pulseCounter // 1 for read sync latency, 2 for unwrap latency
  val addr = Mux(fakeAddr >= pulsePoints, fakeAddr - pulsePoints, fakeAddr)
  val bufferOut = buffer.readSync(addr)

  val core = UnwrapConfig(unWrappedPhaseType).implH.asFunc // unwrap module
  val unwrapped = core(Seq(bufferOut, flowIn.payload)).head
  ret := Mux(firstPulse, flowIn.payload.d(2), unwrapped)

  flowOut.payload := ret
  flowOut.pulseChange := flowIn.pulseChange.validAfter(2)
  flowOut.modeChange := flowIn.modeChange.validAfter(2)
}

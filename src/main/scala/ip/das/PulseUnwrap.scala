package org.datenlord
package ip.das

import dsp.UnwrapConfig

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class PulseUnwrap(pulsePointsMax:Int, typeFull:HardType[SFix], typeStored:HardType[SFix]) extends Component {

  val flowIn = slave(DasFlow(typeFull))
  val flowOut = master(DasFlow(typeFull))

  val pulsePointsIn = in UInt (log2Up(pulsePointsMax + 5) bits)
  val pulsePoints = RegNextWhen(pulsePointsIn, flowIn.modeChange)

  val pulseCounter = DynamicCounter(pulsePoints)
  logger.info(s"length of the addr counter: ${pulsePoints.getBitsWidth}")
  pulseCounter.increment()
  when(flowIn.modeChange)(pulseCounter.clear())

  val firstPulse = RegInit(True)
  firstPulse.setWhen(flowIn.modeChange.validAfter(2))
  firstPulse.clearWhen(pulseCounter.willOverflow.validAfter(2)) // readSync latency

  val buffer = Mem(typeStored, pulsePointsMax)

  val ret = typeFull()

  buffer.write(pulseCounter, ret.truncate(typeStored))
  val fakeAddr = (U(5) + pulseCounter).d(1) // 1 for read sync latency, 2 for unwrap latency, 2 for address generation
  val addr = Mux(fakeAddr >= pulsePoints, fakeAddr - pulsePoints, fakeAddr).d(1)
  val bufferOut = buffer.readSync(addr)

  val core = UnwrapConfig(typeStored, typeFull).implH.asFunc // unwrap module
  val unwrapped = core(Seq(bufferOut, flowIn.payload.truncated)).head
  ret := Mux(firstPulse, flowIn.payload.d(2), unwrapped)

  flowOut.payload := ret
  flowOut.pulseChange := flowIn.pulseChange.validAfter(2)
  flowOut.modeChange := flowIn.modeChange.validAfter(2)
}

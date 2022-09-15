package org.datenlord
package ip.das

import dsp.UnwrapConfig

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class MeanUnwrap(meanPointsMax:Int, typeFull:HardType[SFix], typeStored:HardType[SFix])
  extends Component {

  val flowIn = slave(DasFlow(typeFull))
  val flowOut = master(DasFlow(typeFull))
  val validIn = in Bool()
  val validOut = out Bool()

  val meanPointsIn = in UInt (log2Up(meanPointsMax + 5) bits)
  val meanPoints = RegNextWhen(meanPointsIn, flowIn.modeChange)

  val pulseCounter = DynamicCounter(meanPoints)
  when(validIn)(pulseCounter.increment())
  when(flowIn.modeChange)(pulseCounter.clear())

  val firstPulse = RegInit(True)
  firstPulse.setWhen(flowIn.modeChange.validAfter(2))
  firstPulse.clearWhen(pulseCounter.willOverflow.validAfter(2)) // readSync latency

  val buffer = Mem(typeStored, meanPointsMax)
  val ret = typeFull()
  buffer.write(pulseCounter, ret.truncate(typeStored))
  val fakeAddr = pulseCounter.value // 1 for read sync latency, 2 for unwrap latency
  val addr = Mux(fakeAddr >= meanPoints, fakeAddr - meanPoints, fakeAddr)
  val bufferOut = buffer.readSync(addr)

  val core = UnwrapConfig(typeStored, typeFull).implH.asFunc // unwrap module
  val unwrapped = core(Seq(bufferOut, flowIn.payload)).head
  ret := Mux(firstPulse, flowIn.payload.d(2), unwrapped)

  flowOut.payload := ret
  flowOut.pulseChange := flowIn.pulseChange.validAfter(2)
  flowOut.modeChange := flowIn.modeChange.validAfter(2)

  validOut := validIn.validAfter(2)
}


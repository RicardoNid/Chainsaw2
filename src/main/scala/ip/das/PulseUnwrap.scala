package org.datenlord
package ip.das

import dsp.UnwrapConfig

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{State, StateEntryPoint, StateMachine}

import scala.language.postfixOps

case class PulseUnwrap(staticConfig: DasStaticConfig, typeFull: HardType[SFix], typeStored: HardType[SFix]) extends Component {

  val constants = staticConfig.genConstants()

  import constants._

  val flowIn = in(DasFlowAnother(typeFull, subFilterCount))
  val flowOut = out(DasFlowAnother(typeFull, subFilterCount))

  val pulsePointsIn = in UInt (log2Up(pulsePointsMax.divideAndCeil(subFilterCount) + 2) bits)
  val pulsePoints = RegNextWhen(pulsePointsIn, flowIn.modeChange)

  val writeCounter = DynamicCounter(pulsePoints)
  val readCounter = DynamicCounter(pulsePoints)
  val counterWidth = log2Up(pulsePointsMax.divideAndCeil(subFilterCount) + 2)
  writeCounter.increment()
  readCounter.increment()

  val buffer = Mem(Vec(typeStored, subFilterCount), pulsePointsMax.divideAndCeil(subFilterCount))
  val bufferOut = buffer.readSync(readCounter)

  val unwrapCores = Seq.fill(subFilterCount)(UnwrapConfig(typeStored, typeFull).implH) // unwrap module
  val unwrapLatency = UnwrapConfig(typeStored, typeFull).latency
  val unwrapped = bufferOut.zip(flowIn.payload).zip(unwrapCores).map { case ((prev, next), core) => core.asFunc(Seq(prev, next)).head }

  val ret = cloneOf(flowOut.payload)
  ret.assignDontCare() // pre-assignment

  buffer.write(writeCounter.value.d(unwrapLatency), Vec(ret.map(_.truncate(typeStored))))

  val fsm: StateMachine = new StateMachine { // fsm has to be declared, or else, no state can be seen during simulation
    val first = State()
    val normal = StateEntryPoint()
    first.whenIsActive {
      ret := flowIn.payload.d(unwrapLatency)
      when(flowIn.pulseChange) {
        writeCounter.clear()
        readCounter.value := U(1, counterWidth bits) // read latency = 1
        goto(normal)
      }
    }
    normal.whenIsActive {
      ret := Vec(unwrapped)
      when(flowIn.pulseChange) {
        writeCounter.clear()
        readCounter.value := U(1, counterWidth bits)
      }
      when(flowIn.modeChange)(goto(first))
    }
  }

  flowOut := flowIn.pipeWith(ret, unwrapLatency)

}
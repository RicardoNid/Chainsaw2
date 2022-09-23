package org.datenlord
package ip.das

import dsp.UnwrapConfig

import spinal.core._
import spinal.lib.fsm.{State, StateEntryPoint, StateMachine}

import scala.language.postfixOps

case class MeanUnwrap(implicit staticConfig: DasStaticConfig)
  extends Component {

  val constants = staticConfig.genConstants()
  import constants._

  val flowIn = in(DasFlow(phaseUnwrapType, 1))
  val flowOut = out(DasFlow(phaseUnwrapType, 1))

  val spatialPointsIn = in UInt (log2Up(spatialPointsMax + 2) bits)
  val spatialPoints = RegNextWhen(spatialPointsIn, flowIn.modeChange)

  val writeCounter = DynamicCounter(spatialPoints)
  val readCounter = DynamicCounter(spatialPoints)
  val counterWidth = log2Up(spatialPointsMax + 2)
  when(flowIn.valid)(writeCounter.increment())
  when(flowIn.valid)(readCounter.increment())

  val buffer = Mem(Vec(phaseStoredType, 1), spatialPointsMax)
  val bufferOut = buffer.readSync(readCounter)

  val unwrapCore = UnwrapConfig(phaseStoredType, phaseUnwrapType).implH // unwrap module
  val unwrapLatency = UnwrapConfig(phaseStoredType, phaseUnwrapType).latency
  val unwrapped = bufferOut.zip(flowIn.payload).map { case (prev, next) => unwrapCore.asFunc(Seq(prev, next)).head }

  val ret = cloneOf(flowOut.payload)
  ret.assignDontCare() // pre-assignment
  buffer.write(writeCounter.value.d(unwrapLatency), Vec(ret.map(_.truncate(phaseStoredType))), flowIn.valid.d(2))

  val fsm: StateMachine = new StateMachine { // fsm has to be declared, or else, no state can be seen during simulation
    val first = State()
    val normal = StateEntryPoint()
    first.whenIsActive {
      ret := flowIn.payload.d(unwrapLatency)
      when(flowIn.pulseChange) {
        writeCounter.clear()
        readCounter.value := U(0, counterWidth bits)
        goto(normal)
      }
    }
    normal.whenIsActive {
      ret := Vec(unwrapped)
      when(flowIn.pulseChange) {
        writeCounter.clear()
        // no prefetch as valid comes a few cycles after pulseChange
        readCounter.value := U(0, counterWidth bits)
      }
      when(flowIn.modeChange)(goto(first))
    }
  }

  flowOut := flowIn.pipeWith(ret, unwrapLatency)

}


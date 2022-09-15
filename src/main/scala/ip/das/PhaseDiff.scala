package org.datenlord
package ip.das

import spinal.core._
import spinal.lib.fsm._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class PhaseDiff(staticConfig: DasStaticConfig, typeIn: HardType[SFix], typeOut: HardType[SFix]) extends Component {

  val constants = staticConfig.genConstants()

  import constants._

  val flowIn = in(DasFlowAnother(typeIn, subFilterCount))
  val flowOut = out(DasFlowAnother(typeOut, subFilterCount))

  val gaugePointsIn = in UInt (log2Up(gaugePointsMax.divideAndCeil(subFilterCount) + 1) bits)
  val gaugePoints = RegNextWhen(gaugePointsIn, flowIn.modeChange)

  val writeCounter = DynamicCounter(gaugePoints)
  val readCounter = DynamicCounter(gaugePoints)
  writeCounter.increment()
  readCounter.increment()

  val buffer = Mem(Vec(typeIn, subFilterCount), gaugePointsMax.divideAndCeil(subFilterCount))
  buffer.write(writeCounter, flowIn.payload)
  val bufferOut = buffer.readSync(readCounter)
  flowIn.payload.simPublic()
  bufferOut.simPublic()

  val ret = cloneOf(flowOut.payload)
  ret.assignDontCare() // pre-assignment

  val fsm: StateMachine = new StateMachine { // fsm has to be declared, or else, no state can be seen during simulation
    val first = StateEntryPoint()
    val normal = State()
    first.whenIsActive {
      ret := flowIn.payload
      when(writeCounter.willOverflow)(goto(normal))
      when(flowIn.pulseChange) {
        writeCounter.clear()
        readCounter.set(U(1, readCounter.width bits))
      }
    }
    normal.whenIsActive {
      val diff = flowIn.payload.zip(bufferOut).map { case (next, prev) => next -^ prev }
      ret := Vec(diff)
      when(flowIn.pulseChange) {
        writeCounter.clear()
        readCounter.set(U(1, readCounter.width bits))
        goto(first)
      }
    }
  }

  flowOut := flowIn.pipeWith(ret.d(1), 1)
}
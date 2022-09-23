package org.datenlord
package ip.das

import spinal.core._

import scala.language.postfixOps

/** dynamic pulse generator
 *
 */
case class PulseGen(implicit staticConfig: DasStaticConfig)
  extends Component {

  val constants = staticConfig.genConstants()

  import constants._

  val pulsePeriodIn = in UInt (log2Up(pulsePointsMax) bits)
  val todoModeChange = in Bool()

  val pulseChange = out Bool()
  val modeChange = out Bool()
  val pulseOut = out(PulsesOut())

  // can always update itself as the counter is freerun
  val pulsePeriod = RegNextWhen(pulsePeriodIn, pulseChange, init = U(100))

  // free run dynamic counter for pulse generation
  val pulseCounter = DynamicCounter(pulsePeriod)
  when(True)(pulseCounter.increment())
  when(pulseChange)(pulseCounter.clear())

  val pulseOn = (pulseCounter.value <= pulseWidth)
  pulseOut.Pulse_Single := pulseOn
  // TODO: use p0 & p1 for double-pulse scheme
  pulseOut.Pulseout0 := pulseOn
  pulseOut.Pulseout1 := pulseOn
  pulseOut.Pulseout0N := False
  pulseOut.Pulseout1N := False

  val change = pulseCounter.willOverflow
  pulseChange := change
  modeChange := change && todoModeChange

  modeChange.setName("pulseGenModeChange")
  pulseChange.setName("pulseGenPulseChange")
}

object PulseGen extends App {

  import scala.util.Random

  import spinal.core._
  import spinal.core.sim._
  import spinal.lib._
  import spinal.lib.fsm._

  implicit val staticConfig = DasStaticConfig()

  SimConfig.withFstWave.compile(PulseGen()).doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.pulsePeriodIn #= 100
    dut.clockDomain.waitSampling(100000)
    dut.todoModeChange #= true
    dut.clockDomain.waitSampling(300)
    dut.todoModeChange #= false
    dut.clockDomain.waitSampling(300)
  }

}

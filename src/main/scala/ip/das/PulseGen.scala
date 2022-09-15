package org.datenlord
package ip.das

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/** dynamic pulse generator
 *
 */
case class PulseGen(implicit staticConfig: DasStaticConfig) extends Component {

  val constants = staticConfig.genConstants()
  import constants._

  val pulsePeriodIn = in UInt(log2Up(pulsePointsMax) bits)

  val pulseChange = out Bool()
  val pulseOut = out Bool()

  // can always update itself as the counter is freerun
  val pulsePeriod = RegNextWhen(pulsePeriodIn, pulseChange, init = U(pulsePointsMax))

  // free run dynamic counter for pulse generation
  val pulseCounter = DynamicCounter(pulsePeriod)
  when(True)(pulseCounter.increment())
  when(pulseChange)(pulseCounter.clear())

  pulseOut := (pulseCounter.value <= pulseWidth)
  pulseChange := pulseCounter.willOverflow
}

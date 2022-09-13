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
case class PulseGen(implicit config: DasConfig) extends Component {

  import config._

  val pulsePeriodIn = in UInt(log2Up(pulsePeriodMax) bits)

  val pulseChange = out Bool()
  val pulseOut = out Bool()

  // can always update itself as the counter is freerun
  val pulsePeriod = RegNextWhen(pulsePeriodIn, pulseChange, init = U(pulsePeriodMax))

  // free run dynamic counter for pulse generation
  val pulseCounter = DynamicCounter(pulsePeriod)
  when(True)(pulseCounter.increment())
  when(pulseChange)(pulseCounter.clear())

  pulseOut := (pulseCounter.value <= pulseWidth)
  pulseChange := pulseCounter.willOverflow
}

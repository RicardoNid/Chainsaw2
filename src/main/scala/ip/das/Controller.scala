package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DasFlow[T <: Data](dataType: HardType[T]) extends Bundle with IMasterSlave {

  val pulseChange = Bool() // 1 cycle before data of a pulse
  val modeChange = Bool() // 1 cycle before data of a pulse, parameters should be reloaded
  val payload = dataType()

  assert(!(!pulseChange && modeChange), "pluseChange is necessary for modeChange")

  override def asMaster(): Unit = out(pulseChange, modeChange, payload)
}

/** pulse trigger generator
 *
 */
case class Controller() extends Component {

  // parameters
  val pulsePointsMax = 8e5.toInt
  val pulseBackMax = 125000 // TODO: how to figure out?

  // in
  val pulsePointsIn = in UInt (log2Up(pulsePointsMax - 1) bits)
  val pulseBackIn = in UInt (log2Up(pulseBackMax) bits)

  // out
  val pulse = out Bool()
  val pulseBack = out Bool()

  // states
  val pulseCounter = DynamicCounter(pulsePointsIn)
  val pulseBackCounter = Counter(pulseBackMax)

  // logic
  pulseCounter.increment() // keep working
  when(pulse || (pulseBackCounter =/= U(0)))(pulseBackCounter.increment())
  when(pulseBack)(pulseBackCounter.clear())

  pulse := pulseCounter.willOverflow
  pulseBack := pulseBackCounter === pulseBackIn
}






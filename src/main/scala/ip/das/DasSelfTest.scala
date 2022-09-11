package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DasSelfTest() extends Component {

  val dataOut0, dataOut1 = master Flow Bits(16 bits)
  val pulsesOut = PulsesOut()

  val reqCounter = CounterFreeRun(16) // speed down
  val valueCounter = Counter(64, inc = reqCounter.willOverflow)

  dataOut0.payload := valueCounter.value.resize(16).asBits
  dataOut1.payload := valueCounter.value.resize(16).asBits
  dataOut0.valid := reqCounter.willOverflow
  dataOut1.valid := reqCounter.willOverflow

  pulsesOut.Pulseout0 := reqCounter.willOverflow
  pulsesOut.Pulseout1 := reqCounter.willOverflow
  pulsesOut.Pulseout0N := False
  pulsesOut.Pulseout1N := False
  pulsesOut.Pulse_Single := reqCounter.willOverflow

}

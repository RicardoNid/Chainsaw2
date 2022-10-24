package org.datenlord
package examples

import spinal.core._
import spinal.lib._

// this example showed that you can create an "optional" component by lazy val
class OptionalCounter extends Component {

  val dataIn = in UInt (4 bits)
  val dataOut = out UInt (4 bits)

  lazy val counter = CounterFreeRun(16)
}

case class ModuleUseCounter() extends OptionalCounter {
  dataOut := counter.value
}

case class ModuleUseNoCounter() extends OptionalCounter {
  dataOut := dataIn
}

case class ModuleUseCounterTwice() extends OptionalCounter {
  dataOut := counter.value - counter.valueNext
}

object OptionalCounter extends App {
  RtlGen(ModuleUseCounter(), "counter0")
  RtlGen(ModuleUseNoCounter(), "counter1")
  RtlGen(ModuleUseCounterTwice(), "counter2")
}
package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

case class SignalPro() extends Component {

  val clkPro, rstn = in Bool()
  val dataIn = in Bits (14 bits)
  val userInterface = slave(HostInterface(14))
  val gain = out UInt(6 bits)

  val domainPro = ClockDomain(
    clock = clkPro, reset = rstn, config = dasClockConfig,
    frequency = FixedFrequency(250 MHz)
  )

  new ClockingArea(domainPro) {
    // control
    val doReg = RegInit(False)
    val gainReg = RegInit(U(63, 6 bits))
    val gaugeReg = RegInit(U(0, 8 bits))
    // control logic
    when(userInterface.ctrlEnable) {
      val value = userInterface.ctrlValue
      switch(userInterface.ctrlAddr) {
        is(U(0))(doReg := value.lsb)
        is(U(1))(gainReg := value.takeLow(6).asUInt)
        is(U(2))(gaugeReg := value.takeLow(8).asUInt)
      }
    }
    gain := gainReg
    // signal processing
    when(doReg)(userInterface.data := RegNext(dataIn))
      .otherwise(userInterface.data := RegNext(~dataIn))
  }
}

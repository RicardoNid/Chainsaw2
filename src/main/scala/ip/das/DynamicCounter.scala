package org.datenlord
package ip.das

import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class DynamicCounter(end: UInt) extends ImplicitArea[UInt] {

  val willIncrement = False.allowOverride
  val willClear = False.allowOverride

  def clear(): Unit = willClear := True

  def increment(): Unit = willIncrement := True

  def set(value:UInt) = valueNext := value

  val width = end.getBitsWidth

  val valueNext = cloneOf(end)
  val value = RegNext(valueNext) init valueNext.getZero
  val willOverflowIfInc = value === end - U(1)
  val willOverflow = willOverflowIfInc && willIncrement

  when(willOverflow) {
    valueNext := U(0)
  } otherwise {
    valueNext := (value + U(willIncrement)).resized
  }
  when(willClear) {
    valueNext := 0
  }

  willOverflowIfInc.allowPruning()
  willOverflow.allowPruning()

  override def implicitValue = value
}

object DynamicCounter {
  def apply(end: UInt): DynamicCounter = new DynamicCounter(end)

  def main(args: Array[String]): Unit = {
    SimConfig.withFstWave.compile {
      new Module {
        val dataIn = in UInt (4 bits)
        val counter = DynamicCounter(dataIn)
        counter.increment()
        val dataOut = out UInt (4 bits)
        dataOut := counter.value
      }
    }.doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.dataIn #= 3
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling(100)
      dut.dataIn #= 7
      dut.clockDomain.waitSampling(100)
      dut.dataIn #= 11
      dut.clockDomain.waitSampling(100)
    }
  }

}

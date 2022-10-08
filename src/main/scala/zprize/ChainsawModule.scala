package org.datenlord
package zprize

import org.datenlord.xilinx._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import zprize.ChainsawGenerator

case class FrameFormat()

import scala.language.postfixOps

class ChainsawModule(val gen: ChainsawGenerator) extends Module {

  import gen._

  val dataIn = slave Flow Fragment(Vec(inputWidths.map(w => Bits(w bits))))
  val dataOut = master Flow Fragment(Vec(outputWidths.map(w => Bits(w bits))))
  setDefinitionName(gen.moduleName)

  // methods for easy control
  def autoValid(): Unit = dataOut.valid := dataIn.valid.validAfter(latency)

  def autoLast(): Unit = dataOut.last := dataIn.last.validAfter(latency)

  def autoControl(): Unit = {
    autoValid()
    autoLast()
  }

  def skipControl() = {
    dataIn.valid.assignDontCare()
    dataIn.last.assignDontCare()
    dataIn.valid.allowPruning()
    dataIn.last.allowPruning()
  }
}


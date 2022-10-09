package org.datenlord
package zprize

import org.datenlord.xilinx._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import zprize.{ChainsawGenerator, FrameFormat}

import scala.language.postfixOps

class ChainsawModule(val gen: ChainsawGenerator) extends Module {

  import gen._

  val dataIn = in Vec inputWidths.map(w => Bits(w bits))
  val dataOut = out Vec outputWidths.map(w => Bits(w bits))
  setDefinitionName(gen.name)

  val validIn, lastIn = if (gen.frameFormat.needNoControl) null else in(Bool())
}

class ChainsawModuleWrapper(val gen: ChainsawGenerator) extends Module {

  import gen._

  val dataIn = slave Flow Fragment(Vec(inputWidths.map(w => Bits(w bits))))
  val dataOut = master Flow Fragment(Vec(outputWidths.map(w => Bits(w bits))))
  setDefinitionName(s"${gen.name}_dut")

  val core = gen.getImplH
  core.dataIn.zip(dataIn.fragment).zip(inputTimes).foreach { case ((corePort, dutPort), i) => corePort := dutPort.d(i) }
  val outputSpan = outputTimes.max
  dataOut.fragment.zip(core.dataOut).zip(outputTimes).foreach { case ((dutPort, corePort), i) => dutPort := corePort.d(outputSpan - i) }

  if (!gen.frameFormat.needNoControl) { // when core module need controlIn
    core.validIn := dataIn.valid
    core.lastIn := dataIn.last
  }

  if (frameFormat.needNoControl) { // fully pipelined situation
    dataOut.last := dataIn.last.validAfter(latency + outputSpan)
    dataOut.valid := dataIn.valid.validAfter(latency + outputSpan)
  }

}
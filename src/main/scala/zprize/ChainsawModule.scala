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

  val validIn, lastIn = if (gen.needNoControl) null else in(Bool())

  def bundleIn = {
    val ret = Flow(Fragment(cloneOf(dataIn)))
    ret.fragment := dataIn
    ret.valid := validIn
    ret.last := lastIn
    ret
  }

  def typedDataIn = gen.inputTypes.zip(dataIn).map { case (info, bits) =>
    val typed = info
  }

  def complexDataIn = gen.inputTypes.zip(dataIn).map { case (info, bits) =>
    val typed = info.asComplexFix()
    typed.assignFromBits(bits)
    typed
  }

  def complexDataOut = gen.outputTypes.zip(dataOut).map { case (info, bits) =>
    val typed = info.asComplexFix()
    bits := typed.asBits
    typed
  }

}

class ChainsawModuleWrapper(val gen: ChainsawGenerator) extends Module {

  import gen._

  val dataIn = slave Flow Fragment(Vec(inputWidths.map(w => Bits(w bits))))
  val dataOut = master Flow Fragment(Vec(outputWidths.map(w => Bits(w bits))))
  setDefinitionName(s"${gen.name}_dut")

  val core = gen.getImplH

  // compensation for unaligned inputs/outputs
  val trueInputTimes = if (inputTimes != null) inputTimes else Seq.fill(inputWidths.length)(0)
  val outputSpan = if (outputTimes != null) outputTimes.max else 0
  val trueOutputTimes = if (outputTimes != null) outputTimes else Seq.fill(outputWidths.length)(0)

  core.dataIn.zip(dataIn.fragment).zip(trueInputTimes).foreach { case ((corePort, dutPort), i) => corePort := dutPort.d(i) }
  dataOut.fragment.zip(core.dataOut).zip(trueOutputTimes).foreach { case ((dutPort, corePort), i) => dutPort := corePort.d(outputSpan - i) }

  if (!gen.needNoControl) { // when core module need controlIn
    core.validIn := dataIn.valid
    core.lastIn := dataIn.last
  }

  if (gen.needNoControl) { // fully pipelined situation
    dataOut.last := dataIn.last.validAfter(latency + outputSpan)
    dataOut.valid := dataIn.valid.validAfter(latency + outputSpan)
  } else { // TODO: correct logic for specific frame format
    dataOut.last := dataIn.last.validAfter(latency + outputSpan)
    dataOut.valid := dataIn.valid.validAfter(latency + outputSpan)
  }

}
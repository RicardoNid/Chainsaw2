package org.datenlord
package zprize

import org.datenlord.xilinx._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import zprize.{ChainsawGenerator, FrameFormat}

import scala.language.postfixOps

// TODO: absorb ChainsawModuleWrapper into ChainsawModule
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

  /** --------
   * connection utils
   * -------- */
  lazy val complexDataIn: Vec[ComplexFix] = Vec(gen.inputTypes.zip(dataIn).map { case (info, bits) =>
    val typed = info.asComplexFix()
    typed.assignFromBits(bits)
    typed
  })

  lazy val uintDataIn: Vec[UInt] = Vec(gen.inputTypes.zip(dataIn).map { case (info, bits) =>
    val typed = info.asUInt()
    typed.assignFromBits(bits)
    typed
  })

  lazy val complexDataOut: Vec[ComplexFix] = Vec(gen.outputTypes.zip(dataOut).map { case (info, bits) =>
    val typed = info.asComplexFix()
    bits := typed.asBits
    typed
  })

  lazy val sfixDataOut: Vec[SFix] = Vec(gen.outputTypes.zip(dataOut).map { case (info, bits) =>
    val typed = info.asSFix()
    bits := typed.asBits
    typed
  })

  lazy val uintDataOut: Vec[UInt] = Vec(gen.outputTypes.zip(dataOut).map { case (info, bits) =>
    val typed = info.asUInt()
    bits := typed.asBits
    typed
  })

  /** --------
   * inner control logic
   * -------- */
  val started = if (needNoControl) null else RegInit(False)
  if (!needNoControl) started.setWhen(lastIn)

  val localCounter =
    if (needNoControl) null else {
      val ret = CounterFreeRun(gen.inputFormat.period)
      when(lastIn)(ret.clear())
      ret
    }

  val validFrame = if (needNoControl) null else RegInit(False)
  if (!needNoControl) {
    when(lastIn)(validFrame.set())
    when(localCounter.willOverflow && ~lastIn)(validFrame.clear())
  }

  /** --------
   * control utils
   * -------- */
  def betweenTime(from: Int, until: Int) = {
    require(until - from <= period, "betweenTime can't be used to specify a range longer than the period")
    if (until >= 128) logger.warn(s"betweenTime is implemented by delay line, delay line longer than $until may not suitable for efficiency, you'd better design the control logic by yourself")
    val mark = RegInit(False)
    val aMark = lastIn.validAfter(from)
    val bMark = lastIn.validAfter(until)
    when(aMark && bMark) {} // this may also happen
      .elsewhen(aMark)(mark.set())
      .elsewhen(bMark)(mark.clear())
    mark
  }

  def atTime(time: Int) = lastIn.validAfter(time + 1)

  def beforeTime(time: Int) = betweenTime(0, time)

  def delayedValid(delay: Int) = validIn.validAfter(delay)

  def delayedLast(delay: Int) = lastIn.validAfter(delay)

  /** generate a periodic trigger synced with lastIn
   * @example this can be used to trigger inner modules running under a smaller period
   */
  def periodicTrigger(period:Int): Bool = {
    val innerCounter = CounterFreeRun(period)
    when(lastIn)(innerCounter.clear())
    (innerCounter.willOverflow && started) || lastIn
  }
}

// TODO: merge ChainsawModule and its wrapper

/** this class take a ChainsawModule as its core and generate the validOut and lastOut according to the frame format
 *
 * @param gen
 */
class ChainsawModuleWrapper(val gen: ChainsawGenerator) extends Module {

  import gen._

  val dataIn = slave Flow Fragment(Vec(inputWidths.map(w => Bits(w bits))))
  val dataOut = master Flow Fragment(Vec(outputWidths.map(w => Bits(w bits))))
  setDefinitionName(s"${gen.name}_dut")

  val core = gen.getImplH

  // compensation for unaligned inputs/outputs
  val outputSpan = actualOutTimes.max
  core.dataIn.zip(dataIn.fragment).zip(actualInTimes).foreach { case ((corePort, dutPort), i) => corePort := dutPort.d(i) }
  dataOut.fragment.zip(core.dataOut).zip(actualInTimes).foreach { case ((dutPort, corePort), i) => dutPort := corePort.d(outputSpan - i) }

  if (!needNoControl) { // when core module need controlIn
    core.validIn := dataIn.valid
    core.lastIn := dataIn.last
  }

  if (needNoControl) { // fully pipelined situation
    dataOut.last := dataIn.last.validAfter(latency + outputSpan)
    dataOut.valid := dataIn.valid.validAfter(latency + outputSpan)
  } else { // TODO: correct logic for specific frame format
    // last = start is the true controller, valid is just a mark for debugging
    dataOut.last := dataIn.last.validAfter(latency + outputSpan)
    // FIXME: generate valid, or, just leave valid alone, as inner module shouldn't rely on validIn
    dataOut.valid := True
  }

  /** --------
   * connection utils
   * -------- */
  def >>(that: ChainsawModuleWrapper) = {
    require(that.gen.inputFormat.flow.equals(this.gen.outputFormat.flow))
    this.dataOut >> that.dataIn
    that
  }

  def <<(that: ChainsawModuleWrapper) = {
    that >> this
  }

}


















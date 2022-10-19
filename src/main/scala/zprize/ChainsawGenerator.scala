package org.datenlord
package zprize

import org.datenlord
import org.datenlord.xilinx.{VivadoUtil, VivadoUtilRequirement}
import spinal.core._

import scala.language.postfixOps

trait ChainsawGenerator {

  def name: String

  /** --------
   * golden model
   * -------- */
  val impl: Seq[Any] => Seq[Any] // golden model
  val implMode: ImplMode = Comb

  /** --------
   * size information
   * -------- */
  var inputTypes: Seq[NumericTypeInfo]
  var outputTypes: Seq[NumericTypeInfo]

  /** --------
   * timing information
   * -------- */
  var inputFormat: FrameFormat
  var outputFormat: FrameFormat
  val inputTimes: Option[Seq[Int]] = None // when this is empty, inputs are aligned
  val outputTimes: Option[Seq[Int]] = None
  var latency: Int // defined as the latency from the head of inputs to the head of outputs

  /** --------
   * performance information
   * -------- */
  var utilEstimation: VivadoUtil = VivadoUtilRequirement()
  var fmaxEstimation: HertzNumber = 600 MHz

  /** --------
   * implementation
   * -------- */
  def implH: ChainsawModule // core module, that is, the datapath

  def implNaiveH: Option[ChainsawModule] = None // naive RTL implementation for simulation & top-down design

  def setAsNaive(): Unit = naiveSet += this.getClass.getSimpleName

  def useNaive: Boolean = naiveSet.contains(this.getClass.getSimpleName)

  def getImplH: ChainsawModule = {
    doDrc()
    if (useNaive) implNaiveH.getOrElse(implH)
    else implH
  }

  def implDut = new ChainsawModuleWrapper(this) // testable module, datapath + protocol

  /** --------
   * utils
   * -------- */
  def asFunc: Seq[Bits] => Seq[Bits] = (dataIn: Seq[Bits]) => {
    val core = implH
    core.dataIn := Vec(dataIn)
    core.dataOut
  }

  def asVertex(implicit ref: Dag) = DagVertex(this)

  def doDrc(): Unit = {
    assert(inputFormat.period == outputFormat.period, s"input: ${inputFormat.period}, output: ${outputFormat.period}")

    assert(actualInTimes.head == 0, s"${actualInTimes.mkString(" ")}")
    assert(actualInTimes.length == inputWidths.length)
    assert(actualInTimes.forall(_ >= 0))
    assert(actualOutTimes.head == 0)
    assert(actualOutTimes.length == outputWidths.length)
    assert(actualOutTimes.forall(_ >= 0))

    assert(latency >= 0, "invalid generator with negative latency, " +
      "do you forgot to invoke GraphDone at the end of Dag construction?")
  }

  final def inputWidths = inputTypes.map(_.bitWidth)

  final def outputWidths = outputTypes.map(_.bitWidth)

  def sizeIn: Int = inputWidths.length

  def sizeOut: Int = outputWidths.length

  def needNoControl: Boolean = inputFormat.period == 1 && outputFormat.period == 1

  def inputNoControl: FrameFormat = FrameFormat(Seq(0 until sizeIn))

  def outputNoControl: FrameFormat = FrameFormat(Seq(0 until sizeOut))

  def actualInTimes = inputTimes.getOrElse(inputTypes.map(_ => 0))

  def actualOutTimes = outputTimes.getOrElse(outputTypes.map(_ => 0))

  def period = {
    require(inputFormat.period == outputFormat.period)
    inputFormat.period
  }

  // common instantiation steps
  // TODO: how to print concrete class name?

  if (generatorList.contains(name) && !this.isInstanceOf[IoGenerator] && !this.isInstanceOf[Combinational]) {
    logger.warn(s"an identical generator $name already instantiated, you'd better reuse the existing generator")
    // throw new IllegalArgumentException(s"an identical generator $name already instantiated, you'd better reuse the existing generator")
  } else if (!generatorList.contains(name)) generatorList(name) = 0

  // generator algebra
  def +(that: ChainsawGenerator) = {
    val old = this
    new ChainsawGenerator {
      override def name = old.name + "_" + that.name

      override val impl = (dataIn: Seq[Any]) => that.impl(old.impl(dataIn))

      override var inputTypes = old.inputTypes
      override var outputTypes = that.outputTypes

      override var inputFormat = old.inputFormat
      override var outputFormat = that.outputFormat
      override var latency = old.latency + that.latency

      override def implH: ChainsawModule = new ChainsawModule(this) {
        val core0 = old.implDut
        val core1 = that.implDut
        core0.dataIn := bundleIn
        core0 >> core1
        dataOut := core1.dataOut.fragment
      }
    }
  }

}

package org.datenlord

import org.datenlord.xilinx.{VivadoUtil, VivadoUtilRequirement}
import org.datenlord.zprize._
import org.datenlord.{Comb, ImplMode, generatorList, naiveSet}
import spinal.core._

import scala.language.postfixOps


trait ChainsawGenerator {

  def name: String

  /** --------
   * golden model
   * -------- */
  def impl(dataIn: Seq[Any]): Seq[Any] // golden model

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

  def implDut = new ChainsawModuleWrapper(this) // testable module, datapath + protocol

  def implNaiveH: Option[ChainsawModule] = None // naive RTL implementation for simulation & top-down design

  def implPass: ChainsawModule = new ChainsawModule(this) {
    // TODO: general method that make output variables(rather than constants)
    dataIn.foreach(_.addAttribute("dont_touch", "yes"))
    dataOut.foreach(_.assignDontCare())
    dataOut.foreach(_.addAttribute("dont_touch", "yes"))
  }

  def setAsNaive(): Unit = naiveSet += this.getClass.getSimpleName

  def useNaive: Boolean = naiveSet.contains(this.getClass.getSimpleName)

  def getImplH: ChainsawModule = {
    doDrc()
    if (useNaive && implTime) implPass
    else if (useNaive) implNaiveH.getOrElse(implH)
    else implH
  }

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

  def actualInTimes: Seq[Int] = inputTimes.getOrElse(inputTypes.map(_ => 0))

  def actualOutTimes: Seq[Int] = outputTimes.getOrElse(outputTypes.map(_ => 0))

  def period = {
    require(inputFormat.period == outputFormat.period)
    inputFormat.period
  }

  // TODO: this should be implemented in Dag
  def ->(that: ChainsawGenerator) = {
    require(that.inputFormat.portSize == this.outputFormat.portSize, s"out ${this.outputFormat.portSize} -> in ${that.inputFormat.portSize}")
    //    require(that.inputFormat.period == this.outputFormat.period, s"prev period ${this.outputFormat.period} -> next period ${that.outputFormat.period}")
    val old = this
    new ChainsawGenerator {
      override def name = old.name + "_" + that.name

      override def impl(dataIn: Seq[Any]): Seq[Any] = that.impl(old.impl(dataIn))

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

  // TODO: flowConverters can test itself
  def testItSelf() = {}

}
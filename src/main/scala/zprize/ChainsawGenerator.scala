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
//  var inputWidths: Seq[Int]
//  var outputWidths: Seq[Int]
  var inputTypes: Seq[NumericTypeInfo]
  var outputTypes: Seq[NumericTypeInfo]

  /** --------
   * timing information
   * -------- */
  var inputFormat: FrameFormat
  var outputFormat: FrameFormat
  val inputTimes: Seq[Int] = null // when this is null, inputs are aligned
  val outputTimes: Seq[Int] = null
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

  def implNaiveH: ChainsawModule = null // naive RTL implementation for simulation & top-down design

  def setAsNaive(): Unit = naiveSet += this.getClass.getSimpleName

  def useNaive: Boolean = naiveSet.contains(this.getClass.getSimpleName)

  def getImplH: ChainsawModule = {
    doDrc()
    if (useNaive && implNaiveH != null) implNaiveH else implH
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
    if (inputTimes != null) {
      assert(inputTimes.head == 0)
      assert(inputTimes.length == inputWidths.length)
      assert(inputTimes.forall(_ >= 0))
    }
    if (outputTimes != null) {
      assert(outputTimes.head == 0)
      assert(outputTimes.length == outputWidths.length)
      assert(outputTimes.forall(_ >= 0))
    }
    assert(latency >= 0, "invalid generator with negative latency, " +
      "do you forgot to invoke GraphDone at the end of Dag construction?")
  }

  final def inputWidths = inputTypes.map(_.bitWidth)

  final def outputWidths = outputTypes.map(_.bitWidth)

  def sizeIn: Int = inputWidths.length

  def sizeOut: Int = outputWidths.length

  def needNoControl: Boolean = inputFormat.period == 1 && outputFormat.period == 1

  def inputNoControl: FrameFormat = FrameFormat(Seq(0 until sizeIn)).asInstanceOf[FrameFormat]

  def outputNoControl: FrameFormat = FrameFormat(Seq(0 until sizeOut))

  // common instantiation steps
  // TODO: how to print concrete class name?
  if (generatorList.contains(name) && !this.isInstanceOf[IoGenerator]) {
    logger.warn(s"an identical generator $name already instantiated, you'd better reuse the existing generator")
    // throw new IllegalArgumentException(s"an identical generator $name already instantiated, you'd better reuse the existing generator")
  } else if (!generatorList.contains(name)) generatorList(name) = 0

}

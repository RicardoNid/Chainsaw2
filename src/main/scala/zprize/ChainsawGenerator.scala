package org.datenlord
package zprize

import org.datenlord
import org.datenlord.xilinx.{VivadoUtil, VivadoUtilRequirement}
import spinal.core._
import zprize.FrameFormat

import scala.language.postfixOps

trait ChainsawGenerator {

  val name: String

  /** --------
   * golden model
   * -------- */
  val impl: Seq[Any] => Seq[Any] // golden model
  val implMode: ImplMode = Comb

  /** --------
   * size information
   * -------- */
  var inputWidths: Seq[Int]
  var outputWidths: Seq[Int]
  val inputType: HardType[_]
  val outputType: HardType[_]

  /** --------
   * timing information
   * -------- */
  val frameFormat: FrameFormat
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

  def implFakeH: ChainsawModule = null // naive RTL implementation for simulation & top-down design

  def setAsNaive(): Unit = naiveSet += this.getClass.getSimpleName

  def useNaive: Boolean = naiveSet.contains(this.getClass.getSimpleName)

  def getImplH: ChainsawModule = if (useNaive && implFakeH != null) implFakeH else implH

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
    assert(inputTimes.head == 0)
    assert(outputTimes.head == 0)
    assert(inputTimes.length == inputWidths.length)
    assert(outputTimes.length == outputWidths.length)
    assert(inputTimes.forall(_ >= 0))
    assert(outputTimes.forall(_ >= 0))
  }

  def sizeIn: Int = inputWidths.length

  def sizeOut: Int = outputWidths.length

  def frameNoControl: FrameFormat = FrameNoControl(sizeIn, sizeOut)

  def inputFormat: BasicDataFlow = frameFormat.inputFormat

  def outputFormat: BasicDataFlow = frameFormat.outputFormat

  // common instantiation steps
  if (generatorList.contains(hashCode()) && !this.isInstanceOf[PassThrough]) {
    // TODO: how to print concrete class name?
    logger.warn(s"an identical generator already instantiated, you'd better reuse the existing generator")
  } else generatorList(hashCode()) = 0
}

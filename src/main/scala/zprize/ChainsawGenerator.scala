package org.datenlord
package zprize

import org.datenlord
import org.datenlord.xilinx.{VivadoUtil, VivadoUtilRequirement}
import spinal.core._

trait ChainsawGenerator {

  val name: String

  // make toString result a legal verilog module name
  def moduleName = toString.replace("(", "_")
    .replace(")", "")
    .replace(" ", "_")
    .replace(",", "_")

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
  val frameFormat: FrameFormat = FrameFormat()
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
  def implH: ChainsawModule

  /** --------
   * utils
   * -------- */
  def asFunc: Seq[Bits] => Seq[Bits] = (dataIn: Seq[Bits]) => {
    val core = implH
    core.dataIn.fragment := Vec(dataIn)
    core.skipControl()
    core.dataOut.fragment
  }

  def asVertex(implicit ref: Dag) = DagVertex(this)

  def doDrc(): Unit = {
    assert(inputTimes.head == 0)
    assert(outputTimes.head == 0)
    assert(inputTimes.length == inputWidths.length)
    assert(outputTimes.length == outputWidths.length)
  }

  // common instantiation steps
  if (generatorList.contains(hashCode()) && !this.isInstanceOf[PassThrough])
    logger.warn(s"an identical generator $toString already instantiated, you'd better reuse the existing generator")
  else generatorList(hashCode()) = 0
}

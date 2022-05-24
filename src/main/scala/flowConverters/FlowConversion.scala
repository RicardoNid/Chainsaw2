package org.datenlord
package flowConverters

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object ConversionMode extends Enumeration {
  val Pass, Forward, PermByRAM = Value
  type ConversionMode = Value
}

/** Define and analyse flow conversion, select a converter scheme
 *
 * @param flowIn  format of input flow
 * @param flowOut format of output flow
 */
case class FlowConversion(flowIn: DataFlow, flowOut: DataFlow) {

  import ConversionMode._

  require(flowIn.rawDataCount == flowOut.rawDataCount, s"${flowIn.rawDataCount} != ${flowOut.rawDataCount}")

  val rawDataCount = flowIn.rawDataCount

  val isUnique = flowIn.isUnique && flowOut.isUnique

  val period = flowIn.period max flowOut.period

  def flowInPadded = flowIn.padTo(period)

  def flowOutPadded = flowOut.padTo(period)

  val isCompact = flowInPadded.isCompact && flowOutPadded.isCompact

  val isPass = flowInPadded.flow.flatten.equals(flowOutPadded.flow.flatten)

  /** Analyse the dataflow pattern and select a scheme for format converter implementation
   *
   * @return format converter implementation mode
   */
  def conversionMode = {
    if (isPass) Pass
    else if (isUnique && isCompact) PermByRAM
    else Forward
  }

  def tIns = (0 until rawDataCount).map(flowIn.getTime)

  def tZlOuts = (0 until rawDataCount).map(flowOut.getTime)

  def lifeTimeTable = LifeTimeTable(tIns, tZlOuts)

  def permutation = flowOut.flow.flatten.map(flowIn.flow.flatten.indexOf(_))

  def getConverterConfig[T <: Data](dataType: HardType[T]) = conversionMode match {
    case Forward => ForwardRegisterConverterConfig(this, dataType)
    case PermByRAM => PermutationByRamConfig(permutation, period, dataType)
  }

  def latency: Int = conversionMode match {
    case Pass => 0
    case _ => getConverterConfig(HardType(Bool())).latency
  }
}

object FlowConversion {
  def apply(prev: MeshFormat, next: MeshFormat): FlowConversion =
    new FlowConversion(prev.outputFlow, next.inputFlow)

  def apply(prev: TransformMesh, next: TransformMesh): FlowConversion =
    new FlowConversion(prev.outputFlow, next.inputFlow)
}
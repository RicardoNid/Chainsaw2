package org.datenlord
package zprize

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class OperandInfo(width: Int, weight: Int, positive: Boolean, time: Int) {
  def evaluate(data: BigInt): BigInt = {
    require(data.bitLength <= width)
    (data << weight) * (if (positive) 1 else -1)
  }

  def maxValue: BigInt = if (positive) ((BigInt(1) << width) - 1) << weight else BigInt(0)

  def <<(shiftLeft: Int) = OperandInfo(width, weight + shiftLeft, positive, time)

  def unary_- = OperandInfo(width, weight, !positive, time)
}

case class CompressorTree(operandInfos: Seq[OperandInfo]) extends ChainsawGenerator {

  override def name = s"CompressorTree_operands${operandInfos.hashCode()}".replace('-', 'N')

  override def impl(dataIn: Seq[Any])  =  {
    val ret = dataIn.asInstanceOf[Seq[BigInt]].zip(operandInfos).map { case (operand, info) => info.evaluate(operand) }.sum
    Seq(ret)
  }

  val hasNegative = operandInfos.exists(_.positive == false)
  val positiveInfos = operandInfos.filter(_.positive)
  val negativeInfos = operandInfos.filterNot(_.positive)
  val compensation = if (hasNegative) negativeInfos.map(info => ((BigInt(1) << info.width) - 1) << info.weight).sum else BigInt(0)
  val heapIn = BitHeap.getHeapFromInfos[Int](Seq(operandInfos))
  val (heapOut, solutions) = heapIn.compressAll(Gpcs(), name = s"operands of $name")
  //  val (positiveHeapOut, positiveSolutions) = BitHeap.getHeapFromInfos[Int](Seq(operandInfos.filter(_.positive))).compressAll(Gpcs(), name = s"positive operands of $name")
  //  val (negativeHeapOut, negativeSolutions) = if (hasNegative) BitHeap.getHeapFromInfos[Int](Seq(operandInfos.filterNot(_.positive))).compressAll(Gpcs(), name = s"negative operands of $name") else (positiveHeapOut, positiveSolutions)
  //  val (mergeHeapOut, mergeSolutions)       = if (hasNegative) (positiveHeapOut + negativeHeapOut).compressAll(Gpcs(), name = s"merge operands of $name") else (positiveHeapOut, positiveSolutions)
  //  val (positiveLatency, positiveWidthOut)  = (positiveSolutions.getLatency, if (positiveSolutions.getFinalWidthOut != 0) positiveSolutions.getFinalWidthOut else positiveHeapOut.width)
  //  val (negativeLatency, negativeWidthOut)  = (negativeSolutions.getLatency, if (negativeSolutions.getFinalWidthOut != 0) negativeSolutions.getFinalWidthOut else negativeHeapOut.width)
  //  val (mergeLatency, mergeWidthOut)        = if (hasNegative) (mergeSolutions.getLatency, if (mergeSolutions.getFinalWidthOut != 0) mergeSolutions.getFinalWidthOut else mergeHeapOut.width) else (0, positiveWidthOut)
  //  val cpaConfig  = CpaConfig(heapOut.width max compensation.bitLength, BinarySubtractor)
  //  val cpaLatency = cpaConfig.latency
  val outWidth = heapOut.width

  override var inputTypes = operandInfos.map(_.width).map(UIntInfo(_))
  override var outputTypes = Seq.fill(2)(UIntInfo(outWidth))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  override val inputTimes = Some(operandInfos.map(_.time))
  override var latency = operandInfos.map(_.time).min + solutions.getLatency + 1

  val implNaiveHPostDelay = latency - actualInTimes.max - 1

  //  utilEstimation = ???
  fmaxEstimation = 600 MHz

  def pipeline(data: Bool): Bool = data.d(1)

  def zero(): Bool = False

  override def implH = new ChainsawModule(this) {
    val operands = uintDataIn.zip(operandInfos).map { case (int, info) => if (info.positive) int else ~int }.map(_.d(1).asBools)
    val heapIn = BitHeap.getHeapFromInfos(Seq(operandInfos), Seq(operands))
    val heapOut = heapIn.implCompressTree(Gpcs(), solutions, pipeline, s"operands of CompressorTree_${operandInfos.hashCode()}".replace('-', 'N'))
    val rows = heapOut.output(zero).map(_.asBits().asUInt)
    uintDataOut := Vec(rows.head @@ U(0, heapOut.weightLows.head bits), rows.last @@ U(0, heapOut.weightLows.head bits))
  }

  override def implNaiveH: Option[ChainsawModule] = Option(new ChainsawModule(this) {
    val alignedWeightedInput = uintDataIn.zip(operandInfos).map { case (bits, info) =>
      bits.d(actualInTimes.max - info.time) << info.weight
    }

    val positive = alignedWeightedInput.zip(operandInfos).filter(_._2.positive).map(_._1).reduce(_ +^ _).d(implNaiveHPostDelay)
    val negativeOperands = alignedWeightedInput.zip(operandInfos).filterNot(_._2.positive).map(_._1)
    val ret = if (negativeOperands.nonEmpty) positive - negativeOperands.reduce(_ +^ _).d(implNaiveHPostDelay) else positive
    uintDataOut.head := ret.resize(outWidth).d(1)
    uintDataOut.last := U(0, outWidth bits)
  })
}

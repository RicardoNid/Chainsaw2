package org.datenlord
package arithmetic

import org.datenlord.xilinx.VivadoUtil
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class MultiOperandAdderWithSignFlagConfig(infos: Seq[ArithInfo]) extends TransformDfg {

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret     = bigInts.zip(infos).map { case (int, info) => (int << info.weight) * (if (info.isPositive) 1 else -1) }.sum
    Seq(ret)
  }

  override val size = (infos.length, 1)

  val hasNegative = infos.exists(_.isPositive == false)

  val infosPositive = infos.filter(_.isPositive)
  val infosNegative = infos.filterNot(_.isPositive)

  val (minPositiveInputTime, minNegativeInputTime) = if (hasNegative) (infosPositive.map(_.time).min, infosNegative.map(_.time).min) else (infosPositive.map(_.time).min, infosPositive.map(_.time).min)
  val paddingLatency                               = (minPositiveInputTime - minNegativeInputTime).abs
  val isPadToPositive                              = minPositiveInputTime >= minNegativeInputTime

  val (retBitHeapPositive, solutionsPositive) = BitHeap.getHeapFromInfos[Int](Seq(infosPositive)).compressAll(Gpcs(), name = "compressor tree of positive operands")
  val (latencyPositive, widthOutPositive)     = (solutionsPositive.getLatency, if (solutionsPositive.getFinalWidthOut != 0) solutionsPositive.getFinalWidthOut else retBitHeapPositive.width)

  val (retBitHeapNegative, solutionsNegative) = if (hasNegative) BitHeap.getHeapFromInfos[Int](Seq(infosNegative)).compressAll(Gpcs(), name = "compressor tree of negative operands") else (retBitHeapPositive, solutionsPositive)
  val (latencyNegative, widthOutNegative)     = (solutionsNegative.getLatency, if (solutionsNegative.getFinalWidthOut != 0) solutionsNegative.getFinalWidthOut else retBitHeapNegative.width)

  val (retBitHeapMerge, solutionsMerge) = if (hasNegative) (retBitHeapPositive + retBitHeapNegative).compressAll(Gpcs(), name = "compressor tree of merge operands") else (retBitHeapPositive, solutionsPositive)
  val (latencyMerge, widthOutMerge)     = if (hasNegative) (solutionsMerge.getLatency, if (solutionsMerge.getFinalWidthOut != 0) solutionsMerge.getFinalWidthOut else retBitHeapMerge.width) else (0, widthOutPositive)

  // for negative operands, we use its bitwise inversion instead, and make compensation by the final CPA
  // example: -1010 = 0101 - 1111, compensation = -1111
  var compensation = BigInt(0)
  if (hasNegative) {
    compensation = BitHeap
      .getInfosFromBitHeap(retBitHeapNegative)
      .head
      .map(info => ((BigInt(1) << info.width) - 1) << info.weight)
      .sum
  }

  val cpaConfig  = CpaWithSignFlagConfig(widthOutMerge max (compensation >> retBitHeapMerge.weightLows.head).bitLength, if (hasNegative) TernaryAdder else BinaryAdder, if (hasNegative) 1 else 0) // final two rows - compensation
  val cpaLatency = cpaConfig.latency

  override val name      = "MultiOperandAdderWithSignFlag"
  override val opType    = Compressor
  override val widthsIn  = infos.map(_.width)
  override val widthsOut = Seq(cpaConfig.widthOut)

  override def latency: Int = if (hasNegative) {
    if (isPadToPositive) ((paddingLatency + latencyPositive) min latencyNegative) + latencyMerge + cpaLatency + 1
    else (latencyPositive min (paddingLatency + latencyNegative)) + latencyMerge + cpaLatency + 1
  } else {
    latencyPositive + cpaLatency + 1
  }

  def pipeline(data: Bool): Bool = data.d(1)

  def zero(): Bool = False

  logger.info(
    s"\n----latency report of bit heap compressor----" +
      s"\n\tlatency of Bit Heap Compressor: $latency" +
      s"\n\tCSA latency: ${latency - cpaLatency - 1}, CPA latency: $cpaLatency"
  )

  override def implH = MultiOperandAdderWithSignFlag(this)
}

case class MultiOperandAdderWithSignFlag(config: MultiOperandAdderWithSignFlagConfig) extends TransformModule[UInt, UInt] with SignFlagPort {

  import config._

  override val dataIn     = slave Flow Fragment(Vec(infos.map(info => UInt(info.width bits))))
  override val dataOut    = master Flow Fragment(Vec(UInt(), size._2))
  override val isPositive = out Bool ()

  if (!hasNegative) {
    val operands          = dataIn.fragment.map(_.asBools)
    val heapIn            = BitHeap.getHeapFromInfos(Seq(infos), Seq(operands))
    val heapOut           = heapIn.implCompressTree(Gpcs(), solutionsPositive, pipeline, name = "compressor tree of positive operands")
    val rows              = heapOut.output(zero).map(_.asBits().asUInt)
    val (cpaResult, sign) = cpaConfig.asFuncWithSignFlag(Seq(rows.head, rows.last))
    val positiveResult    = (cpaResult.head @@ U(0, heapOut.weightLows.head bits)).d(1)
    val signResult        = sign.d(1)
    dataOut.fragment.head := positiveResult
    isPositive            := signResult
  } else {
    val operandsPositive = dataIn.fragment.map(_.asBools).zip(infos).filter(_._2.isPositive).map(_._1)
    val operandsNegative = dataIn.fragment.map(_.asBools).zip(infos).filterNot(_._2.isPositive).map(_._1)

    val bitHeapPositive = BitHeap.getHeapFromInfos(Seq(infosPositive), Seq(operandsPositive))
    val bitHeapNegative = BitHeap.getHeapFromInfos(Seq(infosNegative), Seq(operandsNegative))

    val retPositive = bitHeapPositive.implCompressTree(Gpcs(), solutionsPositive, pipeline, name = "compressor tree of positive operands")
    val retNegative = bitHeapNegative.implCompressTree(Gpcs(), solutionsNegative, pipeline, name = "compressor tree of negative operands")

    val retMerge          = (retPositive + retNegative.d(~_)).implCompressTree(Gpcs(), solutionsMerge, pipeline, name = "compressor tree of merge operands")
    val rows              = retMerge.output(zero).map(_.asBits().asUInt)
    val (cpaResult, sign) = cpaConfig.asFuncWithSignFlag(Seq(rows.head, rows.last, compensation >> retMerge.weightLows.head))
    val positiveResult    = (cpaResult.head @@ U(0, retMerge.weightLows.head bits)).d(1)
    val negativeResult    = (~(cpaResult.head @@ U(0, retMerge.weightLows.head bits)) + U(1)).d(1)
    val signResult        = sign.d(1)
    when(signResult) { dataOut.fragment.head := positiveResult }
      .otherwise { dataOut.fragment.head := negativeResult }
    isPositive := signResult
  }

  autoValid()
  autoLast()
}

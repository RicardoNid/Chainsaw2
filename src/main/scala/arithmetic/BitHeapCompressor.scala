package org.datenlord
package arithmetic

import spinal.core.{out, _}
import spinal.lib._

import scala.language.postfixOps

/** the hardware module using [[BitHeap]] model to implement multi operand addition
 *
 * @param infos
 *   width and position(shift) of input operands
 */
case class BitHeapCompressorConfig(infos: Seq[ArithInfo]) extends TransformBase {

  val hasNegative = infos.exists(_.isPositive == false)

  override def impl(dataIn: Seq[Any]) = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret     = bigInts.zip(infos).map { case (int, info) => (int << info.weight) * (if (info.isPositive) 1 else -1) }.sum
    Seq(ret)
  }

  override val size = (infos.length, if (hasNegative) 4 else 2)

  val infosPositive = infos.filter(_.isPositive)
  val infosNegative = infos.filterNot(_.isPositive)

  val (retBitHeapPositive, solutionsPositive) = BitHeap.getHeapFromInfos[Int](Seq(infosPositive)).compressAll(Gpcs(), name = "compressor tree of positive operands")
  val (latencyPositive, widthOutPositive)     = (solutionsPositive.getLatency, if (solutionsPositive.getFinalWidthOut != 0) solutionsPositive.getFinalWidthOut else retBitHeapPositive.width)

  val (retBitHeapNegative, solutionsNegative) = if (hasNegative) BitHeap.getHeapFromInfos[Int](Seq(infosNegative)).compressAll(Gpcs(), name = "compressor tree of negative operands") else (retBitHeapPositive, solutionsPositive)
  val (latencyNegative, widthOutNegative)     = (solutionsNegative.getLatency, if (solutionsNegative.getFinalWidthOut != 0) solutionsNegative.getFinalWidthOut else retBitHeapNegative.width)

  override def latency = latencyPositive max latencyNegative

  def widthOut = widthOutPositive max widthOutNegative

  override def implH = BitHeapCompressor(this)

  def pipeline(data: Bool) = data.d(1)

  def zero() = False

  def metric(yours: Seq[BigInt], golden: Seq[BigInt]) = {
    if (hasNegative) yours.take(2).sum - yours.takeRight(2).sum == golden.head
    else yours.sum == golden.head
  }

  def naiveImplH: Module = new Module {
    val dataIn   = in Vec infos.map(info => UInt(info.width bits))
    val dataOut  = out UInt ()
    val add      = (a: UInt, b: UInt)    => a +^ b
    val pipeline = (a: UInt, level: Int) => a.d(1)
    dataOut := dataIn.reduceBalancedTree(add, pipeline)
  }
}

case class BitHeapCompressor(config: BitHeapCompressorConfig) extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn  = slave Flow Fragment(Vec(infos.map(info => UInt(info.width bits))))
  override val dataOut = master Flow Fragment(Vec(UInt(), size._2))

  if (!hasNegative) {
    val operands = dataIn.fragment.map(_.asBools)
    val bitHeap  = BitHeap.getHeapFromInfos(Seq(infos), Seq(operands))
    val ret      = bitHeap.implCompressTree(Gpcs(), solutionsPositive, pipeline, name = "compressor tree of positive operands")
    dataOut.fragment := ret.output(zero).map(_.asBits().asUInt)
  } else {
    val operandsPositive = dataIn.fragment.map(_.asBools).zip(infos).filter(_._2.isPositive).map(_._1)
    val operandsNegative = dataIn.fragment.map(_.asBools).zip(infos).filterNot(_._2.isPositive).map(_._1)

    val bitHeapPositive = BitHeap.getHeapFromInfos(Seq(infosPositive), Seq(operandsPositive))
    val bitHeapNegative = BitHeap.getHeapFromInfos(Seq(infosNegative), Seq(operandsNegative))

    val retPositive = bitHeapPositive.implCompressTree(Gpcs(), solutionsPositive, pipeline, name = "compressor tree of positive operands")
    val retNegative = bitHeapNegative.implCompressTree(Gpcs(), solutionsNegative, pipeline, name = "compressor tree of negative operands")

    val compensatePositive = latency - latencyPositive
    val compensateNegative = latency - latencyNegative

    val uintPositive = retPositive.output(zero).map(_.asBits().asUInt.d(compensatePositive))
    val uintNegative = retNegative.output(zero).map(_.asBits().asUInt.d(compensateNegative))

    dataOut.fragment := Vec(uintPositive ++ uintNegative)
  }

  autoValid()
  autoLast()
}

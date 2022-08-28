package org.datenlord
package arithmetic

import dfg.ArithInfo

import spinal.core._
import spinal.lib._

case class BitHeapCompressorUseInversionConfig(infos: Seq[ArithInfo]) extends TransformBase {

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = bigInts.zip(infos).map { case (int, info) => (int << info.shift) * (if (info.sign) 1 else -1) }.sum
    Seq(ret)
  }

  override val size = (infos.length, 1)

  // constant that must be subtracted in the end
  val compensation = infos.filter(_.sign == false) // the true compensation
    .map(info => ((BigInt(1) << info.width) - 1) << info.shift)
    .map(_ * 1).sum

  val (_, fixedLatency, widthOut) = BitHeap.getFakeHeapFromInfos(infos)
    .compressAll(GPC())

  override def latency = fixedLatency

  logger.info(s"latency of Bit Heap Compressor: $latency")

  override def implH = BitHeapCompressorUseInversion(this)

  def pipeline(data: Bool): Bool = data.d(1)

  def zero(): Bool = False
}

case class BitHeapCompressorUseInversion(config: BitHeapCompressorUseInversionConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(infos.map(info => UInt(info.width bits))))
  override val dataOut = master Flow Fragment(Vec(UInt(), size._2))

  val operands = dataIn.fragment.zip(infos)
    .map { case (int, info) => if (info.sign) int else ~int }
    .map(_.asBools)

  val bitHeap = BitHeap.getHeapFromInfos(infos, operands)
  val (ret, _, _) = bitHeap.compressAll(GPC(), pipeline)

  val rows = ret.output(zero).map(_.asBits().asUInt)
  dataOut.fragment.head := rows(0) +^ rows(1) - compensation

  autoValid()
  autoLast()
}

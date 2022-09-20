package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class BitHeapCompressorUseInversionConfig(infos: Seq[ArithInfo])
  extends TransformDfg {

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = bigInts.zip(infos).map { case (int, info) => (int << info.weight) * (if (info.sign) 1 else -1) }.sum
    Seq(ret)
  }

  override val size = (infos.length, 1)

  // for negative operands, we use its bitwise inversion instead, and make compensation by the final CPA
  // example: -1010 = 0101 - 1111, compensation = -1111
  val compensation = infos.filter(_.sign == false) // negative operands
    .map(info => ((BigInt(1) << info.width) - 1) << info.weight)
    .map(_ * 1).sum

  val (_, csaLatency, widthOut) = BitHeap.getFakeHeapFromInfos(infos)
    .compressAll(GPC())
  val cpaConfig = CpaConfig(widthOut, TernaryAdder, 1) // final two rows - compensation
  val cpaLatency = cpaConfig.latency

  override val name = "compressor"
  override val opType = Compressor
  override val widthsIn = infos.map(_.width)
  override val widthsOut = Seq(widthOut)

  override def latency: Int = csaLatency + cpaLatency + 1 // 1 for inversion

  logger.info(
    s"\n----latency report of bit heap compressor----" +
      s"\n\tlatency of Bit Heap Compressor: $latency" +
      s"\n\tCSA latency: $csaLatency, CPA latency: $cpaLatency")

  override def implH = BitHeapCompressorUseInversion(this)
}

case class BitHeapCompressorUseInversion(config: BitHeapCompressorUseInversionConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(infos.map(info => UInt(info.width bits))))
  override val dataOut = master Flow Fragment(Vec(UInt(), size._2))

  // build operands through bitwise inversion
  val operands = dataIn.fragment.zip(infos)
    .map { case (int, info) => if (info.sign) int else ~int }
    .map(_.d(1).asBools)

  def pipeline(data: Bool): Bool = data.d(1)

  def zero(): Bool = False

  val bitHeap = BitHeap.getHeapFromInfos(infos, operands)
  val (ret, _, _) = bitHeap.compressAll(GPC(), pipeline)

  // two rows from
  val rows = ret.output(zero).map(_.asBits().asUInt)

  dataOut.fragment.head := cpaConfig.asFunc(Seq(rows.head, rows.last, compensation >> ret.weightLow)).head @@ U(0, ret.weightLow bits)

  autoValid()
  autoLast()
}

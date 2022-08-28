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

  // TODO: use positive compensation, instead of
  // constant that must be subtracted
  val weightLow = infos.map(_.shift).min
  val compensation = infos.filter(_.sign == false)
    .map(info => ((BigInt(1) << info.width) - 1) << info.shift)
    .map(_ * 1).sum

  val negativeCompensation = BigInt(1) << compensation.bitLength
  val positiveCompensation: Seq[Int] = (negativeCompensation - compensation).toString(2).map(_.asDigit).dropRight(weightLow)
  logger.info(s"compensation: $compensation")
  logger.info(s"negative compensation: $negativeCompensation")
  logger.info(s"positive compensation: ${BigInt(positiveCompensation.mkString(""), 2)}")

  infos.filter(_.sign == false).foreach(info => logger.info(s"${info.width}, ${info.shift}"))

  val (_, fixedLatency, widthOut) = BitHeap.getFakeHeapFromInfos(infos)
    .addConstant(positiveCompensation.reverse, 0)
    .compressAll(GPC())

  override def latency = fixedLatency

  logger.info(s"latency of UInt Compressor = $latency")

  override def implH = BitHeapCompressorUseInversion(this)

  def pipeline(data: Bool) = data.d(1)

  def zero() = False
}

case class BitHeapCompressorUseInversion(config: BitHeapCompressorUseInversionConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(infos.map(info => UInt(info.width bits))))
  override val dataOut = master Flow Fragment(Vec(UInt(), size._2))

  val operands = dataIn.fragment.zip(infos)
    .map { case (int, info) => if (info.sign) int else ~int }
    .map(_.asBools)
  logger.info("caution")
  println(s"positive ${positiveCompensation.mkString("")}")
  print(160.toBinaryString)
  val bitHeap = BitHeap.getHeapFromInfos(infos, operands)
    .addConstant(positiveCompensation.reverse.map(int => if (int == 1) True else False), False)
  logger.info(s"bit heap base = ${bitHeap.weightLow}")
  val (ret, _, _) = bitHeap.compressAll(GPC(), pipeline)

  // TODO: better CPA
  val rows = ret.output(zero).map(_.asBits().asUInt)
  dataOut.fragment.head := rows(0) +^ rows(1) - negativeCompensation

  autoValid()
  autoLast()
}

package org.datenlord
package arithmetic

import org.datenlord.xilinx.VivadoUtil
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class BitHeapCompressorUseInversionWithSignFlagConfig(infos: Seq[ArithInfo]) extends TransformDfg {

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret     = bigInts.zip(infos).map { case (int, info) => (int << info.weight) * (if (info.isPositive) 1 else -1) }.sum
    Seq(ret)
  }

  override val size = (infos.length, 1)

  val hasNegative = infos.exists(_.isPositive == false)

  // for negative operands, we use its bitwise inversion instead, and make compensation by the final CPA
  // example: -1010 = 0101 - 1111, compensation = -1111
  var compensation = BigInt(0)
  if (hasNegative) {
    compensation = infos
      .filter(_.isPositive == false) // negative operands
      .map(info => ((BigInt(1) << info.width) - 1) << info.weight).sum
  }

  val (retBitHeap, solutions) = BitHeap.getHeapFromInfos[Int](Seq(infos)).compressAll(Gpcs(), name = "compressor tree for config")
  val (csaLatency, widthOut)  = (solutions.getLatency, if (solutions.getFinalWidthOut != 0) solutions.getFinalWidthOut else retBitHeap.width)

  val cpaConfig  = CpaWithSignFlagConfig(widthOut max (compensation >> retBitHeap.weightLows.head).bitLength, if (hasNegative) TernaryAdder else BinaryAdder, if (hasNegative) 1 else 0) // final two rows - compensation
  val cpaLatency = cpaConfig.latency

  override val name      = "BitHeapCompressorUseInversion"
  override val opType    = Compressor
  override val widthsIn  = infos.map(_.width)
  override val widthsOut = Seq(widthOut)

  override def latency: Int = csaLatency + cpaLatency + 2 // 1 for inversion

  logger.info(
    s"\n----latency report of bit heap compressor----" +
      s"\n\tlatency of Bit Heap Compressor: $latency" +
      s"\n\tCSA latency: $csaLatency, CPA latency: $cpaLatency"
  )

  override def implH = BitHeapCompressorUseInversionWithSignFlag(this)
}

case class BitHeapCompressorUseInversionWithSignFlag(config: BitHeapCompressorUseInversionWithSignFlagConfig) extends TransformModule[UInt, UInt] with SignFlagPort {

  import config._

  override val dataIn     = slave Flow Fragment(Vec(infos.map(info => UInt(info.width bits))))
  override val dataOut    = master Flow Fragment(Vec(UInt(), size._2))
  override val isPositive = out Bool ()

  // build operands through bitwise inversion
  val operands = dataIn.fragment
    .zip(infos)
    .map { case (int, info) => if (info.isPositive) int else ~int }
    .map(_.d(1).asBools)

  def pipeline(data: Bool): Bool = data.d(1)

  def zero(): Bool = False

  val bitHeap = BitHeap.getHeapFromInfos(Seq(infos), Seq(operands))
  val ret     = bitHeap.implCompressTree(Gpcs(), solutions, pipeline, name = "bitHeap compressor tree use inversion")

  // two rows from
  val rows = ret.output(zero).map(_.asBits().asUInt)

  val (cpaResult, sign) = cpaConfig.asFuncWithSignFlag(if (hasNegative) Seq(rows.head, rows.last, compensation >> ret.weightLows.head) else Seq(rows.head, rows.last))

  val positiveResult = (cpaResult.head @@ U(0, ret.weightLows.head bits)).d(1)
  val negativeResult = (~(cpaResult.head @@ U(0, ret.weightLows.head bits)) + U(1)).d(1)
  val signResult     = sign.d(1)
  when(signResult) { dataOut.fragment.head := positiveResult }
    .otherwise { dataOut.fragment.head := negativeResult }
  isPositive := signResult

  autoValid()
  autoLast()
}

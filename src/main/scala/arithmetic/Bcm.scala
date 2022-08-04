package org.datenlord
package arithmetic

import arithmetic.McmType._
import dfg.ArithInfo

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** Big constant multiplier implemented by MCM and BMC
 */
case class BcmConfig(coeff: BigInt, inWidth: Int) extends TransformBase {
  override def impl(dataIn: Seq[Any]) =
    dataIn.asInstanceOf[Seq[BigInt]].map(_ * coeff)

  override val size = (1, 1)

  val solverLimit = 31
  val adderLimit = 126 - 31

  val coeffWords = coeff.toWords(solverLimit) // low to high
  val dataWidths = Seq.fill(inWidth)(1).grouped(adderLimit).toSeq.map(_.sum)
  val dataPositions = dataWidths.scan(0)(_ + _).init

  val adderGraphConfigs = dataWidths.map(width => McmConfig(coeffWords, width, PAG))

  val arithInfos = dataWidths.zip(dataPositions).flatMap { case (width, position) =>
    val prodWidths = coeffWords.map(coeffWord => coeffWord.bitLength + width)
    val shifts = coeffWords.indices.map(i => i * solverLimit + position)
    prodWidths.zip(shifts).map { case (width, shift) => ArithInfo(width, shift) }
  }

  //  arithInfos.foreach(info => logger.info(s"arith info: $info"))

  val compressorConfig = BmcConfig(arithInfos)
  val compressorLatency = compressorConfig.latency
  val adderGraphLatency = adderGraphConfigs.map(_.latency).max

  override def latency = adderGraphLatency + compressorLatency

  override def implH = Bcm(this)
}

case class Bcm(config: BcmConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(inWidth bits), 1))
  override val dataOut = master Flow Fragment(Vec(UInt(inWidth + coeff.bitLength bits), 1))

  val dataWords = dataIn.fragment.head.asBits.asBools.grouped(adderLimit).toSeq.map(_.asBits.asUInt)
  dataWords.foreach(word => logger.info(s"dataWord length: ${word.getBitsWidth}"))

  val partialProds = dataWords.zip(adderGraphConfigs)
    .flatMap { case (dataWord, config) =>
      config.implH.asNode(Seq(dataWord)).map(_.d(adderGraphLatency - config.latency))
    }.map(_.resized)

  val rets = compressorConfig.implH.asNode(partialProds)

  // TODO: output should be registered
  dataOut.fragment.head := (rets(0) +^ rets(1)).resized
  autoValid()
  autoLast()
}

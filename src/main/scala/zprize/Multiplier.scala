package org.datenlord
package zprize

import org.datenlord.xilinx.VivadoUtilRequirement
import spinal.core._

import scala.language.postfixOps

case class Multiplier(widths: Seq[Int]) extends ChainsawGenerator {

  require(widths.forall(_ <= 34) && widths.forall(_ > 17))

  override def name = s"mult_${widths.mkString("_")}"

  override val impl = (dataIn: Seq[Any]) => {
    val Seq(a, b) = dataIn.asInstanceOf[Seq[BigInt]]
    require(a.bitLength <= widths.head, b.bitLength <= widths.last)
    Seq(a * b)
  }

  override var inputTypes = widths.map(UIntInfo(_))
  override var outputTypes = Seq(UIntInfo(widths.sum))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = 6

  utilEstimation = VivadoUtilRequirement(dsp = 3)
  fmaxEstimation = 600 MHz

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val karaCore = Karabase(17, 17).implH

    val Seq(a, b) = dataIn
    val (aHigh, aLow) = a.splitAt(17)
    val (bHigh, bLow) = b.splitAt(17)

    karaCore.dataIn := Seq(aHigh, aLow, bHigh, bLow).map(_.resized)
    val Seq(high, cross, low) = karaCore.dataOut

    uintDataOut.head := ((high.asUInt.d(3) << 34) + (cross.asUInt << 17) + low.asUInt.d(3)).resize(widths.sum).d(1)
  }

  override def implNaiveH: Some[ChainsawModule] = Some(new ChainsawModule(this) {
    uintDataOut.head := uintDataIn.reduce(_ * _).d(latency)
  })

}

package org.datenlord
package zprize

import org.datenlord.xilinx.VivadoUtilRequirement
import spinal.core._

import scala.language.postfixOps

case class Multiplier(widths: Seq[Int]) extends ChainsawGenerator {

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

  utilEstimation = VivadoUtilRequirement(dsp = 3, ff = 128, lut = 128)
  fmaxEstimation = 600 MHz

  override def implH = null

  override def implNaiveH: Some[ChainsawModule] = Some(new ChainsawModule(this) {
    uintDataOut.head := uintDataIn.reduce(_ * _).d(latency)
  })

}

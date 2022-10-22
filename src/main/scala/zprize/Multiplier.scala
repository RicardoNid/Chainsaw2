package org.datenlord
package zprize

import org.datenlord.xilinx.VivadoUtilRequirement
import spinal.core._

import scala.language.postfixOps

/** unsigned multiplier implemented by dedicated methods, according to its size & shape
 *
 * @param widths widths of two operands
 */
case class Multiplier(widths: Seq[Int]) extends ChainsawGenerator {

  val multMode =
    if (widths.forall(_ <= 34) && widths.forall(_ >= 25)) 0 // double-size
    else if (widths.forall(_ == 18)) 1 // single-size with tiling
    else if (widths.forall(_ <= 17)) 2 // single-size
    else -1

  if (verbose >= 1) logger.info(s"generating multiplier in mode $multMode")

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

  override var latency = multMode match {
    case 0 => 6
    case 1 => 2
    case 2 => 1
  }

  fmaxEstimation = 600 MHz

  override def implH: ChainsawModule = new ChainsawModule(this) {

    multMode match {
      case 0 => // double-size multiplier implemented by
        val karaCore = Karabase(17, 17).implH
        val Seq(a, b) = dataIn
        val (aHigh, aLow) = a.splitAt(17)
        val (bHigh, bLow) = b.splitAt(17)
        karaCore.dataIn := Seq(aHigh, aLow, bHigh, bLow).map(_.resized)
        val Seq(high, cross, low) = karaCore.dataOut
        uintDataOut.head := ((high.asUInt.d(3) << 34) + (cross.asUInt << 17) + low.asUInt.d(3)).resize(widths.sum).d(1)
      case 1 =>
        // TODO: avoid implementing C+A*B in a single cycle, Vivado retiming won't help
        uintDataOut.head := uintDataIn.map(_.d(1)).reduce(_ * _).d(1) // implemented as C+A*B
      case 2 =>
        uintDataOut.head := uintDataIn.reduce(_ * _).d(1) // implemented as C+A*B
    }
  }

  override def implNaiveH: Some[ChainsawModule] = Some(new ChainsawModule(this) {
    uintDataOut.head := uintDataIn.reduce(_ * _).d(latency)
  })

}

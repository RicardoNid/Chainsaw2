package org.datenlord
package zprize

import org.datenlord.xilinx.VivadoUtilRequirement
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps


/** dedicated DSP implementation of karatsuba base transform which takes four operands and generate three(high, cross, low)
 *
 * @param widthA widths of high and low from operand A
 * @param widthB widths of high and low from operand B
 */
case class Karabase(widthA: Int, widthB: Int) extends ChainsawGenerator {

  require(Seq(widthA, widthB).sorted.zip(Seq(17, 26)).forall { case (yours, limit) => yours <= limit })

  override def name = s"karabase_${widthA}_$widthB"

  override val impl = (dataIn: Seq[Any]) => {
    val Seq(aH, aL, bH, bL) = dataIn.asInstanceOf[Seq[BigInt]]
    Seq(aH * bH, aH * bL + aL * bH, aL * bL)
  }

  override var inputTypes = Seq(widthA, widthA, widthB, widthB).map(UIntInfo(_))
  val widthCross = widthA + widthB
  override var outputTypes = Seq(widthCross, widthCross + 1, widthCross).map(UIntInfo(_))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  override val outputTimes = Some(Seq(0, 3, 0))
  override var latency = 2

  utilEstimation = VivadoUtilRequirement(dsp = 3)
  fmaxEstimation = 600 MHz

  override def implH: ChainsawModule = new ChainsawModule(this) {
    val Seq(aHigh, aLow, bHigh, bLow) = uintDataIn
    // TODO: absorb aHigh - aLow in pre-adder of dsp0, using blackbox?
    // 0-1
    val aMerge = (aHigh.intoSInt -^ aLow.intoSInt).d(1) // FIXME: make it inside the DSP
    val bMerge = (bHigh -^ bLow).asSInt.d(1) // outside
    // 0-2
    val high = (aHigh.intoSInt * bHigh.intoSInt).d(2) // dsp1
    val low = (aLow.intoSInt * bLow.intoSInt).d(2) // dsp2
    // 2-3, dsp0
    val mid0 = (aMerge.d(1) * bMerge.d(1)).d(1)
    // 3-4
    val mid1 = (high.d(1) - mid0).d(1) // 4-5 post-adder
    // 4-5
    val mid = (low.d(2) + mid1).d(1)
    uintDataOut := Seq(high.asUInt.resized, mid.asUInt.resized, low.asUInt.resized)
  }
}

package org.datenlord
package dsp

import breeze.math._
import org.datenlord.zprize.CommonAlgos.WNnk
import org.datenlord.{ChainsawGenerator, ChainsawModule, ComplexFix, NumericTypeInfo, SFConstant}
import spinal.core._

import scala.language.postfixOps

case class Twiddle(N: Int, nk: Int, dataType: NumericTypeInfo, coeffWidth: Int) extends ChainsawGenerator {

  // get a legal index
  var positiveIndex = nk
  while (positiveIndex < 0) positiveIndex += N

  override def name = s"twiddle_${positiveIndex}_by_$N"

  override def impl(dataIn:Seq[Any]): Seq[Complex] = dataIn.asInstanceOf[Seq[Complex]].map(_ * WNnk(N, nk))
  override var inputTypes = Seq(dataType)
  override var outputTypes = Seq(dataType)

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = 6 // latency of complex multiplication

  override def implH: ChainsawModule = new ChainsawModule(this) {

    /** optimized multiplications with twiddle factors of fourier transform
     */
    val data = complexDataIn.head
    val sfixType = HardType(data.real)
    val coeffType = HardType(SFix(1 exp, -(coeffWidth - 2) exp))

    def toFixedCoeff: Double => SFix = SFConstant(_, coeffType)

    // when position is i * (2*pi/4), latency = 1
    def toQuadrant(signal: ComplexFix, quadrant: Int): ComplexFix = {
      quadrant match {
        case 0 => signal.d(1)
        case 1 => (-signal.multiplyI).d(1)
        case 2 => (-signal).d(1)
        case 3 => signal.multiplyI.d(1)
      }
    }

    // when position is i * (2*pi/4) + (2*pi/8), latency = 2
    def multiply1MinusJ(signal: ComplexFix): ComplexFix = {
      val sqrt2Factor = toFixedCoeff(1 / scala.math.sqrt(2.0)) // * factor = / sqrt2
      import signal._
      ComplexFix(
        ((real +^ imag).d(1) * sqrt2Factor).d(1),
        ((imag -^ real).d(1) * sqrt2Factor).d(1)
      )
    }

    val isSpecial = N % 8 == 0 && positiveIndex % (N / 8) == 0

    val ret = if (isSpecial) {
      val position = positiveIndex / (N / 8)
      val mode = position % 2
      val quadrant = position / 2

      val mode0Compensation = latency - 1
      val mode1Compensation = latency - 3

      mode match {
        case 0 => toQuadrant(data, quadrant).d(mode0Compensation)
        case 1 => multiply1MinusJ(toQuadrant(data, quadrant)).truncate(sfixType).d(mode1Compensation)
        case _ =>
          val coeffValue = WNnk(N, nk)
          val coeff = ComplexFix(toFixedCoeff(coeffValue.real), toFixedCoeff(coeffValue.imag))
          data * coeff.truncate(sfixType)
      }
    } else {
      val coeffValue = WNnk(N, nk)
      val coeff = ComplexFix(toFixedCoeff(coeffValue.real), toFixedCoeff(coeffValue.imag))
      (data * coeff).truncate(sfixType)
    }

    complexDataOut.head := ret
  }
}

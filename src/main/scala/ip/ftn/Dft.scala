package org.datenlord
package ip.ftn

import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.signal.{fourierTr, iFourierTr}
import org.datenlord.{ComplexFix, SFConstant}
import spinal.core._

import scala.language.postfixOps

case class Dft(N: Int, inverse: Boolean, dataType: NumericTypeInfo, coeffWidth: Int)
  extends ChainsawGenerator {

  val prefix = if (inverse) "idft" else "dft"

  override def name = s"$prefix$N"

  override val impl = (dataIn: Seq[Any]) => {
    val data = dataIn.asInstanceOf[Seq[Complex]].toArray
    if (inverse) iFourierTr.dvComplexIFFT(DenseVector(data)).toArray.toSeq.map(_ * N)
    else fourierTr.dvComplex1DFFT(DenseVector(data)).toArray.toSeq
  }

  override var inputTypes = Seq.fill(N)(dataType)
  override var outputTypes = Seq.fill(N)(dataType)

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = N match {
    case 2 => 1
    case 4 => 2
    case 8 => 5
  }

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val coeffType = HardType(SFix(1 exp, -(coeffWidth - 2) exp))

    /** --------
     * utils for datapath description
     * -------- */
    def butterfly(A: ComplexFix, B: ComplexFix) = Seq(A + B, A - B)

    // get idft result
    def getFftSym(data: Seq[ComplexFix]): Seq[ComplexFix] = {
      val N = data.length
      val mid = Seq(data(N / 2))
      val seg0 = data.slice(1, N / 2)
      val seg1 = data.slice(N / 2 + 1, N)
      Seq(data.head) ++ seg1.reverse ++ mid ++ seg0.reverse
    }

    /** --------
     * datapath description
     * -------- */
    val ret: Seq[ComplexFix] = N match {
      case 2 => butterfly(complexDataIn.head, complexDataIn.last)

      case 4 =>
        val Seq(a, b, c, d) = complexDataIn
        val Seq(e, f, g, h) = Seq(a + c, b + d, a - c, b - d).map(_.d(1))
        val ret = Seq(e + f, g - h.multiplyI, e - f, g + h.multiplyI)
        if (!inverse) ret else getFftSym(ret)

      case 8 =>

        val s0 = { // stage 0, latency +1
          val Seq(a, b, c, d, e, f, g, h) = complexDataIn
          Seq(a + e, b + f, c + g, d + h, a - e, b - f, c - g, d - h).map(_.d(1))
        }

        val s1 = { // stage 1, latency +2
          val Seq(a, b, c, d, e, f, g, h) = s0
          val sqrt2coeff = SFConstant(1 / scala.math.sqrt(2), coeffType)
          Seq(
            (a + c).d(2), (b + d).d(2),
            (a - c).d(2), (b - d).d(2),
            e.d(2),
            ((f + h).d(1) * sqrt2coeff).d(1).truncate(dataType.asSFix),
            g.d(2),
            ((f - h).d(1) * sqrt2coeff).d(1).truncate(dataType.asSFix))
        }

        val s2 = { // stage 2, latency +1
          val Seq(a, b, c, d, e, f, g, h) = s1
          Seq(a + b, a - b,
            c - d.multiplyI, c + d.multiplyI,
            e - f.multiplyI, e + f.multiplyI,
            h - g.multiplyI, h + g.multiplyI).map(_.d(1))
        }

        val s3 = { // stage 3, latency +1
          val Seq(a, b, c, d, e, f, g, h) = s2
          Seq(a, e + g, c, e - g, b, f - h, d, f + h)
        }

        if (!inverse) s3 else getFftSym(s3)
    }

    complexDataOut.zip(ret.map(_.d(1))).foreach { case (port, fix) => port := fix }
  }
}
package org.datenlord

import breeze.math._
import org.datenlord.device.ComplexMult
import spinal.core._

import scala.language.postfixOps

object ComplexFixType

/** complex number type based on SFix
 *
 * @param R real part of the complex number
 * @param I imaginary part of the complex number
 */
case class ComplexFix(peak: ExpNumber, resolution: ExpNumber) extends Bundle {

  val real = SFix(peak, resolution)
  val imag = SFix(peak, resolution)

  def +(that: ComplexFix): ComplexFix = ComplexFix(real + that.real, imag + that.imag)

  def +^(that: ComplexFix): ComplexFix = ComplexFix(real +^ that.real, imag +^ that.imag)

  def -(that: ComplexFix): ComplexFix = ComplexFix(real - that.real, imag - that.imag)

  def -^(that: ComplexFix): ComplexFix = ComplexFix(real -^ that.real, imag -^ that.imag)

  def unary_-(): ComplexFix = ComplexFix(-real, -imag)

  // nontrivial computations
  def multiplyI = ComplexFix(-imag, real)

  def divideI = ComplexFix(imag, -real)

  def conj = ComplexFix(real, -imag)

  def *(that: SFix) = {
    val R = real * that
    val I = imag * that
    ComplexFix(R, I)
  }

  def *(that: ComplexFix): ComplexFix = {
    val mult = ComplexMult(this.sfixType, that.sfixType)
    mult.data := this
    mult.coeff := that
    mult.product
  }

  def truncated(dataType: HardType[SFix]) = {
    val retReal, retImag = dataType()
    retReal := real.truncated
    retImag := imag.truncated
    ComplexFix(retReal, retImag)
  }

  def sfixType = HardType(this.real)

  def >>(that: Int) = ComplexFix(real >> that, imag >> that)

  def <<(that: Int) = ComplexFix(real << that, imag << that)
}

object ComplexFix {

  //  def apply(peak: ExpNumber, resolution: ExpNumber): ComplexFix = new ComplexFix(peak, resolution)

  def apply(dataType: HardType[SFix]): ComplexFix = ComplexFix(dataType().maxExp exp, dataType().minExp exp)

  def apply(R: SFix, I: SFix): ComplexFix = {
    require(R.maxExp == I.maxExp && R.minExp == I.minExp)
    val ret = ComplexFix(HardType(R))
    ret.real := R
    ret.imag := I
    ret
  }
}

object CF {
  def apply(complex: Complex, dataType: HardType[SFix]): ComplexFix = {
    def toSF(value: Double) = SF(value, dataType().maxExp exp, dataType().minExp exp)
    ComplexFix(toSF(complex.real), toSF(complex.imag))
  }
}

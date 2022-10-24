package org.datenlord

import breeze.math._

import scala.language.implicitConversions

package object matlab {

  type MComplex = com.mathworks.matlab.types.Complex

  /** implicit conversion from Matlab Complex to Breeze Complex
   */
  implicit def ComplexConversion(mcomplex: MComplex): Complex = Complex(mcomplex.real, mcomplex.imag)

  implicit class mcomplexConversion(mcomplex: MComplex){
    def toComplex = Complex(mcomplex.real, mcomplex.imag)
  }

  implicit class complexConversion(complex: Complex) {
    def toMComplex: MComplex = new MComplex(complex.real, complex.imag)
  }

}
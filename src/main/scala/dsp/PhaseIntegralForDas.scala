package org.datenlord
package dsp

import dsp.FilterStructure._

import breeze.math._
import breeze.numerics.constants._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/**
 * @param window length of the integral window
 */
case class PhaseIntegralForDas(window: Int) extends Component {

  val typeFix = HardType(SFix(1 exp, -16 exp))
  val typeOut = HardType(SFix(log2Up(window) + 1 exp, -13 exp))

  val dataIn = slave Flow typeFix()
  val dataOut = master Flow ComplexFix(typeOut)

  val phases = (1 to window).map { j =>
    val exponent = -i * (2 * Pi * j / window)
    val phase = E.pow(exponent)
    CF(phase, typeFix)
  }

  def mult(data: ComplexFix, coeff: ComplexFix) = (coeff * data.real).truncate(typeOut)

  def add(a: ComplexFix, b: ComplexFix) = a + b

  val dataInComplex = ComplexFix(dataIn.payload, typeFix().getZero)

  dataOut.payload := DoFir(dataInComplex, phases, mult, add, structure = TRANSPOSE)
  dataOut.valid := dataIn.valid.validAfter(DoFir.latency(window, TRANSPOSE))
}

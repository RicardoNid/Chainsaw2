package org.datenlord
package dsp

import dsp.FilterStructure._

import spinal.core.Data

object DoFir {

  /** we drop the first taps - 1 output, as the registers hasn't been fully refreshed, and therefore, the output and input have the same size
   */
  def latency(taps: Int, structure: FilterStructure = TRANSPOSE) = structure match {
    case DIRECT => taps + 1
    case TRANSPOSE => taps + 1
    case SYSTOLIC => taps * 2 + 1
  }

  def apply[T <: Data](data: T, coeffs: Seq[T], mult: (T, T) => T, add: (T, T) => T,
                       structure: FilterStructure = TRANSPOSE) = {
    val taps = coeffs.length
    structure match {
      case DIRECT =>
        val delayLine = Seq.iterate(data.d(1), taps)(_.d(1))
        val products = delayLine.zip(coeffs).map { case (x, h) => mult(x, h) }
        products.reduce(add).d(1)
      case TRANSPOSE =>
        val products = coeffs.map(coeff => mult(data.d(1), coeff))
        products.reverse.reduce((prev, next) => add(prev.d(1), next)).d(1)
      case SYSTOLIC =>
        val delayLine = Seq.iterate(data.d(1), taps)(_.d(2))
        val products = delayLine.zip(coeffs).map { case (x, h) => mult(x, h) }.map(_.d(1))
        products.reduce((prev, next) => add(prev.d(1), next)).d(1)
    }
  }
}

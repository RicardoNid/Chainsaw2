package org.datenlord
package arithmetic

import breeze.math.Complex
import spinal.core._

import scala.language.postfixOps

object GetCoeff {
  def apply[THard <: Data, TSoft](dataType: HardType[THard], coeffs:TSoft) = {
    dataType() match {
      case cf: ComplexFix =>
        val sfixType = cf.sfixType
        val coeffHard = coeffs.asInstanceOf[Seq[Complex]].map(CF(_, sfixType))
        coeffHard.asInstanceOf[Seq[THard]]
      case sf: SFix =>
        val peak = sf.maxExp
        val resolution = sf.minExp
        val coeffHard = coeffs.asInstanceOf[Seq[Double]].map(SF(_, peak exp, resolution exp))
        coeffHard.asInstanceOf[Seq[THard]]
      case si: SInt =>
        val width = si.getBitsWidth
        val coeffHard = coeffs.asInstanceOf[Seq[Int]].map(S(_, width bits))
        coeffHard.asInstanceOf[Seq[THard]]
      case ui: UInt =>
        val width = ui.getBitsWidth
        val coeffHard = coeffs.asInstanceOf[Seq[Int]].map(U(_, width bits))
        coeffHard.asInstanceOf[Seq[THard]]
    }
  }
}

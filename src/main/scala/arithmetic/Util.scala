package org.datenlord
package arithmetic

import org.datenlord.device.ComplexMult.complexMult

import breeze.math.Complex
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object Util {
  def getCoeff[THard <: Data, TSoft](dataType: HardType[THard], coeffs:Seq[TSoft]) = {
    val ret = dataType() match {
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
    Vec(ret)
  }

  def mult[THard <: Data, TSoft]
  (dataIn: Vec[THard], coeffs:Vec[THard],
   dataType:HardType[THard]) = {
    val ret = dataIn.zip(coeffs).map { case (data, coeff) =>
      data match {
        case cf: ComplexFix => complexMult(cf.asInstanceOf[ComplexFix], coeff.asInstanceOf[ComplexFix]).truncated(dataType.asInstanceOf[HardType[ComplexFix]]().sfixType)
        case sf: SFix => (sf.asInstanceOf[SFix] * coeff.asInstanceOf[SFix]).truncated(dataType.asInstanceOf[HardType[SFix]]).d(1)
        case si: SInt => (si.asInstanceOf[SInt] * coeff.asInstanceOf[SInt]).resize(dataType.getBitsWidth).d(1)
        case ui: UInt => (ui.asInstanceOf[UInt] * coeff.asInstanceOf[UInt]).resize(dataType.getBitsWidth).d(1)
      }
    }.asInstanceOf[Seq[THard]]
    Vec(ret)
  }

  def sum[THard <: Data]
  (dataIn: Vec[THard]) = {
    val ret = dataIn.head match {
      case _: ComplexFix => dataIn.asInstanceOf[Vec[ComplexFix]].reduceBalancedTree((a, b) => (a + b).d(1))
      case _: SFix => dataIn.asInstanceOf[Vec[SFix]].reduceBalancedTree((a, b) => (a + b).d(1))
      case _: SInt => dataIn.asInstanceOf[Vec[SInt]].reduceBalancedTree((a, b) => (a + b).d(1))
      case _: UInt => dataIn.asInstanceOf[Vec[UInt]].reduceBalancedTree((a, b) => (a + b).d(1))
    }
    ret.asInstanceOf[THard]
  }
}

package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

import scala.language.postfixOps

sealed trait NumericType

object UIntType extends NumericType

object SIntType extends NumericType

object SFixType extends NumericType

object ComplexFixType extends NumericType

case class NumericTypeInfo(integral: Int, signed: Boolean = false, fractional: Int = 0, complex: Boolean = false) {

  require(integral >= 0 && fractional >= 0)
  if (complex) require(signed, "unsigned complex is not supported")
  else if (fractional > 0) require(signed, "UFix is not supported")

  def bitWidth = ((if (signed) 1 else 0) + integral + fractional) * (if (complex) 2 else 1)

  def asUInt: HardType[UInt] = HardType(UInt(integral bits))

  def asSInt: HardType[SInt] = HardType(SInt(integral bits))

  def asSFix: HardType[SFix] = HardType(SFix(integral exp, -fractional exp))

  def asComplexFix: HardType[ComplexFix] = HardType(ComplexFix(integral exp, -fractional exp))

  def toSFixInfo = NumericTypeInfo(integral, signed, fractional, complex = false)

  def getType =
    if (complex) ComplexFixType
    else if (fractional > 0) SFixType
    else if (signed) SIntType
    else UIntType

  def toSigned(bigInt: BigInt, width: Int): BigInt = {
    val padded = bigInt.toString(2).padToLeft(width + 1, '0')
    val sign = if (padded.head == '1') -1 else 0
    BigInt(1 << width) * sign + BigInt(padded.tail, 2)
  }

  // TODO: verification
  def bits2SInt(bigInt: BigInt) = toSigned(bigInt, integral)

  def bits2SFix(bigInt: BigInt): Double = (BigDecimal(toSigned(bigInt, integral + fractional)) / BigDecimal(BigInt(1) << fractional)).toDouble

  def bits2ComplexFix(bigInt: BigInt): Complex = {
    val imag = bigInt >> (bitWidth / 2)
    val real = bigInt % (1 << (bitWidth / 2)) // real is the first one in ComplexFix, so it takes the lower part
    Complex(bits2SFix(real), bits2SFix(imag))
  }

  def fromBits(bigInt: BigInt) = getType match {
    case UIntType => bigInt
    case SIntType => bits2SFix(bigInt)
    case SFixType => bits2SFix(bigInt)
    case ComplexFixType => bits2ComplexFix(bigInt)
  }

  def toUnSigned(sint: BigInt, width: Int): BigInt = if (sint < 0) sint + (BigInt(1) << (width + 1)) else sint

  def sint2Bits(sint: BigInt) = toUnSigned(sint, integral)

  // TODO: arbitrary width double
  def double2Bits(double: Double) = {
    val sint = round(double * (1 << fractional)).toInt
    toUnSigned(sint, integral + fractional)
  }

  def complex2Bits(complex: Complex) = {
    val sintR = double2Bits(complex.real)
    val sintI = double2Bits(complex.imag)
    BigInt(sintI.toString(2).padToLeft(bitWidth / 2, '0') + sintR.toString(2).padToLeft(bitWidth / 2, '0'), 2)
  }

  def toBits(value: Any) = getType match {
    case UIntType => value.asInstanceOf[BigInt]
    case SIntType => sint2Bits(value.asInstanceOf[BigInt])
    case SFixType => double2Bits(value.asInstanceOf[Double])
    case ComplexFixType => complex2Bits(value.asInstanceOf[Complex])
  }

  def toComplex(bits: Bits): ComplexFix = {
    val ret = asComplexFix()
    ret.assignFromBits(bits)
    ret
  }

  // TODO
  def resize(bits: Bits) = {
    getType match {
      case UIntType => if (bits.getBitsWidth > bitWidth) None else Some(bits.resize(bitWidth))
      case SIntType => if (bits.getBitsWidth > bitWidth) None else Some(bits.resize(bitWidth))
      case SFixType => ???
      case ComplexFixType => ???
    }
  }
}

object UIntInfo {
  def apply(width: Int) = NumericTypeInfo(width)
}

object SIntInfo {
  def apply(width: Int) = NumericTypeInfo(width, signed = true)
}

object SFixInfo {
  def apply(integral: Int, fractional: Int) =
    NumericTypeInfo(integral = integral, signed = true, fractional = fractional)
}

object ComplexFixInfo {
  def apply(integral: Int, fractional: Int) =
    NumericTypeInfo(integral = integral, signed = true, fractional = fractional, complex = true)
}
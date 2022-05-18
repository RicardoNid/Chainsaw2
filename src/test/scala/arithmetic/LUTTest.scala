package org.datenlord
package arithmetic

import breeze.math._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

class LUTTest extends AnyFlatSpec {

  "Complex LUT" should "work for all numeric types" in {

    // ComplexFix
    val coeffs0 = Seq(Complex(1, 0), Complex(2, 0))
    val dataType0 = HardType(ComplexFix(2 exp, -13 exp))
    val config0 = LUTConfig(coeffs0, dataType0)

    // SFix
    val coeffs1 = Seq(1.0, 2.0)
    val dataType1 = HardType(SFix(2 exp, -13 exp))
    val config1 = LUTConfig(coeffs1, dataType1)

    // SInt
    val coeffs2 = Seq(-1, 1)
    val dataType2 = HardType(SInt(2 bits))
    val config2 = LUTConfig(coeffs2, dataType2)

    // UInt
    val coeffs3 = Seq(1, 2)
    val dataType3 = HardType(UInt(2 bits))
    val config3 = LUTConfig(coeffs3, dataType3)

    val data = Seq(BigInt(0), BigInt(1))

    TransformTest.test(LUT(config0), data)
    TransformTest.test(LUT(config1), data)
    TransformTest.test(LUT(config2), data)
    TransformTest.test(LUT(config3), data)
  }
}

package org.datenlord
package arithmetic

import breeze.math._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

class ComplexLUTTest extends AnyFlatSpec {

  "Complex LUT" should "work" in {

    val coeffs = Seq(Complex(1, 0), Complex(2, 0))
    val dataType = HardType(SFix(2 exp, -13 exp))
    val config = ComplexLUTConfig(coeffs, dataType)

    val data = Seq(BigInt(0), BigInt(1))

    TransformTest.test(ComplexLUT(config), data)
  }
}

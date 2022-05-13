package org.datenlord
package arithmetic

import flowConverters._
import intel.QuartusFlow

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random

class DiagonalMatrixTest extends AnyFlatSpec {

  "DiagonalMatrix" should "work" in {

    val width = 16
    val coeffs = (0 until 16).map(_ => Random.nextBigInt(width))
    def baseMult(a:Bits, b:Bits) = (a.asUInt * b.asUInt).asBits.d(1)
    val config0 = DiagonalMatrixConfig(coeffs, 1, width, width, width * 2, baseMult, 1)
    val config1 = DiagonalMatrixConfig(coeffs, 4, width, width, width * 2, baseMult, 1)

    TransformTest.bitAccurateTest(DiagonalMatrix(config0), coeffs)
    TransformTest.bitAccurateTest(DiagonalMatrix(config1), coeffs)
    new QuartusFlow(DiagonalMatrix(config1)).impl()
  }

}

package org.datenlord
package arithmetic

import breeze.linalg._
import breeze.math._
import org.datenlord.TransformTest
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random


class ComplexDiagonalMatrixTest extends AnyFlatSpec {

  val coeffs = Random.RandomComplexSequences(1, 100).head
  val data = Random.RandomComplexSequences(1, 100).head
  val dataType = HardType(SFix(0 exp, -15 exp))
  val coeffType = HardType(SFix(1 exp, -10 exp))

  def metric(yours: Seq[Complex], golden: Seq[Complex]) = {
    val yourV = new DenseVector(yours.toArray)
    val goldenV = new DenseVector(golden.toArray)
    val errorV = yourV - goldenV
    println(yourV)
    println(goldenV)
    println(errorV)
    errorV.forall(_.abs < 10e-3)
  }

  "ComplexDiagonalMatrix" should "work" in {
    val folds = Seq(1, 2, 5, 10)
    folds.foreach { i =>
      val config = ComplexDiagonalMatrixConfig(coeffs, i, dataType = dataType, coeffType = coeffType)
      TransformTest.test(ComplexDiagonalMatrix(config), data, metric)
    }
  }

}

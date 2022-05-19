package org.datenlord
package arithmetic

import breeze.linalg._
import breeze.math._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random


class DiagonalTest extends AnyFlatSpec {

  "ComplexDiagonalMatrix" should "work for all folds" in {

    val coeffs = Random.RandomComplexSequences(1, 100).head
    val data = Random.RandomComplexSequences(1, 100).head
    val dataType = HardType(ComplexFix(0 exp, -15 exp))
    val coeffType = HardType(ComplexFix(1 exp, -10 exp))

    // TODO: implement common metrics in an object Metric
    def metric(yours: Seq[Complex], golden: Seq[Complex]) = {
      val yourV = new DenseVector(yours.toArray)
      val goldenV = new DenseVector(golden.toArray)
      val errorV = yourV - goldenV
      errorV.forall(_.abs < 10e-3)
    }

    val config = DiagonalConfig(coeffs, dataType = dataType, coeffType = coeffType, 1)
    TransformTest.testAllFolds(config, data, metric)
  }

  "Diagonal Matrix" should "work for all numeric types" in {
    // double
    val coeffsDouble = Random.RandomSequences(1, 100, Random.nextDouble).head
    val dataDouble = Random.RandomSequences(1, 100, Random.nextDouble).head
    val dataTypeDouble = HardType(SFix(0 exp, -15 exp))
    val coeffTypeDouble = HardType(SFix(1 exp, -10 exp))
    def metricDouble(yours: Seq[Double], golden: Seq[Double]) = {
      val yourV = new DenseVector(yours.toArray)
      val goldenV = new DenseVector(golden.toArray)
      val errorV = yourV - goldenV
      errorV.forall(_.abs < 10e-3)
    }

    val configDouble = DiagonalConfig(coeffsDouble, dataTypeDouble, coeffTypeDouble, 1)
    TransformTest.test(configDouble.implH,dataDouble,metricDouble)
  }


}

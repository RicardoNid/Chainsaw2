package org.datenlord
package arithmetic

import breeze.linalg._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

class AlgebraTest extends AnyFlatSpec {

  "Algebra Matrix" should "work for all folds" in {
    val coeff = DenseMatrix.rand[Double](6, 8)
    val data = DenseVector.rand[Double](8 * 4).toArray
    val dataType = HardType(SFix(3 exp, -12 exp))
    val coeffType = HardType(SFix(3 exp, -12 exp))
    val config = AlgebraConfig(coeff, dataType, coeffType, 1)
    val metric = Metric.DoubleAbs(1e-2)

    TransformTest.testAllFolds(config, data, metric)
  }
}

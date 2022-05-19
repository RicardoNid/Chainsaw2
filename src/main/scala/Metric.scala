package org.datenlord

import breeze.linalg.DenseVector
import breeze.math.Complex

object Metric {

  def ComplexAbs(epsilon: Double) =
    (yours: Seq[Complex], golden: Seq[Complex]) => {
      val yourV = new DenseVector(yours.toArray)
      val goldenV = new DenseVector(golden.toArray)
      val errorV = yourV - goldenV
      errorV.forall(_.abs < epsilon)
    }

  def DoubleAbs(epsilon: Double) =
    (yours: Seq[Double], golden: Seq[Double]) => {
      val yourV = new DenseVector(yours.toArray)
      val goldenV = new DenseVector(golden.toArray)
      val errorV = yourV - goldenV
      errorV.forall(_.abs < epsilon)
    }

}

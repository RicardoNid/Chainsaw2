package org.datenlord

import breeze.linalg.{DenseVector, max}
import breeze.math.Complex
import breeze.stats.mean

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
      val errorV = (yourV - goldenV).map(_.abs)
      logger.info(s"errorMax = ${max(errorV)}, errormean${mean(errorV)}")
      errorV.forall(_.abs < epsilon)
    }
}
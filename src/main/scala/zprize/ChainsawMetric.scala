package org.datenlord
package zprize

import breeze.linalg.{DenseVector, max}
import breeze.math.Complex
import breeze.numerics.abs
import breeze.stats.mean
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

object ChainsawMetric {

  def complexAbs(epsilon: Double) =
    (yours: Seq[Any], golden: Seq[Any]) => {
      val yourV = new DenseVector(yours.map(_.asInstanceOf[Complex]).toArray)
      val goldenV = new DenseVector(golden.map(_.asInstanceOf[Complex]).toArray)
      val errorV = yourV - goldenV
      val pass = errorV.forall(_.abs < epsilon)
      logger.info(s"errorMax = ${max(abs(errorV))}, errorMean = ${mean(abs(errorV))}")
      pass
    }

  def fftByMean(epsilon: Double) =
    (yours: Seq[Any], golden: Seq[Any]) => {
      val yourV = new DenseVector(yours.tail.map(_.asInstanceOf[Complex]).toArray) // leave DC part alone
      val goldenV = new DenseVector(golden.tail.map(_.asInstanceOf[Complex]).toArray)
      val errorV = yourV - goldenV
      val pass = mean(abs(errorV)) < epsilon
      logger.info(s"errorMax = ${max(abs(errorV))}, errorMean = ${mean(abs(errorV))}")
      pass
    }

  def doubleAbs(epsilon: Double) =
    (yours: Seq[Any], golden: Seq[Any]) => {
      val yourV = new DenseVector(yours.map(_.asInstanceOf[Double]).toArray)
      val goldenV = new DenseVector(golden.map(_.asInstanceOf[Double]).toArray)
      val errorV = (yourV - goldenV).map(_.abs)
      val pass = errorV.forall(_.abs < epsilon)
      if (!pass) logger.info(s"errorMax = ${max(errorV)}, errorMean = ${mean(errorV)}")
      pass
    }

  def ignoreNegative = (yours: Seq[Any], golden: Seq[Any]) => {
    if (golden.asInstanceOf[Seq[BigInt]].exists(_ < 0)) true else yours.equals(golden)
  }

  def carrySaveMetric = (yours: Seq[Any], golden: Seq[Any]) => {
    val g = golden.asInstanceOf[Seq[BigInt]].sum
    val y = yours.asInstanceOf[Seq[BigInt]].sum
    if (g < 0) true else g == y
  }


}

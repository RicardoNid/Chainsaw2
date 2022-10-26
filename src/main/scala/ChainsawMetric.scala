package org.datenlord

import breeze.linalg.{DenseVector, max}
import breeze.math.Complex
import breeze.numerics.abs
import breeze.stats.mean
import org.datenlord.ChainsawMetric.noBound
import org.datenlord.logger

case class ChainsawMetric(
                           elementWise: Metric = noBound,
                           frameWise: FrameMetric
                         )

object ChainsawMetric { // common metrics

  def noBound: Metric = (y: Any, g: Any) => true

  def defaultBound: Metric = (y: Any, g: Any) => y == g

  def doubleBound(epsilon: Double): Metric = (y: Any, g: Any) => abs(y.asInstanceOf[Double] - g.asInstanceOf[Double]) < epsilon

  def complexBound(epsilon: Double): Metric = (y: Any, g: Any) => abs(y.asInstanceOf[Complex] - g.asInstanceOf[Complex]) < epsilon

  def berBound(ber: Double, elementWise: (Any, Any) => Boolean): FrameMetric = (ys: Seq[Any], gs: Seq[Any]) => {
    val errorCount = ys.zip(gs).count { case (y, g) => !elementWise(y, g) }
    val ret = errorCount.toDouble / ys.length <= ber
    logger.info(s"frame ber: ${errorCount.toDouble / ys.length}")
    ret
  }

  def forallBound(elementWise: (Any, Any) => Boolean): FrameMetric = (ys: Seq[Any], gs: Seq[Any]) => {
    ys.zip(gs).forall { case (y, g) => elementWise(y, g) }
  }


  def complexAbs(epsilon: Double) = ChainsawMetric(complexBound(epsilon), berBound(0, complexBound(epsilon)))

  def doubleAbs(epsilon: Double) = ChainsawMetric(doubleBound(epsilon), berBound(0, doubleBound(epsilon)))

  def carrySaveMetric(compensation: BigInt) = ChainsawMetric(noBound,
    frameWise = (yours: Seq[Any], golden: Seq[Any]) => {
      val g = golden.asInstanceOf[Seq[BigInt]].sum
      val y = yours.asInstanceOf[Seq[BigInt]].sum
      if (g < 0) true else g == y - compensation
    }
  )

  val defaultMetric = ChainsawMetric(defaultBound, forallBound(defaultBound))
}
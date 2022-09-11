package org.datenlord
package ip.das

import matlab.MatlabFeval

import breeze.math._
import matlab._

/** DAS parameter generator
 *
 * @param samplingFreq     sampling frequency after upsampling
 * @param gaugeLengthRange range of gauge length (meter)
 * @param probeLengthRange range of probe length (kilometer)
 * @param bandWidthChoice  available pass band widths of the bandpass filter
 */
case class DasConfig(
                      samplingFreq: Double = 2e9,
                      gaugeLengthRange: (Double, Double) = (5.0, 50.0),
                      probeLengthRange: (Double, Double) = (0.9, 39.9),
                      bandWidthChoice: Seq[Double] = Seq(5e6, 10e6, 15e6, 20e6, 25e6)
                    ) {

  val c = 2e8
  val samplingFreqAdc: Double = 250e6
  val aomFreq = 80e6
  val bandPassOrder = 50

  val upSampleFactor = (samplingFreq / samplingFreqAdc).toInt
  val integrationLength = samplingFreq / aomFreq

  val subFilterCount = upSampleFactor * 2 // number of sub-filters(as well as data processing channels)

  def pulseFreq(probeLength: Double): Double = c / ((probeLength + 0.1) * 1000 * 2) // speed / distance

  def pulsePoints(probeLength: Double): Int = (samplingFreq / pulseFreq(probeLength)).toInt

  def gaugePoints(gaugeLength: Double): Int = (samplingFreq / (c / (gaugeLength * 2))).toInt

  def spatialPoints(probeLength: Double, gaugeLength: Double) = pulsePoints(probeLength) / gaugePoints(gaugeLength)

  def bandPassRanges: Seq[Seq[Double]] = bandWidthChoice
    .map(width => Seq(aomFreq - width / 2, aomFreq + width / 2))
    .map(_.map(_ / (samplingFreq / 2)))

  def bandpassCoeffGroups: Seq[Array[Double]] = bandPassRanges.map(range =>
    MatlabFeval[Array[Double]]("fir1", 0, bandPassOrder, range.toArray))

  def upSampleCoeffs: Array[Double] =
    MatlabFeval[Array[Double]]("resample", 1, Array(0.0), upSampleFactor.toDouble, 1.toDouble)

  def phaseCoeffs: Array[MComplex] = {
    matlabEngine.eval(s"LcmFreq = lcm($samplingFreq, $aomFreq);")
    matlabEngine.eval(s"IntegrationLength = LcmFreq / $aomFreq; ")
    matlabEngine.eval(s"points = exp(-1i * 2 * pi * 1 / IntegrationLength * (1:IntegrationLength) * (LcmFreq / $samplingFreq));")
    matlabEngine.getVariable[Array[MComplex]]("points")
  }

  def combinedCoeffGroups = bandpassCoeffGroups.map { bandpassCoeffs =>
    val temp = matlab.MatlabFeval[Array[Double]]("conv", 0, bandpassCoeffs, upSampleCoeffs)
    val ret = matlab.MatlabFeval[Array[MComplex]]("conv", 0, temp, phaseCoeffs)
    val paddedLength = ret.length.nextMultiple(subFilterCount)
    logger.info(s"${ret.length} padded to $paddedLength")
    ret.padTo(paddedLength, new MComplex(0.0, 0.0))
  }

  def imagCoeffGroups: Seq[Array[Double]] = combinedCoeffGroups.map(_.map(_.imag))

  def realCoeffGroups: Seq[Array[Double]] = combinedCoeffGroups.map(_.map(_.real))

  val pulsePointsMax = pulsePoints(probeLengthRange._2)
  val gaugePointsMax = gaugePoints(gaugeLengthRange._2)
  val gaugePointsMin = gaugePoints(gaugeLengthRange._1)
  val meanPointsMax = pulsePointsMax / gaugePointsMin

  val cordicIteration = 12
  val cordicFraction = 16

}

object DasConfig {
  def main(args: Array[String]): Unit = {
    //    DasConfig(samplingFreq = 1e9).bandPassRanges.foreach(println)
    //    DasConfig(samplingFreq = 1e9).bandpassCoeffs.foreach(coeffs => logger.info(s"coeffs: ${coeffs.mkString(" ")}"))
    //    DasConfig(samplingFreq = 1e9).phaseCoeffs.foreach(println)
    DasConfig(samplingFreq = 1e9).combinedCoeffGroups.head.foreach(println)
  }
}
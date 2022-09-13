package org.datenlord
package ip.das

import matlab.{MatlabFeval, _}

import spinal.core._


// TODO: range arithmetic

/** DAS parameter generator
 *
 * @param samplingFreq     sampling frequency after upsampling
 * @param gaugeLengthRange range of gauge length (meter)
 * @param probeLengthRange range of probe length (kilometer)
 * @param bandWidthChoice  available pass band widths of the bandpass filter
 */
case class DasConfig(
                      samplingFreq: Double = 250e6,
                      gaugeLengthRange: (Double, Double) = (5.0, 50.0),
                      probeLengthRange: (Double, Double) = (0.9, 39.9),
                      bandWidthChoice: Seq[Double] = Seq(5e6, 10e6, 15e6, 20e6, 25e6)
                    ) {

  val c = 2e8
  val samplingFreqAdc: Double = 250e6
  val aomFreq = 80e6
  val bandPassOrder = 50

  val signalProFreq = 125e6 // frequency of signal processing logic

  val unwrapPhasePrecision = 14 // TODO: minimize this
  val intensityPrecision = 14

  val upSampleFactor = (samplingFreq / samplingFreqAdc).toInt
  val integrationLength = samplingFreq / aomFreq

  val subFilterCount = upSampleFactor * 2 // number of sub-filters(as well as data processing channels)

  def pulseFreq(probeLength: Double): Double = c / ((probeLength + 0.1) * 1000 * 2) // speed / distance

  def pulsePoints(probeLength: Double): Int = (samplingFreq / pulseFreq(probeLength)).ceil.toInt

  def pulsePointsProcessed(probeLength: Double): Int = pulsePoints(probeLength) * upSampleFactor

  def gaugePoints(gaugeLength: Double): Int = (samplingFreq / (c / (gaugeLength * 2))).ceil.toInt

  def spatialPoints(probeLength: Double, gaugeLength: Double): Int =
    (pulsePoints(probeLength).toDouble / gaugePoints(gaugeLength)).ceil.toInt

  /** number of cycles in a pulse, used as parameter for pulse generator
   */
  def pulsePeriod(probeLength: Double): Int =
    (pulsePoints(probeLength) * signalProFreq / samplingFreq).ceil.toInt

  /** pulse width = 100ns, number of cycles when a pulseOut is high
   */
  val pulseWidth = (100 * 1e-9 / (1 / signalProFreq)).ceil.toInt

  /** --------
   * filter coeffs generation
   * -------- */

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
  val spatialPointsMax = spatialPoints(probeLengthRange._2, gaugeLengthRange._1)
  val pulsePeriodMax = pulsePeriod(probeLengthRange._2)

  val cordicIteration = 12
  val cordicFraction = 16

  def onChipStorage = {
    val storage0 = pulsePointsProcessed(probeLengthRange._2) * unwrapPhasePrecision // for unwrap after diff
    val storage1 = spatialPoints(probeLengthRange._2, gaugeLengthRange._1) * unwrapPhasePrecision // for unwrap after mean
    val ret = storage0 + storage1
    logger.info(s"on chip storage: ${(storage0 + storage1) / 1e6} Mbs")
    ret
  }


  logger.info(s"\npulsePointsMax: $pulsePointsMax -> ${log2Up(pulsePointsMax)}" +
    s"\ngaugePointsMax: $gaugePointsMax -> ${log2Up(gaugePointsMax)}" +
    s"\nspatialPointsMax: $spatialPointsMax -> ${log2Up(spatialPointsMax)}" +
    s"\npulsePeriodMax: $pulsePeriodMax -> ${log2Up(pulsePeriodMax)}" +
    s"\npulseWidth: $pulseWidth -> ${log2Up(pulseWidth)}"
  )
}

object DasConfig {
  def main(args: Array[String]): Unit = {
    println(DasConfig().pulsePeriod(39.9))
    println(DasConfig().pulseFreq(39.9))
    println(DasConfig().pulsePeriod(0.9))
  }
}
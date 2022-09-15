package org.datenlord
package ip.das

import matlab.{MComplex, MatlabFeval}

import spinal.core.{HardType, SFix, IntToBuilder}
import scala.language.postfixOps

case class DasConstants( // parameters for hardware generation
                         pulseWidth: Int,
                         imagCoeffGroups: Map[Double, Array[Double]],
                         realCoeffGroups: Map[Double, Array[Double]],
                         subFilterCount: Int,
                         pulsePointsMax: Int,
                         pulsePeriodMax: Int,
                         gaugePointsMax: Int,
                         spatialPointsMax: Int
                       ) {
  logger.info(
    s"\n----constants for FPGA hardware----" +
      s"\n\thigh-voltage cycles in a pulse -> $pulseWidth" +
      s"\n\tpulse points max -> $pulsePointsMax" +
      s"\n\tgauge points max -> $gaugePointsMax" +
      s"\n\tspatial points max -> $spatialPointsMax"
  )

  val adcDataType = HardType(SFix(0 exp, -13 exp))
  val firOutDataType = HardType(SFix(4 exp, -11 exp))
  val normalizedPhaseType = HardType(SFix(0 exp, -15 exp))
  val phaseDiffType = HardType(SFix(1 exp, -15 exp))
  val phaseUnwrapType = HardType(SFix(4 exp, -15 exp))
  val phaseStoredType = HardType(SFix(4 exp, -8 exp))
}

case class DasCost(
                    multiplierCost: Int,
                    onChipStorageCost: Double, // b
                    upstreamBandwidthCost: Double // b/s
                  ) {
  require(multiplierCost <= 684)
  require(onChipStorageCost <= 10e6)
  require(upstreamBandwidthCost <= 400e6 * 8, s"upstream bandwidth: ${upstreamBandwidthCost / 1e6} Mb")
  logger.info(
    s"\n----cost on FPGA for current configurations----" +
      s"\n\tmultiplier -> $multiplierCost" +
      s"\n\ton chip memory -> ${onChipStorageCost / 1e6} Mb" +
      s"\n\tupstream bandwidth -> ${upstreamBandwidthCost / 1e6} Mb"
  )
}

case class DasStaticConfig(
                            samplingFreq: Double = 250e6,
                            sigProFreq: Double = 125e6,
                            aomFreq: Double = 80e6,
                            gaugeLengthRange: (Double, Double) = (5.0, 50.0),
                            probeLengthRange: (Double, Double) = (0.9, 39.9),
                            bandWidthChoices: Seq[Double] = Seq(5e6, 10e6, 15e6, 20e6, 25e6),
                            bandPassOrder: Int = 50,
                            phasePrecision: Int = 14,
                            intensityPrecision: Int = 14
                          ) {
  val c = 2e8 // speed of light in fiber

  def genConstants() = {

    /** --------
     * filter coeffs generation
     * -------- */

    def subFilterCount = (samplingFreq / sigProFreq).ceil.toInt

    def bandPassRanges: Seq[Seq[Double]] = bandWidthChoices
      .map(width => Seq(aomFreq - width / 2, aomFreq + width / 2))
      .map(_.map(_ / (samplingFreq / 2)))

    def bandpassCoeffGroups: Seq[Array[Double]] = bandPassRanges.map(range =>
      MatlabFeval[Array[Double]]("fir1", 0, bandPassOrder, range.toArray))

    def phaseCoeffs: Array[MComplex] = {
      matlabEngine.eval(s"LcmFreq = lcm($samplingFreq, $aomFreq);")
      matlabEngine.eval(s"IntegrationLength = LcmFreq / $aomFreq; ")
      matlabEngine.eval(s"points = exp(-1i * 2 * pi * 1 / IntegrationLength * (1:IntegrationLength) * (LcmFreq / $samplingFreq));")
      matlabEngine.getVariable[Array[MComplex]]("points")
    }

    def combinedCoeffGroups: Seq[Array[MComplex]] = bandpassCoeffGroups.map { bandpassCoeffs =>
      val ret = matlab.MatlabFeval[Array[MComplex]]("conv", 0, bandpassCoeffs, phaseCoeffs)
      val paddedLength = ret.length.nextMultiple(subFilterCount)
      logger.info(s"${ret.length} padded to $paddedLength")
      ret.padTo(paddedLength, new MComplex(0.0, 0.0))
    }

    def pulseFreqMin: Double = c / ((probeLengthRange._2 + 0.1) * 1e3 * 2)

    def pulsePointsMax: Int = (samplingFreq / pulseFreqMin).ceil.toInt

    def pulsePeriodMax: Int = (pulsePointsMax * sigProFreq / samplingFreq).ceil.toInt

    def gaugePointsMin: Int = (gaugeLengthRange._1 * 2 / c * samplingFreq).ceil.toInt

    def gaugePointsMax: Int = (gaugeLengthRange._2 * 2 / c * samplingFreq).ceil.toInt

    def spatialPointsMax: Int = (pulsePointsMax / gaugePointsMin).ceil.toInt

    def imagCoeffGroups = Map(bandWidthChoices.zip(combinedCoeffGroups.map(_.map(_.imag))): _*)

    def realCoeffGroups = Map(bandWidthChoices.zip(combinedCoeffGroups.map(_.map(_.real))): _*)

    DasCost(
      multiplierCost = (combinedCoeffGroups.head.length * (samplingFreq / sigProFreq) * 2).toInt,
      onChipStorageCost = pulsePointsMax * phasePrecision + spatialPointsMax + phasePrecision,
      upstreamBandwidthCost = pulseFreqMin * spatialPointsMax * phasePrecision + pulseFreqMin * spatialPointsMax * intensityPrecision
    )

    DasConstants(
      pulseWidth = (100 * 1e-9 / (1 / sigProFreq)).ceil.toInt,
      imagCoeffGroups = imagCoeffGroups,
      realCoeffGroups = realCoeffGroups,
      subFilterCount = subFilterCount,
      pulsePointsMax = pulsePointsMax,
      pulsePeriodMax = pulsePeriodMax,
      gaugePointsMax = gaugePointsMax,
      spatialPointsMax = spatialPointsMax
    )
  }
}

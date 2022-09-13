package org.datenlord
package ip.das

import matlab.{MComplex, MatlabFeval}

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

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
}

case class DasConstants(
                         pulseWidth: Int,
                         imagCoeffGroups: Map[Double, Array[Double]],
                         realCoeffGroups: Map[Double, Array[Double]],
                         pulsePointsMax: Int,
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

case class DasRuntimeConfig(
                             gaugeLength: Double,
                             probeLength: Double,
                             bandWidth: Double,
                             gain: Int
                           )

case class DasRegValues(
                         pulsePoints: Int,
                         gaugePoints: Int,
                         spatialPoints: Int,
                         pulsePeriod: Int,
                         gain: Int
                       )

object DasConstantGen {
  def apply(staticConfig: DasStaticConfig) = {

    import staticConfig._

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

    def combinedCoeffGroups = bandpassCoeffGroups.map { bandpassCoeffs =>
      val ret = matlab.MatlabFeval[Array[MComplex]]("conv", 0, bandpassCoeffs, phaseCoeffs)
      val paddedLength = ret.length.nextMultiple(subFilterCount)
      logger.info(s"${ret.length} padded to $paddedLength")
      ret.padTo(paddedLength, new MComplex(0.0, 0.0))
    }

    def pulseFreqMin = c / ((probeLengthRange._2 + 0.1) * 1e3 * 2)

    def pulsePointsMax = (samplingFreq / pulseFreqMin).ceil.toInt

    def gaugePointsMin = (gaugeLengthRange._1 * 2 / c * samplingFreq).ceil.toInt

    def gaugePointsMax = (gaugeLengthRange._2 * 2 / c * samplingFreq).ceil.toInt

    def spatialPointsMax = (pulsePointsMax / gaugePointsMin).ceil.toInt

    def imagCoeffGroups = bandWidthChoices.zip(combinedCoeffGroups).map {
      case (bw, coeffs) => bw -> coeffs.map(_.imag)
    }.toMap

    def realCoeffGroups = bandWidthChoices.zip(combinedCoeffGroups).map {
      case (bw, coeffs) => bw -> coeffs.map(_.real)
    }.toMap

    DasCost(
      multiplierCost = (combinedCoeffGroups.head.length * (samplingFreq / sigProFreq) * 2).toInt,
      onChipStorageCost = pulsePointsMax * phasePrecision + spatialPointsMax + phasePrecision,
      upstreamBandwidthCost = pulseFreqMin * spatialPointsMax * phasePrecision + pulseFreqMin * spatialPointsMax * intensityPrecision
    )

    DasConstants(
      pulseWidth = (100 * 1e-9 / (1 / sigProFreq)).ceil.toInt,
      imagCoeffGroups = imagCoeffGroups,
      realCoeffGroups = realCoeffGroups,
      pulsePointsMax = pulsePointsMax,
      gaugePointsMax = gaugePointsMax,
      spatialPointsMax = spatialPointsMax
    )
  }
}

object DasRegValueGen {
  def apply(staticConfig: DasStaticConfig, runtimeConfig: DasRuntimeConfig) = {

    import runtimeConfig._
    import staticConfig._

    def pulseFreq = c / ((probeLength + 0.1) * 1e3 * 2)

    def pulsePoints = (samplingFreq / pulseFreq).ceil.toInt

    def gaugePoints = (gaugeLength * 2 / c * samplingFreq).ceil.toInt

    DasRegValues(
      pulsePoints = pulsePoints,
      gaugePoints = gaugePoints,
      spatialPoints = (pulsePoints / gaugePoints).ceil.toInt,
      pulsePeriod = (sigProFreq / pulseFreq).ceil.toInt,
      gain = gain
    )
  }
}

object DoDas {
  def apply(
             staticConfig: DasStaticConfig,
             runtimeConfig: DasRuntimeConfig,
             dataIn: Array[Double]) = {

    val constants = DasConstantGen(staticConfig)
    val regValues = DasRegValueGen(staticConfig, runtimeConfig)
    import constants._
    import regValues._

    val pulses = dataIn.grouped(pulsePoints).toSeq

    var pulseRam = Array[Double]()
    var meanRam = Array[Double]()
    val phaseRet = ArrayBuffer[Array[Double]]()
    val intensityRet = ArrayBuffer[Array[Double]]()

    def conv(data: Array[Double], coeff: Array[Double]) = {
      val padSize = coeff.length - 1
      val paddedData = Array.fill(padSize)(0.0) ++ data ++ Array.fill(padSize)(0.0)
      (0 until data.length + coeff.length - 1).map { i =>
        val dataSlice = paddedData.slice(i, i + coeff.length)
        dataSlice.zip(coeff).map { case (d, c) => d * c }.sum
      }.toArray
    }

    import breeze.math._
    import breeze.numerics._
    import breeze.numerics.constants._

    def pointwiseUnwrapNormalized(prev: Double, next: Double) = {
      val m = prev.floor
      val candidates = Seq(-1, 0, 1).map(_ + m + (next - next.floor))
      candidates.find(ret => (ret - prev).abs <= 1 && (ret - next).abs.round % 2 == 0).get
    }

    def pointwiseUnwrap(prev: Double, next: Double): Double = pointwiseUnwrapNormalized(prev / Pi, next / Pi) * Pi

    val coeffsReal = realCoeffGroups(runtimeConfig.bandWidth)
    val coeffsImag = imagCoeffGroups(runtimeConfig.bandWidth)

    matlabEngine.eval("figure;")
    matlabEngine.putVariable("real", coeffsReal)
    matlabEngine.putVariable("imag", coeffsImag)
    matlabEngine.eval("subplot(1,2,1); plot(real);")
    matlabEngine.eval("subplot(1,2,2); plot(imag);")

    pulses.zipWithIndex.foreach { case (pulse, i) =>
      println(s"currently running on pulse ${i + 1}/${pulses.length}")
      // filtering
      val filteredReal = conv(pulse, coeffsReal)
      val filteredImag = conv(pulse, coeffsImag)
      val dataComplex = filteredReal.zip(filteredImag).map { case (r, i) => Complex(r, i) }
      val phases = matlab.MatlabFeval[Array[Double]]("angle", 0, dataComplex.map(complex => new MComplex(complex.real, complex.imag)))
      val intensities = dataComplex.map(_.abs)

      // phase path
      val diffPulse = phases.drop(gaugePoints).zip(phases.dropRight(gaugePoints)).map { case (next, prev) => next - prev } // diff
      val unwrappedPulse = if (pulseRam.isEmpty) diffPulse else pulseRam.zip(diffPulse).map { case (prev, next) => pointwiseUnwrap(prev, next) } // unwrap
      pulseRam = unwrappedPulse
      val meanPulse = unwrappedPulse.grouped(gaugePoints).toArray.map(slice => slice.sum / slice.length) // mean
      val unwrappedMeanPulse = if (meanRam.isEmpty) meanPulse else meanRam.zip(meanPulse).map { case (prev, next) => pointwiseUnwrap(prev, next) } // unwrap
      meanRam = unwrappedMeanPulse
      phaseRet += unwrappedMeanPulse

      // intensity path
      val meanIntensity = intensities.grouped(gaugePoints).toArray.map(slice => slice.sum / slice.length) // mean
      intensityRet += meanIntensity
    }

    // TODO: precise calculation on position
    val position = (19785 / runtimeConfig.gaugeLength).ceil.toInt
    matlabEngine.eval("figure;")
    (0 until 9).foreach { i =>
      matlabEngine.eval(s"subplot(3,3,${i + 1})")
      matlabEngine.putVariable("target", phaseRet.transpose.apply(position + i - 4).toArray)
      matlabEngine.eval("plot(target)")
    }

    matlabEngine.eval("figure;")
    matlabEngine.putVariable("target", intensityRet.apply(9))
    matlabEngine.eval("plot(target)")
    StdIn.readLine()
  }
}

object ShowGen {
  def main(args: Array[String]): Unit = {
    matlabEngine.eval("load('/home/ltr/sysudas/code/matlab/dataFromOscil/100ns_4k_1.mat');")
    matlabEngine.eval("dataIn = Channel_1.Data(1:5000000);") // 80 pulses
    matlabEngine.eval("dataIn = double(dataIn);")
    val data = matlabEngine.getVariable[Array[Double]]("dataIn")
    DoDas(DasStaticConfig(), DasRuntimeConfig(10, 24.9, 5e6, 31), data)
  }
}
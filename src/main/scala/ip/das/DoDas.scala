package org.datenlord
package ip.das

import breeze.math._
import breeze.numerics.atan
import breeze.numerics.constants._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

/** DAS signal processing algorithm
 *
 */
object DoDas {
  /**
   * @param staticConfig hardware configurations
   * @param runtimeConfig user configurations, defined by (gaugeLength, probeLength, bandWidth)
   * @param dataIn data tobe demodulated
   * @return phase and intensity output
   */
  def apply(
             staticConfig: DasStaticConfig,
             runtimeConfig: DasRuntimeConfig,
             dataIn: Array[Double]
           ) = {

    // parameters generation
    val constants = staticConfig.genConstants()
    val regValues = runtimeConfig.genRegValues(staticConfig)
    import constants._
    import regValues._

    val coeffsReal = realCoeffGroups(runtimeConfig.bandWidth)
    val coeffsImag = imagCoeffGroups(runtimeConfig.bandWidth)
    val taps = coeffsReal.length

    // functions for signal processing
    def conv(data: Array[Double], coeff: Array[Double]) = {
      val padSize = coeff.length - 1
      val paddedData = Array.fill(padSize)(0.0) ++ data ++ Array.fill(padSize)(0.0)
      (0 until data.length + coeff.length - 1).map { i =>
        val dataSlice = paddedData.slice(i, i + coeff.length)
        dataSlice.zip(coeff).map { case (d, c) => d * c }.sum
      }.toArray
    }

    def pointwiseUnwrapNormalized(prev: Double, next: Double) = {
      val m = prev.floor
      val candidates = Seq(-1, 0, 1).map(_ + m + (next - next.floor))
      val shuffled = Random.shuffle(candidates) // TODO: hardware implementation?
      shuffled.find(ret => (ret - prev).abs <= 1 && (ret - next).abs.round % 2 == 0).get
    }

    def angle(complex: Complex) = {
      import complex._
      val temp = atan(imag / real)
      val ret = {
        if (temp > 0 && real < 0) temp - Pi
        else if (temp < 0 && real < 0) temp + Pi
        else if (real == 0 && imag > 0) Pi / 2
        else if (real == 0 && imag < 0) -Pi / 2
        else temp
      }
      ret
    }

    // statistics during signal processing
    var filteredMax = 0.0
    var phaseDiffMax = 0.0
    var finalPhaseMax = 0.0
    var finalIntensityMax = 0.0

    // buffers used during signal processing
    var pulseRam = Array[Double]()
    var meanRam = Array[Double]()
    val phaseRet = ArrayBuffer[Array[Double]]()
    val intensityRet = ArrayBuffer[Array[Double]]()

    /** --------
     * start signal processing
     * -------- */
    val pulses = dataIn.grouped(pulsePoints).toSeq
    pulses.zipWithIndex.foreach { case (pulse, i) => // pulse by pulse
      println(s"currently running on pulse ${i + 1}/${pulses.length}")
      // filtering
      val filteredReal = conv(pulse, coeffsReal).drop(taps)
      val filteredImag = conv(pulse, coeffsImag).drop(taps)
      filteredMax = filteredMax max filteredReal.max
      val dataComplex = filteredReal.zip(filteredImag).map { case (r, i) => Complex(r, i) }
      val phases = dataComplex.map(angle).map(_ / Pi) // / pi for normalization
      val intensities = dataComplex.map(_.abs)
      // phase path
      val diffPulse = phases.take(gaugePoints) ++ // keep the first segment, size unchanged
        phases.drop(gaugePoints).zip(phases.dropRight(gaugePoints)).map { case (next, prev) => next - prev } // diff
      val unwrappedPulse =
        if (pulseRam.isEmpty) diffPulse
        else pulseRam.zip(diffPulse).map { case (prev, next) => pointwiseUnwrapNormalized(prev, next) } // unwrap, size unchanged
      val meanPulse = unwrappedPulse.grouped(gaugePoints)
        .toArray.map(slice => slice.sum / slice.length) // mean
      val unwrappedMeanPulse = if (meanRam.isEmpty) meanPulse else meanRam.zip(meanPulse).map { case (prev, next) => pointwiseUnwrapNormalized(prev, next) } // unwrap

      pulseRam = unwrappedPulse
      meanRam = unwrappedMeanPulse

      phaseDiffMax = phaseDiffMax max diffPulse.max
      finalPhaseMax = finalPhaseMax max unwrappedMeanPulse.take((20000 / runtimeConfig.gaugeLength).toInt).max
      //      logger.info(s"current final phase max: $finalPhaseMax")

      phaseRet += unwrappedMeanPulse // add data from a stage you want for debugging

      // intensity path
      val meanIntensity = intensities.grouped(gaugePoints).toArray.map(slice => slice.sum / slice.length) // mean
      intensityRet += meanIntensity
      finalIntensityMax = finalIntensityMax max meanIntensity.max
    }

    logger.info( // data range statistics
      s"\n----max values for each steps----" +
        s"\n\tfir -> $filteredMax" +
        s"\n\tdiff -> $phaseDiffMax" +
        s"\n\tphaseOut -> $finalPhaseMax" +
        s"\n\tintensity -> $finalIntensityMax"
    )

    /** --------
     * show results
     * -------- */
    val position = (runtimeConfig.probePosition / runtimeConfig.gaugeLength).ceil.toInt
    logger.info(s"ret size: ${phaseRet.length}, ${phaseRet.head.length}")
    (0 until 9).foreach { i =>
      val current = position + i - 4
      matlabEngine.eval(s"subplot(3,3,${i + 1})")
      matlabEngine.putVariable("target", phaseRet.transpose.apply(current).toArray)
      matlabEngine.eval("plot(target)")
      matlabEngine.eval(s"title('${current * runtimeConfig.gaugeLength / 1e3} km')")
    }
    matlab.SaveCurrentFigure(s"doDasPhaseOutput_$pulsePoints")

    (phaseRet, intensityRet)
  }
}

object ShowGen {
  def main(args: Array[String]): Unit = {
    // experiment 1
    matlabEngine.eval("load('/home/ltr/sysudas/code/matlab/dataFromOscil/100ns_4k_1.mat');")
    matlabEngine.eval("dataIn = Channel_1.Data(1:2500000);") // 40 pulses
    matlabEngine.eval("dataIn = double(dataIn);")
    matlabEngine.eval("dataIn = dataIn ./ max(abs(dataIn));") // normalization
    val data250 = matlabEngine.getVariable[Array[Double]]("dataIn")
    DoDas(DasStaticConfig(), DasRuntimeConfig(10.4, 24.9, 5e6, 31, 19785), data250)
    // experiment 2
    matlabEngine.eval("load('/home/ltr/sysudas/code/matlab/dataFromOscil/100ns_4k_1.mat');")
    matlabEngine.eval("dataIn = Channel_1.Data(1:2500000);") // 40 pulses
    matlabEngine.eval("dataIn = double(dataIn);")
    matlabEngine.eval("dataIn = dataIn ./ max(abs(dataIn));") // normalization
    matlabEngine.eval("dataIn = resample(dataIn, 4, 5);")
    val data200 = matlabEngine.getVariable[Array[Double]]("dataIn")
    DoDas(DasStaticConfig(samplingFreq = 200e6, sigProFreq = 100e6),
      DasRuntimeConfig(11, 24.9, 5e6, 31, 19785), data200)
  }
}
package org.datenlord
package ip.das

import matlab.{MComplex, MatlabFeval}

import breeze.math._
import breeze.numerics.constants._

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.language.postfixOps
import scala.util.Random

/** DAS signal processing algorithm
 *
 */
object DoDas {
  def apply(
             staticConfig: DasStaticConfig,
             runtimeConfig: DasRuntimeConfig,
             dataIn: Array[Double]) = {

    val constants = staticConfig.genConstants()
    val regValues = runtimeConfig.genRegValues(staticConfig)

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

    def pointwiseUnwrapNormalized(prev: Double, next: Double) = {
      val m = prev.floor
      val candidates = Seq(-1, 0, 1).map(_ + m + (next - next.floor))
      val shuffled = Random.shuffle(candidates) // TODO: hardware implementation?
      shuffled.find(ret => (ret - prev).abs <= 1 && (ret - next).abs.round % 2 == 0).get
    }

    def pointwiseUnwrap(prev: Double, next: Double): Double = pointwiseUnwrapNormalized(prev / Pi, next / Pi) * Pi

    val coeffsReal = realCoeffGroups(runtimeConfig.bandWidth)
    val coeffsImag = imagCoeffGroups(runtimeConfig.bandWidth)

    // draw coeffs
    //    matlabEngine.eval("figure;")
    //    matlabEngine.putVariable("real", coeffsReal)
    //    matlabEngine.putVariable("imag", coeffsImag)
    //    matlabEngine.eval("subplot(1,2,1); plot(real);")
    //    matlabEngine.eval("subplot(1,2,2); plot(imag);")

    var filteredMax = 0.0
    var phaseDiffMax = 0.0
    var finalPhaseMax = 0.0
    var finalIntensityMax = 0.0

    pulses.zipWithIndex.foreach { case (pulse, i) =>
      println(s"currently running on pulse ${i + 1}/${pulses.length}")
      // filtering
      val filteredReal = conv(pulse, coeffsReal)
      val filteredImag = conv(pulse, coeffsImag)
      filteredMax = filteredMax max filteredReal.max
      val dataComplex = filteredReal.zip(filteredImag).map { case (r, i) => Complex(r, i) }
      val phases = matlab.MatlabFeval[Array[Double]]("angle", 0, dataComplex.map(complex => new MComplex(complex.real, complex.imag)))
      val intensities = dataComplex.map(_.abs)

      // phase path
      val diffPulse = phases.drop(gaugePoints).zip(phases.dropRight(gaugePoints)).map { case (next, prev) => next - prev } // diff
      phaseDiffMax = phaseDiffMax max diffPulse.max
      val unwrappedPulse = if (pulseRam.isEmpty) diffPulse else pulseRam.zip(diffPulse).map { case (prev, next) => pointwiseUnwrap(prev, next) } // unwrap
      pulseRam = unwrappedPulse
      val meanPulse = unwrappedPulse.grouped(gaugePoints).toArray.map(slice => slice.sum / slice.length) // mean
      val unwrappedMeanPulse = if (meanRam.isEmpty) meanPulse else meanRam.zip(meanPulse).map { case (prev, next) => pointwiseUnwrap(prev, next) } // unwrap
      finalPhaseMax = finalPhaseMax max unwrappedMeanPulse.take((20000 / runtimeConfig.gaugeLength).toInt).max
      logger.info(s"current final phase max: $finalPhaseMax")
      meanRam = unwrappedMeanPulse
      //      phaseRet += unwrappedMeanPulse
      phaseRet += filteredReal

      // intensity path
      val meanIntensity = intensities.grouped(gaugePoints).toArray.map(slice => slice.sum / slice.length) // mean
      intensityRet += meanIntensity
      finalIntensityMax = finalIntensityMax max meanIntensity.max
    }

    logger.info(
      s"\n----max values for each steps----" +
        s"\n\tfir -> $filteredMax" +
        s"\n\tdiff -> $phaseDiffMax" +
        s"\n\tphaseOut -> $finalPhaseMax" +
        s"\n\tintensity -> $finalIntensityMax"
    )

    // TODO: precise calculation on position

    // draw phase
    //    val position = (19785 / runtimeConfig.gaugeLength).ceil.toInt
    //    val position = 0 + 76 + 4
    //    matlabEngine.eval("figure;")
    //    (0 until 9).foreach { i =>
    //      matlabEngine.eval(s"subplot(3,3,${i + 1})")
    //      matlabEngine.putVariable("target", phaseRet.transpose.apply(position + i - 4).toArray)
    //      matlabEngine.eval("plot(target)")
    //    }

    // draw intensity
    //    matlabEngine.eval("figure;")
    //    matlabEngine.putVariable("target", intensityRet.apply(9))
    //    matlabEngine.eval("plot(target)")
    (phaseRet, intensityRet)
  }
}

object ShowGen {
  def main(args: Array[String]): Unit = {
    matlabEngine.eval("load('/home/ltr/sysudas/code/matlab/dataFromOscil/100ns_4k_1.mat');")
    matlabEngine.eval("dataIn = Channel_1.Data(1:2500000);") // 40 pulses
    matlabEngine.eval("dataIn = double(dataIn);")
    matlabEngine.eval("dataIn = dataIn ./ max(abs(dataIn));") // normalization
    val data = matlabEngine.getVariable[Array[Double]]("dataIn")
    DoDas(DasStaticConfig(), DasRuntimeConfig(10, 24.9, 5e6, 31), data)
  }
}
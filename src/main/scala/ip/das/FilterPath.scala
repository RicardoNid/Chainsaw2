package org.datenlord
package ip.das

import dsp.AlgebraicMode._
import dsp.RotationMode._
import dsp._
import intel.QuartusFlow
import matlab.MComplex

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps

case class FilterPathConfig(dasConfig: DasConfig) extends TransformBase {

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[Double]]
    val coeffs = dasConfig.combinedCoeffGroups.head
    val filtered = matlab.MatlabFeval[Array[MComplex]]("upfirdn", 0, data.toArray, coeffs, dasConfig.upSampleFactor.toDouble, 1.toDouble)
    matlab.MatlabFeval[Array[Double]]("angle", 0, filtered).drop(coeffs.length - 1)
  }

  override val implMode = Infinite

  // at 125MHz, first subFilterCount are intensities and the next subFilterCount are phases
  override val size = (2, dasConfig.subFilterCount)

  logger.info(s"upsample: ${dasConfig.upSampleFactor}, subfilters: ${dasConfig.subFilterCount}")

  val coeffsReal = dasConfig.realCoeffGroups.head
  val coeffsImag = dasConfig.imagCoeffGroups.head
  // TODO: implement precision coeffs in dasConfig
  val adcDataType = HardType(SFix(0 exp, -13 exp))
  val firOutDataType = HardType(SFix(4 exp, -13 exp))

  val realFirConfig = UpFirDnAnotherConfig(dasConfig.upSampleFactor, 2, coeffsReal, adcDataType, firOutDataType)
  val imagFirConfig = UpFirDnAnotherConfig(dasConfig.upSampleFactor, 2, coeffsImag, adcDataType, firOutDataType)

  val cordicConfig = CordicConfig(CIRCULAR, VECTORING, dasConfig.cordicIteration, dasConfig.cordicFraction)

  //  override def latency = realFirConfig.latency

  override def latency = realFirConfig.latency + cordicConfig.latency

  override def implH = FilterPath(this)

}

case class FilterPath(config: FilterPathConfig) extends TransformModule[SFix, SFix] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(adcDataType, inputPortWidth))

  //  def outputSeq = Seq.fill(dasConfig.subFilterCount)(cordicConfig.amplitudeType()) ++ Seq.fill(dasConfig.subFilterCount)(cordicConfig.phaseType())
  def outputSeq = Seq.fill(dasConfig.subFilterCount)(cordicConfig.phaseType())

  override val dataOut = master Flow Fragment(Vec(outputSeq))

  val realFirRets = realFirConfig.implH.asFunc(dataIn.fragment)
    .map(_ >> 4) // normalization
  val imagFirRets = imagFirConfig.implH.asFunc(dataIn.fragment)
    .map(_ >> 4) // normalization

  logger.info(s"max exp = ${realFirRets.head.maxExp}")

  realFirRets.foreach(_.simPublic())
  imagFirRets.foreach(_.simPublic())

  val both = realFirRets.zip(imagFirRets).map { case (real, imag) =>
    val core = cordicConfig.implH
    core.dataIn.fragment := Vec(real.truncated, imag.truncated, SFConstant(0.0, cordicConfig.phaseType))
    core.skipControl()
    core.dataOut.fragment
  }

  val intensities = both.map(_.head)
  val phases = both.map(_.last)

  dataOut.fragment.zip(phases).foreach { case (port, value) => port := value }
  //  dataOut.fragment.zip(realFirRets).foreach { case (port, value) => port := value }
  //    dataOut.fragment.zip(imagFirRets).foreach { case (port, value) => port := value }

  autoValid()
  autoLast()
}

object FilterPath {
  def main(args: Array[String]): Unit = {
    //    SpinalConfig().generateVerilog(FilterPathConfig(DasConfig(samplingFreq = 1e9)).implH)
    new QuartusFlow(FilterPathConfig(DasConfig(samplingFreq = 1e9)).implH, "filterpath").impl()
  }
}

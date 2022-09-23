package org.datenlord
package ip.das

import intel.QuartusFlow

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class SignalProTest extends AnyFlatSpec {

  val test200 = false

  implicit val staticConfig: DasStaticConfig = DasStaticConfig(
    samplingFreq = if (test200) 200e6 else 250e6,
    sigProFreq = if (test200) 100e6 else 125e6
  )
  val runtimeConfig = DasRuntimeConfig(
    gaugeLength = if (test200) 11 else 10.4,
    probeLength = 24.9,
    bandWidth = 5e6,
    gain = 31,
    probePosition = 19785)

  val constants = staticConfig.genConstants()
  val regValues = runtimeConfig.genRegValues(staticConfig)

  import constants._
  import regValues._

  val testPulses = 80
  val testType = 0

  val data = testType match {
    case 0 => // valid data from oscil
      matlabEngine.eval("load('/home/ltr/sysudas/code/matlab/dataFromOscil/100ns_4k_1.mat');")
      matlabEngine.eval(s"dataIn = Channel_1.Data(1:${62500 * testPulses});") // 250MHz -> 62500
      matlabEngine.eval("dataIn = double(dataIn);")
      if (test200) matlabEngine.eval("dataIn = resample(dataIn, 4, 5);")
      matlabEngine.eval("dataIn = dataIn ./ max(abs(dataIn));") // normalization
      matlabEngine.getVariable[Array[Double]]("dataIn")
  }

  behavior of "DAS signal processing"

  it should "work" in {

    val meshSize = 3
    val simName = "testDasSigPro"

    SimConfig.workspaceName("testDasSigPro").withFstWave.compile(SignalPro()).doSim { dut =>

      /** --------
       * initialize runtime parameters
       * -------- */
      dut.flowIn.modeChange #= true
      dut.flowIn.pulseChange #= true
      dut.gaugePointsIn #= gaugePoints.divideAndCeil(subFilterCount)
      dut.gaugeReverseIn #= 1.0 / gaugePoints.nextMultiple(subFilterCount)
      dut.pulsePointsIn #= pulsePoints.divideAndCeil(subFilterCount)
      dut.spatialPointsIn #= spatialPoints
      //            dut.positionIn #= position
      dut.flowIn.valid #= false
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()

      val pulses = data.grouped(pulsePoints).toSeq
      val ret = Seq.fill(pulses.length)(ArrayBuffer[Double]())

      var pokeStart = false // pole start after first pulseChange signal

      /** --------
       * poke pulses & peek output
       * -------- */
      pulses.zipWithIndex.foreach { case (pulse, i) =>
        println(s"currently simulating on pulse ${i + 1}/${pulses.length}")
        val dataGrouped = pulse.grouped(subFilterCount).toSeq
        dataGrouped.zipWithIndex.foreach { case (vec, j) =>
          if (pokeStart && dut.flowOut.valid.toBoolean && dut.flowOut.index.toInt < ret.length) {
            ret(dut.flowOut.index.toInt) ++= dut.flowOut.payload.map(_.toDouble)
          }
          dut.flowIn.valid #= true
          dut.flowIn.payload.zip(vec).foreach { case (port, data) => port #= data }
          dut.flowIn.index #= i
          dut.flowIn.modeChange #= false // deassert after initialization
          dut.flowIn.pulseChange #= (j == dataGrouped.length - 1) // asserted before next pulse
          dut.clockDomain.waitSampling()
          if (!pokeStart) pokeStart = dut.flowOut.pulseChange.toBoolean
        }
      }

      /** --------
       * peek tailing part of the output
       * -------- */
      (0 until 1000).foreach { _ =>
        if (dut.flowOut.valid.toBoolean && dut.flowOut.index.toInt < ret.length) ret(dut.flowOut.index.toInt) ++= dut.flowOut.payload.map(_.toDouble)
        dut.flowIn.valid #= false
        dut.clockDomain.waitSampling()
      }

      val goldenPhase = DoDas(staticConfig, runtimeConfig, data)._1

      /** --------
       * show results
       * -------- */
      logger.info(s"size of input: ${pulses.length} * ${pulses.head.length}")
      logger.info(s"size of ret: ${ret.length} * ${ret.head.length}")
      logger.info(s"size of pulses in ret: ${ret.map(_.length).mkString(" ")}")
      logger.info(s"size of ret: ${goldenPhase.length} * ${goldenPhase.head.length}")

      // compare point by point
      (0 until meshSize * meshSize).foreach { i =>
        matlabEngine.eval(s"subplot($meshSize,$meshSize,${i + 1})")
        val shift = meshSize * meshSize / 2
        val yours = ret.transpose.apply(position + i - shift).toArray
        val golden = goldenPhase.transpose.apply(position + i - shift).toArray
        matlab.CompareData(yours, golden, name = s"fig_$i")
      }
      matlab.SaveCurrentFigure(s"point-by-point")

      // compare pulse by pulse
      val time = 20
      matlab.CompareData(ret.apply(time).take(200), goldenPhase.apply(time).take(200), name = s"compare")
      matlab.SaveCurrentFigure(s"pulse-by-pulse")

      // when extractor is connected, show the curve at a specific position
      //      matlab.CompareData(ret.flatten, ret.flatten, name = s"compare")
      //      matlab.SaveCurrentFigure(s"all")
    }
  }

  it should "synth for filter path" in new QuartusFlow(FilterPath()).impl()
  it should "synth for diff" in new QuartusFlow(PhaseDiff()).impl()
  it should "synth for unwrap0" in new QuartusFlow(PulseUnwrap()).impl()
  it should "synth for mean" in new QuartusFlow(PhaseMean()).impl()
  it should "synth for unwrap1" in new QuartusFlow(MeanUnwrap()).impl()
  it should "synth for full module" in new QuartusFlow(SignalPro()).impl()
}

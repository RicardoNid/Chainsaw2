package org.datenlord
package ip.das

import dsp.Unwrap

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class PhasePathTest extends AnyFlatSpec {

  "phase path" should "work" in {
    import scala.util.Random

    val dasConfig = DasConfig(samplingFreq = 1e9)
    val pulseCount = 10
    val gaugeLength = 20
    val probeLength = 0.9
    val pulsePoints = dasConfig.pulsePoints(probeLength)
    val gaugePoints = dasConfig.gaugePoints(gaugeLength)
    val spatialPoints = dasConfig.spatialPoints(probeLength, gaugeLength)
    val phaseData = Seq.fill(pulsePoints * pulseCount)(Random.nextDouble() * 2 - 1) // normalized phase data

    val golden = phaseData.grouped(pulsePoints).toSeq // pulse by pulse
      .map(pulse => pulse.drop(gaugePoints).zip(pulse).map { case (next, prev) => next - prev }) // diff
      .transpose.map(Unwrap.unwrap).transpose // unwrap
      .map(pulse => pulse.grouped(gaugePoints).toSeq.map(_.sum / gaugePoints)) // mean
      .transpose.map(Unwrap.unwrap).transpose // unwrap
      .flatten


    logger.info(s"$pulsePoints cycles for a pulse")

    SimConfig.withFstWave.compile(PhasePath(dasConfig)).doSim { dut =>

      dut.clockDomain.forkStimulus(2)
      // initialize
      dut.flowIn.pulseChange #= true
      dut.flowIn.modeChange #= true
      dut.pulsePointsIn #= pulsePoints
      dut.gaugePointsIn #= gaugePoints
      dut.meanPointsIn #= spatialPoints
      dut.clockDomain.waitSampling()

      val retBuffer = ArrayBuffer[Double]()

      phaseData.zipWithIndex.foreach { case (phase, i) =>
        dut.flowIn.payload #= phase
        dut.flowIn.pulseChange #= (i % pulsePoints == pulsePoints - 1)
        dut.flowIn.modeChange #= false
        dut.clockDomain.waitSampling()
        if (dut.valid.toBoolean) retBuffer += dut.flowOut.payload.toDouble
      }

      val valids = retBuffer.grouped(pulsePoints / gaugePoints).toSeq.flatMap(_.tail)
      logger.info(s"spatial points: $spatialPoints")
      logger.info(s"gauge points: $gaugePoints")
      logger.info(s"number of ret: ${valids.length}, expected: ${pulseCount * (pulsePoints / gaugePoints - 1)}")
      logger.info(valids.mkString(" "))
      logger.info(golden.mkString(" "))

      // draw figures pulse by pulse
      (0 until pulseCount).foreach { i =>
        val width = spatialPoints - 1
        matlabEngine.eval(s"subplot(2,5,${i + 1})")
        matlab.CompareData(valids.slice(i * width, (i + 1) * width), golden.slice(i * width, (i + 1) * width), s"compare_$i")
        matlab.SaveCurrentFigure("phase_test_pulse_by_pulse")
      }

      matlab.CompareData(valids, golden, "compare")
      matlab.SaveCurrentFigure("phase_path_test")
      assert(golden.zip(valids).forall { case (g, y) => (g - y).abs < 1e-1 })
    }

  }

}

package org.datenlord
package ip.das

import breeze.numerics.abs
import breeze.numerics.constants.Pi
import org.datenlord.intel.QuartusFlow
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer

class PhaseDiffTest extends AnyFlatSpec {

  val gaugePointsMax = 1000
  val phaseType = HardType(SFix(1 exp, -14 exp)) // [-1, 1] for normalized phase
  val phaseDiffType = HardType(SFix(2 exp, -14 exp)) // [-2, 2] for phase difference
  def dut = PhaseDiff(gaugePointsMax, phaseType, phaseDiffType)

  "phase diff" should "work" in {

    import scala.util.Random

    val phaseData = Seq.fill(2000)(Random.nextDouble() * 2 - 1)
    val pulsePoints0 = 100
    val gaugePoints0 = 25
    val pulsePoints1 = 200
    val gaugePoints1 = 40

    SimConfig.withFstWave.compile(dut).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      // initialize
      dut.flowIn.pulseChange #= true
      dut.flowIn.modeChange #= true
      dut.gaugePointsIn #= gaugePoints0
      dut.clockDomain.waitSampling()
      val retBuffer = ArrayBuffer[Double]()

      // work under mode 0
      phaseData.indices.foreach { i =>
        val phase = phaseData(i)
        dut.flowIn.payload #= phase
        dut.flowIn.pulseChange #= (i % pulsePoints0 == pulsePoints0 - 1)
        dut.flowIn.modeChange #= false
        dut.clockDomain.waitSampling()
        retBuffer += dut.flowOut.payload.toDouble
      }

      phaseData.grouped(pulsePoints0).toSeq
        .zip(retBuffer.grouped(pulsePoints0).toSeq)
        .foreach { case (raw, yours) =>
          val diff = raw.drop(gaugePoints0).zip(raw).map{ case (next, prev) => next - prev}
          assert(diff.zip(yours.slice(gaugePoints0 + 1, pulsePoints0)).forall { case (g, y) => abs(g - y) < 1e-2 })
        }

      // disturb
      (0 until 100).indices.foreach { i =>
        val phase = Random.nextDouble()
        dut.flowIn.payload #= phase
        dut.gaugePointsIn #= Random.nextInt(10)
        dut.flowIn.pulseChange #= Random.nextBoolean()
        dut.flowIn.modeChange #= false
        dut.clockDomain.waitSampling()
      }

      // recover
      retBuffer.clear()
      dut.flowIn.pulseChange #= true
      dut.flowIn.modeChange #= true
      dut.gaugePointsIn #= gaugePoints1
      dut.clockDomain.waitSampling()

      // work under mode 1
      phaseData.indices.foreach { i =>
        val phase = phaseData(i)
        dut.flowIn.payload #= phase
        dut.flowIn.pulseChange #= (i % pulsePoints1 == pulsePoints1 - 1)
        dut.flowIn.modeChange #= false
        dut.clockDomain.waitSampling()
        retBuffer += dut.flowOut.payload.toDouble
      }

      phaseData.grouped(pulsePoints1).toSeq
        .zip(retBuffer.grouped(pulsePoints1).toSeq)
        .foreach { case (raw, yours) =>
          val diff = raw.drop(gaugePoints1).zip(raw).map{ case (next, prev) => next - prev}
          assert(diff.zip(yours.slice(gaugePoints1 + 1, pulsePoints1)).forall { case (g, y) => abs(g - y) < 1e-2 })
        }
    }
  }

  it should "synth" in new QuartusFlow(dut).impl()
}

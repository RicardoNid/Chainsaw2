package org.datenlord
package ip.das

import breeze.numerics.abs
import breeze.numerics.constants.Pi
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._
import spinal.core._

import scala.collection.mutable.ArrayBuffer

class PhaseMeanTest extends AnyFlatSpec {

  val typeIn = HardType(SFix(6 exp, -13 exp)) // [-1, 1] for normalized phase
  val gaugePointsMax = 1000
  val gaugePointsMin = 100

  "phase mean" should "work" in {
    import scala.util.Random

    val phaseData = Seq.fill(20000)(Random.nextDouble() * 2 * Pi - Pi)
    val pulsePoints0 = 2000
    val gaugePoints0 = 400
    val pulsePoints1 = 4000
    val gaugePoints1 = 1000

    SimConfig.withFstWave.compile(PhaseMean(gaugePointsMax, gaugePointsMin, typeIn)).doSim { dut =>
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
        if (dut.valid.toBoolean) retBuffer += dut.flowOut.payload.toDouble
      }

      val golden0 = phaseData.grouped(gaugePoints0).toSeq.map(_.sum / gaugePoints0)
      assert(golden0.zip(retBuffer).forall{ case (g, y) => abs(g-y) < 1e-2})

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
      retBuffer.clear()

      // work under mode 1
      phaseData.indices.foreach { i =>
        val phase = phaseData(i)
        dut.flowIn.payload #= phase
        dut.flowIn.pulseChange #= (i % pulsePoints1 == pulsePoints1 - 1)
        dut.flowIn.modeChange #= false
        dut.clockDomain.waitSampling()
        if (dut.valid.toBoolean) retBuffer += dut.flowOut.payload.toDouble
      }

      val golden1 = phaseData.grouped(gaugePoints1).toSeq.map(_.sum / gaugePoints1)
      assert(golden1.zip(retBuffer).forall{ case (g, y) => abs(g-y) < 1e-2})

    }
  }
}

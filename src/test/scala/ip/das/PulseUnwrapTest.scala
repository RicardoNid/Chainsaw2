package org.datenlord
package ip.das

import intel.QuartusFlow

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class PulseUnwrapTest extends AnyFlatSpec {

  val typeFull = HardType(SFix(6 exp, -13 exp)) // [-1, 1] for normalized phase
  val typeStored = HardType(SFix(6 exp, -4 exp)) // [-2, 2] for phase difference
  val pulsePointsMax = 8e5.toInt / 16
  def dut = PulseUnwrap(pulsePointsMax, typeFull, typeStored)

  "pulse unwrap" should "work" in {
    import scala.util.Random

    val phaseData = Seq.fill(100)(Random.nextDouble())
    val pulsePoints0 = 10
    val pulsePoints1 = 20

    SimConfig.withFstWave.compile(dut).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      // initialize
      dut.flowIn.pulseChange #= true
      dut.flowIn.modeChange #= true
      dut.pulsePointsIn #= pulsePoints0
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

      println(phaseData.mkString(" "))
      println(retBuffer.mkString(" "))

      // change mode
      dut.flowIn.pulseChange #= true
      dut.flowIn.modeChange #= true
      dut.pulsePointsIn #= pulsePoints1
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
    }
  }

  it should "synth" in new QuartusFlow(dut).impl()
}

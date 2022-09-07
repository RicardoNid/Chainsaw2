package org.datenlord
package ip.das

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.{HardType, IntToBuilder, SFix}
import spinal.core.sim.{SimConfig, _}

import scala.collection.mutable.ArrayBuffer

class MeanUnwrapTest extends AnyFlatSpec {

  val typeFull = HardType(SFix(6 exp, -13 exp)) // [-1, 1] for normalized phase
  val typeStored = HardType(SFix(6 exp, -4 exp)) // [-2, 2] for phase difference
  val meanPointsMax = 8e3.toInt

  "mean unwrap" should "work" in {
    import scala.util.Random

    val phaseData = Seq.fill(100)(Random.nextDouble())
    val meanPoints0 = 10
    val meanPoints1 = 20

    SimConfig.withFstWave.compile(MeanUnwrap(meanPointsMax, typeStored, typeFull)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      // initialize
      dut.validIn #= false
      dut.flowIn.pulseChange #= true
      dut.flowIn.modeChange #= true
      dut.meanPointsIn #= meanPoints0
      dut.clockDomain.waitSampling()
      val retBuffer = ArrayBuffer[Double]()

      // work under mode 0
      phaseData.indices.foreach { i =>
        (0 until meanPoints0 - 1).foreach { _ =>
          dut.flowIn.payload #= Random.nextDouble()
          dut.validIn #= false
          dut.flowIn.pulseChange #= false
          dut.flowIn.modeChange #= false
          dut.clockDomain.waitSampling()
          if(dut.validOut.toBoolean) retBuffer += dut.flowOut.payload.toDouble
        }
        val phase = phaseData(i)
        dut.flowIn.payload #= phase
        dut.validIn #= true
        dut.clockDomain.waitSampling()
        if(dut.validOut.toBoolean) retBuffer += dut.flowOut.payload.toDouble
      }

      println(phaseData.mkString(" "))
      println(retBuffer.mkString(" "))
      println(phaseData.zip(retBuffer).map{ case (g, y) => g - y}.mkString(" "))

      // change mode
      dut.validIn #= false
      dut.flowIn.pulseChange #= true
      dut.flowIn.modeChange #= true
      dut.meanPointsIn #= meanPoints1
      dut.clockDomain.waitSampling()
      retBuffer.clear()

      // work under mode 1
      phaseData.indices.foreach { i =>
        (0 until meanPoints1 - 1).foreach { _ =>
          dut.flowIn.payload #= Random.nextDouble()
          dut.validIn #= false
          dut.flowIn.pulseChange #= false
          dut.flowIn.modeChange #= false
          dut.clockDomain.waitSampling()
          if(dut.validOut.toBoolean) retBuffer += dut.flowOut.payload.toDouble
        }
        val phase = phaseData(i)
        dut.flowIn.payload #= phase
        dut.validIn #= true
        dut.clockDomain.waitSampling()
        if(dut.validOut.toBoolean) retBuffer += dut.flowOut.payload.toDouble
      }

      println()
      println(phaseData.mkString(" "))
      println(retBuffer.mkString(" "))
      println(phaseData.zip(retBuffer).map{ case (g, y) => g - y}.mkString(" "))
    }
  }

}

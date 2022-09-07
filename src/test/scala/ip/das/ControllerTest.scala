package org.datenlord
package ip.das

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer

class ControllerTest extends AnyFlatSpec {

  "controller" should "generate pulse correctly" in {

    import scala.util.Random

    def sim(changePoint: Int) = {
      SimConfig.withFstWave.compile(Controller()).doSim { dut =>

        dut.clockDomain.forkStimulus(2)
        dut.clockDomain.waitSampling()

        val pulseTime = ArrayBuffer[Int]()
        val pulseBackTime = ArrayBuffer[Int]()

        (0 until 500).foreach { i =>
          if (i < changePoint) {
            dut.pulsePointsIn #= 20
            dut.pulseBackIn #= 10
          }
          else {
            dut.pulsePointsIn #= 30
            dut.pulseBackIn #= 20
          }
          if (dut.pulse.toBoolean) pulseTime += i
          if (dut.pulseBack.toBoolean) pulseBackTime += i
          dut.clockDomain.waitSampling()
        }

        val pulseGaps = pulseTime.prevAndNext { case (prev, next) => next - prev }
        val pulseBackGaps = pulseTime.zip(pulseBackTime).map { case (sent, back) => back - sent }

        assert(pulseGaps.forall(gap => gap == 20 || gap == 30))
        assert(pulseBackGaps.forall(gap => gap == 10 || gap == 20))
      }
    }

    (200 until 230).foreach(sim)
  }

}

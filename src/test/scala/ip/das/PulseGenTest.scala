package org.datenlord
package ip.das

import org.scalatest.flatspec.AnyFlatSpec

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class PulseGenTest extends AnyFlatSpec {

  implicit val config = DasStaticConfig()

  "pulse gen" should "work" in {
    SimConfig.withFstWave.compile(PulseGen()).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.pulsePeriodIn #= 1000
      dut.clockDomain.waitSampling(2500)
      dut.pulsePeriodIn #= 500
      dut.clockDomain.waitSampling(1800)
    }
  }


}

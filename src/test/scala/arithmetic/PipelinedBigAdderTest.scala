package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim.{SimConfig, _}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class PipelinedBigAdderTest extends AnyFlatSpec {

  val testCount = 1000
  val width = 256

  val xs = (0 until testCount).map(_ => nextBigInt(width))
  val ys = (0 until testCount).map(_ => nextBigInt(width))
  val goldens = xs.zip(ys).map { case (x, y) => x + y }

  val config = PipelinedBigAdderConfig(width, 66)

  "PipelinedBigAdder" should "work" in {

    SimConfig.withFstWave.compile(PipelinedBigAdder(config)).doSim { dut =>

      dut.clockDomain.forkStimulus(2)

      dut.dataIn.valid #= false
      dut.clockDomain.waitSampling()

      val rets = ArrayBuffer[BigInt]()

      xs.zip(ys).foreach { case (x, y) =>
        dut.dataIn.payload(0) #= x
        dut.dataIn.payload(1) #= y
        dut.dataIn.valid #= true
        if (dut.dataOut.valid.toBoolean) rets += dut.dataOut.payload.toBigInt
        dut.clockDomain.waitSampling()
      }

      (0 until config.latency).foreach { _ =>
        dut.dataIn.valid #= false
        if (dut.dataOut.valid.toBoolean) rets += dut.dataOut.payload.toBigInt
        dut.clockDomain.waitSampling()
      }

      rets.zip(goldens).foreach { case (ret, golden) => assert(ret == golden)}
    }

  }

}

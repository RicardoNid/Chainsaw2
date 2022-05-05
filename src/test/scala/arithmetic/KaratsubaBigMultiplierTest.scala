package org.datenlord
package arithmetic

import org.datenlord.device.KaratsubaForXilinx
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim.{SimConfig, _}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class KaratsubaBigMultiplierTest extends AnyFlatSpec {

  val testCount = 1000
  val width = 256

  val xs = (0 until testCount).map(_ => nextBigInt(width))
  val ys = (0 until testCount).map(_ => nextBigInt(width))
  val goldens = xs.zip(ys).map { case (x, y) => x * y }

  // configuration
  def baseMult(x: UInt, y: UInt) = {
    val core = KaratsubaForXilinx()
    core.dataIn.payload := Vec(x.resized, y.resized)
    core.dataIn.valid := True
    core.dataOut.payload
  }

  val config = KaratsubaBigMultiplierConfig(width, 34, baseMult, baseMultLatency = KaratsubaForXilinx.latency)

  "Karatsuba Multiplier " should "work" in {

    //    VivadoImpl(KaratsubaMultiplier(config))

    SimConfig.withFstWave.compile(KaratsubaBigMultiplier(config)).doSim { dut =>

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

      (0 until KaratsubaBigMultiplier.latency(config)).foreach { _ =>
        dut.dataIn.valid #= false
        if (dut.dataOut.valid.toBoolean) rets += dut.dataOut.payload.toBigInt
        dut.clockDomain.waitSampling()
      }

      rets.zip(goldens).foreach { case (ret, golden) => assert(ret == golden) }
    }
  }

  "Karatsuba Multiplier " should "impl" in VivadoImpl(KaratsubaBigMultiplier(config))

}

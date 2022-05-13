package org.datenlord
package arithmetic

import org.datenlord.device.KaratsubaForXilinx
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class KaratsubaForXilinxTest extends AnyFlatSpec {

  val testCount = 1000
  val width = 34

  val xs = (0 until testCount).map(_ => Random.nextBigInt(width))
  val ys = (0 until testCount).map(_ => Random.nextBigInt(width))
  val goldens = xs.zip(ys).map{ case (x, y) => x * y}

  "karatsuba for Xilinx" should "work" in {
    SimConfig.withFstWave.compile(KaratsubaForXilinx()).doSim{dut =>

      dut.clockDomain.forkStimulus(2)

      dut.dataIn.valid #= false
      dut.clockDomain.waitSampling()

      val rets = ArrayBuffer[BigInt]()

      xs.zip(ys).foreach{ case (x, y) =>
        dut.dataIn.payload(0) #= x
        dut.dataIn.payload(1) #= y
        dut.dataIn.valid #= true
        if(dut.dataOut.valid.toBoolean) rets += dut.dataOut.payload.toBigInt
        dut.clockDomain.waitSampling()
      }

      (0 until 6).foreach{ _ =>
        dut.dataIn.valid #= false
        if(dut.dataOut.valid.toBoolean) rets += dut.dataOut.payload.toBigInt
        dut.clockDomain.waitSampling()
      }

       rets.zip(goldens).foreach{ case (ret, golden) =>
         println(s"$ret = $golden")
         assert(ret == golden)
       }
    }
  }


}

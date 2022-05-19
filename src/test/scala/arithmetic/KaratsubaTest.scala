package org.datenlord
package arithmetic

import org.datenlord.device.KaratsubaForXilinx
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim.{SimConfig, _}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class KaratsubaTest extends AnyFlatSpec {

  val testCount = 1000
  val width = 256
  val data = (0 until testCount * 2).map(_ => Random.nextBigInt(width))

  // configuration
  def baseMult(x: UInt, y: UInt) = {
    val core = KaratsubaForXilinx()
    core.dataIn.payload := Vec(x.resized, y.resized)
    core.dataIn.valid := True
    core.dataOut.payload
  }

  val config = KaratusbaConfig(width, 34, baseMult, baseMultLatency = KaratsubaForXilinx.latency)

  "Karatsuba Multiplier" should "work" in {
    TransformTest.test(config.implH, data)
    //    VivadoSynth(config.implH)
  }
}

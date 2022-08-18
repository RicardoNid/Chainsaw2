package org.datenlord
package arithmetic

import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim.{SimConfig, _}

import scala.language.postfixOps
import scala.util.Random

class GPCTest extends AnyFlatSpec {

  behavior of "counter42"

  it should "synth" in {
    def synthCheck(width: Int): Unit = {
      val requirement = VivadoUtilRequirement(lut = width, carry8 = width / 8)
      VivadoImpl(Compressor4to2Hard(width), name = s"compressor42_${width}bits").require(requirement, 800 MHz)
    }

    (3 to 6).foreach(i => synthCheck(1 << i))
  }

  it should "work" in {
    def simCheck(width: Int): Unit = {
      SimConfig.withFstWave.compile(Compressor4to2Hard(width)).doSim { dut =>
        (0 until 1000).foreach { _ =>
          val Seq(w, x, y, z) = (0 until 4).map(_ => Random.nextBigInt(width))
          val cin = Random.nextBoolean()
          dut.cIn #= cin
          dut.w #= w
          dut.x #= x
          dut.y #= y
          dut.z #= z
          sleep(1)
          val yours = dut.sumsOut.toBigInt + (dut.carrysOut.toBigInt << 1) + (dut.cOut.toBigInt << (width))
          val golden = Seq(w, x, y, z).sum + (if (cin) 1 else 0)
          assert(yours == golden, s"\nyours :$yours\ngolden:$golden\nw=$w\nx=$x\ny=$y\nz=$z")
        }
      }
    }

    simCheck(1)
    simCheck(16)
    simCheck(17)
    simCheck(23)
  }

  behavior of "counter63"

  it should "work" in {
    def simCheck(): Unit = {
      SimConfig.withFstWave.compile(Compressor6to3Hard()).doSim { dut =>
        (0 until 1000).foreach { _ =>
          val data = Random.nextBigInt(6)
          dut.dataIn #= data
          sleep(1)
          val yours = dut.dataOut.toInt
          val golden = data.toString(2).map(_.asDigit).sum
          assert(yours == golden, s"\nyours :$yours\ngolden:$golden")
        }
      }
    }
    simCheck()
  }

  it should "synth" in VivadoSynth(Compressor6to3Hard(), "GPC63")
}

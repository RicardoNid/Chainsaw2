package org.datenlord
package device

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

class UnisimTest extends AnyFlatSpec {

  "CARR8" should "synth" in VivadoSynth(new Module {
    val a, b = in UInt (8 bits)
    val sum = out UInt (8 bits)
    val carry = CARRY8()
    carry.DI := a
    carry.S := b
    carry.CI := U(0)
    carry.CI_TOP := U(0)
    sum := carry.O
  }, "carry8")

  it should "work" in SimConfig.withWave.compile {
    new Module {
      val a, b = in UInt (8 bits)
      val sum = out UInt (8 bits)
      val carry = CARRY8()
      carry.DI := a
      carry.S := b
      carry.CI := U(0)
      carry.CI_TOP := U(0)
      sum := carry.O
    }
  }.doSim { dut =>
    dut.a #= 127
    dut.b #= 127
    sleep(1)
    println(dut.sum.toBigInt)
  }


}

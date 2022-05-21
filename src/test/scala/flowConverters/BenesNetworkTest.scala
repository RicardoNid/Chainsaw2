package org.datenlord
package flowConverters

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import intel.QuartusFlow

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BenesNetworkTest extends AnyFlatSpec {

  val data = Seq.fill(10)((0 until 8).map(BigInt(_)))
  val perms = data.map(seq => Random.shuffle(seq.toList).map(_.toInt))
  println(perms.mkString("\n"))

  val dataType = HardType(UInt(3 bits))
  val config = BenesNetworkConfig(8, perms, 5, dataType)

  "Benes network" should "work" in {
    TransformTest.test(BenesNetwork(config), data.flatten)
  }
}

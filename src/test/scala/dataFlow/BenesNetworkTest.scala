package org.datenlord
package dataFlow

import intel.QuartusFlow

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BenesNetworkTest extends AnyFlatSpec {

  val data = Seq.fill(10)((0 until 8).map(BigInt(_)))
  val perms = data.map(seq => Random.shuffle(seq.toList).map(_.toInt))
  println(perms.mkString("\n"))

  val config = BenesNetworkConfig(8, 3, perms, 5)

  "Benes network" should "work" in {
    TransformTest.bitAccurateTest(BenesNetwork(config), data.flatten)
  }
}

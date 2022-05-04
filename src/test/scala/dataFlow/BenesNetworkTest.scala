package org.datenlord
package dataFlow

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BenesNetworkTest extends AnyFlatSpec {

  val data = Seq.fill(10)((0 until 8).map(BigInt(_)))
  val perms = data.map(seq => Random.shuffle(seq.toList).map(_.toInt))
  val golden = perms.map(_.map(BigInt(_)))
  println(perms.mkString("\n"))

  val config = BenesNetworkConfig(8, 3, perms)

  "Benes network" should "work" in {

    TransformTest.testTransformModule(data.flatten, golden.flatten, BenesNetworkForPermutation(config))

  }
}

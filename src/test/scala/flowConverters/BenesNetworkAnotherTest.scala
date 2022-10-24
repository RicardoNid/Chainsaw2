package org.datenlord
package flowConverters

import flowConverters.Permutation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BenesNetworkAnotherTest extends AnyFlatSpec {

  val testCaseCount = 1000
  val N = 64
  val period = 8
  val sequence = (0 until N).toList
  val data = Seq.fill(testCaseCount)(sequence.map(BigInt(_)))
  val perms = Seq.fill(period)(Permutation(Random.shuffle(sequence)))

  val config = BenesNetworkAnother(N, perms, bitWidth = 8, 1)


  "Perm2Control algorithm" should "work" in {
    perms.foreach { permutation =>
      val control = Benes.permutation2Control(permutation)
      val ret = Benes.doBenes(sequence, control)
      assert(ret.zip(permutation.permuted).forall { case (a, b) => a == b })
    }
  }

  "Benes network" should "work" in ChainsawTest.test(config, data.flatten)
}

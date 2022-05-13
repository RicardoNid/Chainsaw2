package org.datenlord
package flowConverters

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class BenesTest extends AnyFlatSpec {

  val data = (0 until 8).toList
  val testCases = (0 until 1000).map(_ => Random.shuffle(data))

  "algorithm to get control for Benes network" should "work" in {
    testCases.foreach { permutation =>
      val control = Benes.getControlForPermutation(permutation)
      val ret = Benes.doBenes(data, control)
      assert(ret.zip(permutation).forall { case (a, b) => a == b })
    }
  }
}

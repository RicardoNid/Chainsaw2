package org.datenlord
package flowConverters

import flowConverters.Permutation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BenesTest extends AnyFlatSpec {

  val data = (0 until 8).toList
  val testCases = (0 until 1000).map(_ => Random.shuffle(data)).map(Permutation(_))


}

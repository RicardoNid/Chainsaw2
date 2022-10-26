package org.datenlord
package flowConverters

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ReverseTest extends AnyFlatSpec {

  val blockLength = 127
  val vecWidth = 4
  val bitWidth = 10

  val gen = Reverse(blockLength, vecWidth, bitWidth)
  val data = Seq.fill(vecWidth * blockLength * 7)(BigInt(bitWidth, Random))

  "reverse module" should "work" in ChainsawTest.test(
    gen = gen,
    data = data,
    testName = "testReverse"
  )

}

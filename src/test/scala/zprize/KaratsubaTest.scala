package org.datenlord
package zprize

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class KaratsubaTest extends AnyFlatSpec {

  val width = 128
  val karaGen = Karatsuba(width)
  karaGen.setVerticesAsNaive()
  karaGen.updateLatency()
  karaGen.toPng(s"kara$width")
  val data = Seq.fill(100)(BigInt(width, Random))

  behavior of "Karatsuba"

  it should "work with vertices as naive" in ChainsawTest.test(karaGen, data)
}

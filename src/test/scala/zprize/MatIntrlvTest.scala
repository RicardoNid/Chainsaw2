package org.datenlord
package zprize

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class MatIntrlvTest extends AnyFlatSpec {

  Random.setSeed(42)
  val col = 64
  val row = 32
  val width = 32
  val frameCount = 10
  val core0 = MatIntrlvCore(row, col, width)
  val core1 = MatIntrlvCore(col, row, width)
  val data = Seq.fill(row * col * frameCount)(BigInt(4, Random))

  // TODO: more testcases
  "matIntrlvCore" should "work when row > col" in ChainsawTest.test(core0, data)

  it should "work when row < col" in ChainsawTest.test(core1, data)

  it should "impl" in ChainsawImpl(core0)

  val pf = 64
  val core = MatIntrlv(row, col, width, pf)

  "matIntrlv" should "work" in ChainsawTest.test(core, data)

  it should "impl" in ChainsawImpl(core)
}

package org.datenlord
package arithmetic

import crypto.BarrettConfig
import ip.pippenger.ZPrizeMSM
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BarrettTest extends AnyFlatSpec {

  val width = 377
  val M = ZPrizeMSM.baseModulus
  val MPrime = ZPrizeMSM.MPrime

  val config = BarrettConfig(width, M, MPrime)

  val data = Seq.fill(1000)(BigInt(width, Random)).filterNot(_ >= M)
  val validData = data.take(data.length / 2 * 2)

  "Barrett" should "work" in TransformTest.test(config.implH, validData)
}

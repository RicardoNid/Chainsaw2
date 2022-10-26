package org.datenlord
package flowConverters

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FlowConvertersTest extends AnyFlatSpec {

  val intrlvS2P = S2P(10, 40, 1)
  val intrlvP2S = P2S(40, 10, 1)
  val frameData = Seq.fill(40 * 2)(BigInt(1, Random))

  "s2p" should "work" in ChainsawTest.test(intrlvS2P, frameData, testName = "testS2P")

  "p2s" should "work" in ChainsawTest.test(intrlvP2S, frameData, testName = "testP2S")

  val p2s0 = P2S(40, 10, 4, 1)
  val p2s1 = P2S(40, 10, 4, 10)
  val testData = Seq.fill(1600)(BigInt(4, Random))

  "p2sBlock" should "work" in {
    ChainsawTest.test(p2s0, testData) // register-based
    ChainsawTest.test(p2s1, testData) // RAM-based
  }
}

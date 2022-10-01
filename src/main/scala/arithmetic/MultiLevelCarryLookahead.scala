package org.datenlord
package arithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps

case class MultiLevelCarryLookahead(width: Int, base: Int)
  extends Component {
  val a, b = in Bits (width bits)
  val z = out Bits (width + 1 bits)

  def carryGen(gs: Bits, ps: Bits) = { // comment for w = 4
    val pTails = ps.asBools
      .tails.toSeq.init // (p0,p1,p2,p3), (p1,p2,p3) ...
      .map(_.reduce(_ & _)) // p0p1p2p3, p1p2p3, p2p3, p3

    val pOut = pTails.head

    val pPart = pTails.reverse.init // p3, p3p2, p3p2p1
    val gPart = gs.asBools.reverse.tail // g2, g1, g0
    val gOut = gs.msb | gPart.zip(pPart).map { case (g, p) => g & p }.reduce(_ | _)

    (gOut, pOut)
  }

  def genOnce(gs: Bits, ps: Bits) = {
    val slices = gs.subdivideIn(base bits).zip(ps.subdivideIn(base bits))
    val gpOut = slices.map { case (g, p) => carryGen(g, p) }
    (gpOut.map(_._1).asBits(), gpOut.map(_._2).asBits())
  }

  def genRec(gs: Bits, ps: Bits): (Bits, Bits) = {
    if (gs.getBitsWidth == 1) (gs, ps)
    else {
      val (gNext, pNext) = genOnce(gs, ps)
      genRec(gNext, pNext)
    }
  }

  val gs = a & b // a + b = 2
  val ps = a ^ b // a + b = 1
  val gOut = genRec(gs, ps)._1

  z := gOut ## ps
}

object MultiLevelCarryLookahead extends App {

  import scala.util.Random

  val w = 256

  SimConfig.withFstWave.compile(MultiLevelCarryLookahead(w, 4)).doSim { dut =>

    (0 until 1000).foreach { _ =>
      val testA = Random.nextBigInt(w)
      val testB = Random.nextBigInt(w)
      dut.a #= testA
      dut.b #= testB
      sleep(1)
      val ret = dut.z.toBigInt
      assert(ret >> w == (testA + testB) >> w, s"$ret != $testA + $testB")
    }
  }
}
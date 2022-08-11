package org.datenlord
package algos

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import arithmetic.MultplierMode._

import spinal.core

import scala.util.Random

class KaratsubaTest extends AnyFlatSpec {

  val testCount = 10000
  val width = 377
  val baseWidth = 32

  val data = (0 until testCount * 2).map(_ => Random.nextBigInt(width))
  val xs = data.take(testCount)
  val ys = data.takeRight(testCount)

  "Karatsuba Algo" should "work for all situation" in {
    val modes = Seq(FULL, HALFLOW, SQUARE)
    modes.foreach { mode =>
      val kara = Karatsuba(width, mode, baseWidth)
      xs.zip(ys).foreach { case (x, y) => if (mode == SQUARE) kara.mult(x, x) else kara.mult(x, y) }
    }
  }

  "3-stage Karatsuba Algo" should "work for all situation" in {
    val modes = Seq(FULL, HALFLOW, SQUARE)
    modes.foreach { mode =>
      val kara = Karatsuba(width, mode, baseWidth)
      xs.zip(ys).foreach { case (x, y) => if (mode == SQUARE) kara.multImproved(x, x) else kara.multImproved(x, y) }
    }
  }

  it should "show the cost estimated" in {
    val modes = Seq(FULL, HALFLOW, SQUARE)
    modes.foreach { mode =>
      val kara = Karatsuba(width, mode, baseWidth)
      val x = xs.head
      val y = ys.head
      if (mode == SQUARE) kara.multImproved(x, x, verbose = true) else kara.multImproved(x, y, verbose = true)
    }
  }

  "Karatsuba96" should "work" in {
    val kara = Karatsuba(width, FULL, baseWidth)
    val a = Random.nextBigInt(96)
    val b = Random.nextBigInt(96)
    val ret = kara.karatsuba96(a,b)
    assert(a * b == ret)
  }

  "Karatsuba377" should "show the cost estimated" in {
    val kara = Karatsuba(width, FULL, baseWidth)
    kara.karatsuba377(xs.head, ys.head)
  }

}

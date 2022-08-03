package org.datenlord
package algos

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import arithmetic.MultplierMode._

import scala.util.Random

class KaratsubaTest extends AnyFlatSpec {

  val testCount = 10000
  val width = 377
  val baseWidth = 32

  val data = (0 until testCount * 2).map(_ => Random.nextBigInt(width))
  val xs = data.take(testCount)
  val ys = data.takeRight(testCount)

  val multByLut = (x: UInt, y: UInt) => {
    val product = x * y
    product.addAttribute("use_dsp", "no")
    product.d(1)
  }

  "Karatsuba Algo" should "work for all situation" in {
    val modes = Seq(Full, Low, Square)
    modes.foreach{ mode =>
      val kara = Karatsuba(width, mode, baseWidth)
      xs.zip(ys).foreach { case (x, y) => if(mode == Square) kara.mult(x, x) else kara.mult(x, y)}
    }
  }

  "3-stage Karatsuba Algo" should "work for all situation" in {
    val modes = Seq(Full, Low, Square)
    modes.foreach{ mode =>
      val kara = Karatsuba(width, mode, baseWidth)
      xs.zip(ys).foreach { case (x, y) => if(mode == Square) kara.multImproved(x, x) else kara.multImproved(x, y)}
    }
  }

}

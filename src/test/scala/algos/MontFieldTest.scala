package org.datenlord
package algos

import algos.ZPrizeMSM.baseModulus

import cc.redberry.rings
import cc.redberry.rings.scaladsl._
import org.scalatest.flatspec.AnyFlatSpec

class MontFieldTest extends AnyFlatSpec {


  def testMont(): Unit = {
    implicit val baseField: MontField = MontField(baseModulus)

    def test(a: BigInt, b: BigInt): Unit = {
      val montA = baseField(a)
      val montB = baseField(b)
      val ret0 = (a * b) % baseModulus
      val ret1 = (montA * montB).toBigInt
      println(s"\ngolden: $ret0, \nyours : $ret1")
      assert(ret0 == ret1, s"\ngolden: $ret0, \nyours : $ret1")
    }

    Seq.tabulate(3, 3)((i, j) => test(baseModulus / 2 - i, baseModulus / 2 + j))
    logger.info("montgomery multiplication is correct")
  }

  "Mont Mult" should "work correctly for BLS-377 modulus" in testMont()

}

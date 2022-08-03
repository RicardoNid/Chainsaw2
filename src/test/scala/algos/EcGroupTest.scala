package org.datenlord
package algos

import cc.redberry.rings
import cc.redberry.rings.scaladsl._
import org.scalatest.flatspec.AnyFlatSpec

class EcGroupTest extends AnyFlatSpec {

  def testEcc() = {

    val p = 23
    val a = 0
    val b = 1

    implicit val ecc = EcGroup(p, a, b)
    val points = ecc.getAllPoints
    val (point0, point1, scalar) = (points(0), points(2), 15)

    val sum = ecc.padd(point0, point1)
    val sumByProj = ecc.paddByProj(point0.toProjective, point1.toProjective).toAffine
    assert(ecc.isOnCurve(sum))
    assert(sumByProj == sum)
    println(s"sum: $sumByProj = $sum")

    val dbl = ecc.pdbl(point0)
    val dblByProj = ecc.pdblByProj(point0.toProjective).toAffine
    assert(ecc.isOnCurve(dbl))
    assert(dblByProj == dbl)
    println(s"dbl: $dblByProj = $dbl")

    println(ecc.paddByProj(point0.toProjective, point0.inverse.toProjective))

    val product = ecc.pmult(scalar, point1)
    val productByProj = ecc.pmultByProj(scalar, point1.toProjective).toAffine
    assert(ecc.isOnCurve(product))
    assert(product == productByProj)
    println(s"product $product = $productByProj")
  }

  "Ecc Group" should "do add and dbl correctly with proj coordinate" in testEcc()

}

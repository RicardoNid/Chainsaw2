package org.datenlord
package algos

import cc.redberry.rings
import cc.redberry.rings.scaladsl._
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class EcGroupTest extends AnyFlatSpec {

  val p = algos.ZPrizeMSM.baseModulus
  val a = 0
  val b = 1

  implicit val ecc = EcGroup(p, a, b)
  //  val points = ecc.getAllPoints.filterNot(_ == EcPointAffine(0, 1))

  val src = Source.fromFile("/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/BLS12377POINTS")
  val lines = src.getLines().toSeq
  //  src.close()

  // get points from file
  val points: Seq[EcPointAffine] = lines.map { line =>
    val Seq(xline, yline) = line.split(" ").toSeq
    val Seq(x, y) = Seq(xline, yline).map(str => BigInt(str.drop(2), 16))
    EcPointAffine(x, y)
  }.take(10)

  val scalar = algos.ZPrizeMSM.scalarModulus

  //  val points = Seq(EcPointAffine(x, y))
  val size = points.size

  logger.info(s"test on a small EC Group with ${size} points")

  def traverse(func: (EcPointAffine, EcPointAffine) => Unit) = {
    Seq.tabulate(size, size) { (i, j) => func(points(i), points(j)) }
  }

  def testAddAffine(a: EcPointAffine, b: EcPointAffine) = {
    val sum = ecc.padd(a, b)
    assert(ecc.isOnCurve(sum), s"$a, $b, $sum")
  }

  def testAddJacobi(a: EcPointAffine, b: EcPointAffine) = {
    val sum = ecc.padd(a, b)
    val sumByProj = ecc.paddJacobi(a.toProjective, b.toProjective)
    assert(sumByProj.toAffine(jacobi = true) == sum, s"\na:$a, \nb:$b, \nproj: $sumByProj")
  }

  def testAddHomo(a: EcPointAffine, b: EcPointAffine) = {
    val sum = ecc.padd(a, b)
    val sumByProjMSM = ecc.paddHomo(a.toProjective, b.toProjective)
    assert(sumByProjMSM.toAffine(jacobi = false) == sum, s"\na:$a, \nb:$b, \nproj: $sumByProjMSM")
    //    assert(ecc.isOnCurve(sumByProjMSM.toAffine))
  }

  def testMultAffine(a: EcPointAffine) = assert(ecc.isOnCurve(ecc.pmult(scalar, a)))

  def testMultJacobi(a: EcPointAffine) = assert(ecc.pmult(scalar, a) == ecc.pmultJacobi(scalar, a.toProjective).toAffine(jacobi = true))

  def testMultHomo(a: EcPointAffine) = assert(ecc.pmult(scalar, a) == ecc.pmultHomo(scalar, a.toProjective).toAffine(jacobi = false))

  "padd and pdbl" should "work on affine coord" in traverse(testAddAffine)

  it should "work on jacobi proj coord" in traverse(testAddJacobi)

  it should "work on homo proj coord " in traverse(testAddHomo)

  "pmul" should "work on affine coord" in points.foreach(testMultAffine)

  it should "work on jacobi proj coord" in points.foreach(testMultJacobi)

  it should "work on homo proj coord" in points.foreach(testMultHomo)

}

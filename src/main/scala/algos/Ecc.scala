package org.datenlord
package algos

import cc.redberry.rings.scaladsl._

case class Point(x: Long, y: Long)

object Ecc extends App {

  val p = 23
  val a = 1
  val b = 1

  val zp = Zp64(p)
  val polyRingOnZp: UnivariateRingZp64 = UnivariateRingZp64(p, "x")
  val ec: UnivariatePolynomialZp64 = polyRingOnZp(s"x ^ 3 + $a * x + $b")

  def getSquareRoot(value: Long): Seq[Int] =
    (0 until p).filter(i => zp(i * i) == zp(value))

  def getPoints(x: Int): Seq[Point] =
    getSquareRoot(ec.evaluate(x)).map(y => Point(x, y))

  // show the points we use
  println("all points on this curve")
  println((0 until 23).flatMap(getPoints).mkString("\n"))

  def padd(p0: Point, p1: Point) = {
    val (x0, x1, y0, y1) = (p0.x, p1.x, p0.y, p1.y)
    val lambda = {
      if (p0.x == p1.x && p0.y + p1.y == p) throw new IllegalArgumentException("addition of a point a its inverse")
      if (p0 == p1) zp.divide(3 * x0 * x0 + a, 2 * y0)
      else zp.divide(y1 - y0, x1 - x0)
    }
    val x2 = zp(lambda * lambda - x0 - x1)
    val y2 = zp(lambda * (x0 - x2) - y0)
    Point(x2, y2)
  }

  def pdbl(p: Point) = padd(p, p)

  def pmultByDefinition(k: Int, p: Point) = Seq.fill(k)(p).reduce(padd)

  def pmult(k: Int, p: Point) = {
    var temp = p
    k.toBinaryString.tail.foreach { bit =>
      temp = pdbl(temp)
      if (bit == '1') temp = padd(temp, p)
    }
    temp
  }

  val (point0, point1, scalar) = (Point(3, 10), Point(9, 7), 15)
  println("result of padd")
  println(padd(point0, point1))
  println("result of pdbl")
  println(pdbl(point0))
  println("result of pmult")
  val product = pmult(scalar, point0)
  println(product)
  assert(product == pmultByDefinition(scalar, point0))

}

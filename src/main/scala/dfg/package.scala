package org.datenlord

package object dfg {

  val defaultSchedule = Schedule(Array(1), 1)

  def gcd(a: Int, b: Int): Int = {
    val (p, q) = if (a >= b) (a, b) else (b, a)
    if (q == 0) p
    else gcd(q, p % q)
  }

  def lcm(a: Int, b: Int) = a * b / gcd(a, b)

}

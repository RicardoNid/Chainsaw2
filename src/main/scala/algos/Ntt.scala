package org.datenlord
package algos

import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import rings.primes._

object Ntt {
  case class Term(coeff: Long, order: Long) // 系数-指数对,比起字符串,更加接近于仿真得到的输出(实际上,仿真得到的输出应该包含256个<3329的自然数)

  def buildString(terms: Seq[Term], symbol: String) = terms.map(term => s"${term.coeff}*$symbol^${term.order}").mkString("+") // 将系数-指数对变为可解析的表达式

  def evaluateHUAWEI(poly0: Seq[Term], poly1: Seq[Term], ret: Seq[Term]) = {
    val PolyRingOnZp = UnivariateRingZp64(3329, "x") // 构造多项式环Z_3329[X]
    val modulo = PolyRingOnZp("x^256 + 1") // 作为模数的多项式 X^256 +１

    val poly0String = buildString(poly0, "x")
    val poly1String = buildString(poly1, "x")
    val golden = PolyRingOnZp(poly0String) * PolyRingOnZp(poly1String) % modulo // 环上的多项式模乘
    val goldenTerms = (0 until 255).map(i => i -> golden.get(i)).filterNot(_._2 == 0).map{ case (order, coeff) => Term(coeff, order)} // 将结果变为terms

    println("evaluate HUAWEI")
    println(s"yours:  ${ret.mkString(" ")}")
    println(s"golden: ${goldenTerms.mkString(" ")}")
    assert(ret.diff(goldenTerms).isEmpty, s"${ret.diff(goldenTerms)}") // 进行对照
  }

  def main(args: Array[String]): Unit = {
    val thePoly1 = Seq(Term(278,245), Term(1,0))
    val thePoly2 = Seq(Term(213,399), Term(2,0))
    val result = Seq(Term(2,0), Term(2621,132), Term(3116,143), Term(556,246)) // 我们故意将最后一项设为错的
    evaluateHUAWEI(thePoly1, thePoly2, result)
  }
}


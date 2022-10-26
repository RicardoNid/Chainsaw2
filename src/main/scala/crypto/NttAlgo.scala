package org.datenlord
package crypto

import breeze.linalg._
import breeze.math._
import cc.redberry.rings.scaladsl._
import org.datenlord.dsp.DftAlgo._

object NttAlgo {

  case class Term(coeff: Long, order: Long) // 系数-指数对,比起字符串,更加接近于仿真得到的输出(实际上,仿真得到的输出应该包含256个<3329的自然数)

  case class ZpInt(value: BigInt)

  case class SemiZp(modulus: BigInt) extends Semiring[ZpInt] {
    override def zero: ZpInt = ZpInt(0)

    override def one: ZpInt = ZpInt(1)

    override def +(a: ZpInt, b: ZpInt): ZpInt = ZpInt((a.value + b.value) % modulus)

    override def *(a: ZpInt, b: ZpInt): ZpInt = ZpInt((a.value * b.value) % modulus)

    override def ==(a: ZpInt, b: ZpInt): Boolean = a.value == b.value

    override def !=(a: ZpInt, b: ZpInt): Boolean = a.value != b.value
  }

  def getNthRoot(modulus: Int, N: Int): ZpInt = { // TODO: better algorithm to support ZpInt
    import cc.redberry.rings.primes.BigPrimes
    require(BigPrimes.isPrime(modulus) && (modulus - 1) % N == 0, "p is prime & p - 1 % N <=> N-th root of exists")
    val ring = Zp(asBigInteger(modulus))
    val ret = (2 until modulus).filter { root =>
      ring.pow(root, N) == ring(1) && // N-th root
        (1 to N).map(ring.pow(root, _)).distinct.size == N
    }.head
    ZpInt(ret)
  }

  /** number theoretic transform
   *
   * @param ring the ring at which ntt is defined
   */
  def ntt(data: Array[BigInt], p: BigInt, inverse: Boolean = false): Array[BigInt] = {
    implicit val ring = SemiZp(p)
    val N = data.length
    val omega = getNthRoot(p.toInt, N)
    val zpData = DenseVector(data.map(ZpInt))
    val ret = genericDft(zpData, omega)
    ret.toArray.map(_.value)
  }

  def intt(data: Array[BigInt], p: BigInt): Array[BigInt] = ntt(data, p, inverse = true)

  def cconvByNtt(a: Array[BigInt], b: Array[BigInt], p: BigInt): Array[BigInt] = {
    implicit val ring = SemiZp(p)
    intt(ntt(a, p) *:* ntt(b, p), p)
  }

  def main(args: Array[String]): Unit = {
    val p = 3329
    val polyA = Array.fill(256)(BigInt(1))
    val polyB = Array.fill(256)(BigInt(1))
    println(ntt(polyA, p).mkString(" "))
    // polyA * polyB % x^256 - 1
    println(cconvByNtt(polyA, polyB, p).mkString(" "))
  }
}


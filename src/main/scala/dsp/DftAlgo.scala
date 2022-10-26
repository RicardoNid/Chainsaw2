package org.datenlord
package dsp

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._
import org.datenlord.arithmetic.Matrices
import org.datenlord.{logR, powR}

import scala.reflect.ClassTag

object DftAlgo {

  /** generic DFT which can be implemented in multiple different domain
   *
   * @param omega N-th root of unity in the domain
   * @param unity multiplication unity in the domain
   * @param mult  multiplication operation
   * @param add   addition operation
   * @see ''Fast Algorithms for Signal Processing'' Chap1.4
   */
  def genericDft[T](data: DenseVector[T], omega: T, inverse: Boolean = false)
                   (implicit semiring: Semiring[T], classTag: ClassTag[T]) = {
    val N = data.length
    val factors = semiring.one +: (1 to N).map(i => product(DenseVector.fill(i)(omega)))

    DenseVector.tabulate(N) { k => // vector style, which use var
      val indices = (0 until N).map(i => if (inverse) (i * k) % N else -(i * k) % N + N)
      val coeffs = DenseVector.tabulate(N)(i => factors(indices(i)))
      sum(data *:* coeffs)
    }

    // TODO: matrix style
  }

  /** generic DFT which can be implemented in multiple different domain
   *
   * @param omega N-th root of unity in the domain
   * @see ''Fast Algorithms for Signal Processing'' Chap1.4
   */
  def genericDFTMatrix[T: ClassTag](N: Int, inverse: Boolean, omega: T)(implicit field: Field[T]) = {
    val base = if (inverse) field.one / omega else omega

    def factor(exp: Int) = {
      exp match {
        case 0 => field.one
        case 1 => base
        case _ => Seq.fill(exp)(base).reduce(_ * _)
      }
    }

    DenseMatrix.tabulate(N, N)((i, j) => factor(i * j))
  }

  // for complex field
  def omega(N: Int, inverse: Boolean) = if (inverse) exp((2 * Pi / N) * i) else Complex(1, 0) / exp((2 * Pi / N) * i)

  def diagT(N: Int, n: Int, inverse: Boolean) =
    Matrices.diagonal(Seq.tabulate(N / n, n) { (i, j) =>
      Seq.fill(i * j)(omega(N, inverse)).product
    }.flatten)

  def diagC(N: Int, l: Int, radix: Int, inverse: Boolean) = {
    val pow: Int => Int = powR(radix, _)
    val t = logR(N, radix)
    val part0 = Matrices.stridePermutation[Complex](N, pow(t - l - 1))
    val T = diagT(pow(t - l), pow(t - l - 1), inverse)
    val part1 = Matrices.kronecker(T, pow(l))
    val part2 = Matrices.stridePermutation[Complex](N, pow(l + 1))
    part0 * part1 * part2
  }

  def dftMatrix(N: Int, inverse: Boolean) = genericDFTMatrix(N, inverse, omega(N, false))

  // TODO: implement inverse
  def peaseFftMatrix(N: Int, radix: Int, inverse: Boolean) = {
    val pow: Int => Int = powR(radix, _)
    val t = logR(N, radix)
    if (t == 1) dftMatrix(radix, inverse) else {

      val L = Matrices.stridePermutation[Complex](N, radix)
      val DFTs = Matrices.kronecker(dftMatrix(radix, inverse), pow(t - 1))
      def C(l: Int) = diagC(N, l, radix, inverse)
      def iterativeBox(l:Int) = L * DFTs * C(l)

      val R = Matrices.digitReversalPermutation[Complex](N, radix)
      val parts = (0 until t).map(iterativeBox)
      parts.reduce(_ * _) * R
    }
  }

  def rvdftByDouble(data0: DenseVector[Double], data1: DenseVector[Double]): (DenseVector[Complex], DenseVector[Complex]) = {
    def fftSymmetricOf(data: DenseVector[Complex]) =
      new DenseVector((data(0) +: data.toArray.tail.reverse).map(_.conjugate))
    
    val dataIn = new DenseVector(data0.toArray.zip(data1.toArray).map { case (real, imag) => Complex(real, imag) })
    val dataOut = fourierTr.dvComplex1DFFT(dataIn)
    val out0 = (dataOut + fftSymmetricOf(dataOut)) / Complex(2.0, 0.0)
    val out1 = (dataOut - fftSymmetricOf(dataOut)) / Complex(0.0, 2.0)
    assert((out0 - fourierTr.dvComplex1DFFT(data0.map(Complex(_, 0)))).forall(_.abs < 1e-4))
    assert((out1 - fourierTr.dvComplex1DFFT(data1.map(Complex(_, 0)))).forall(_.abs < 1e-4))
    (out0, out1)
  }

  def main(args: Array[String]): Unit = {
    val data = DenseVector((0 until 8).map(i => Complex(i, 0)).toArray)
    val golden = dftMatrix(8, true) * data
    val pease = peaseFftMatrix(8, 2, true) * data
    println(golden)
    println(pease)
    assert((golden - pease).forall(_.abs < 0.001))
  }

}

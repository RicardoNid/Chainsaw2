package org.datenlord
package arithmetic

import breeze.linalg.{DenseMatrix, _}
import breeze.math._
import spinal.core._
import spinal.lib._

case class AlgebraConfig[TSoft, THard <: Data]
(matrix: DenseMatrix[TSoft],
 dataType: HardType[THard], coeffType: HardType[THard],
 override val timeFold: Int)
  extends TransformBase {

  val (m, n) = (matrix.rows, matrix.cols)
  require(m % timeFold == 0)

  override def timeFolds = factors(m)

  val lMult = matrix(0, 0) match {
    case _: Complex => ComplexMult.latency
    case _ => 1
  }
  val lAdd = 1

  override def impl(dataIn: Seq[Any]) = {
    val ret = dataIn.head match {
      case _: Int =>
        val vector = new DenseVector(dataIn.asInstanceOf[Seq[Int]].toArray)
        matrix.asInstanceOf[DenseMatrix[Int]] * vector
      case _: Double =>
        val vector = new DenseVector(dataIn.asInstanceOf[Seq[Double]].toArray)
        matrix.asInstanceOf[DenseMatrix[Double]] * vector
      case _: Complex =>
        val vector = new DenseVector(dataIn.asInstanceOf[Seq[Complex]].toArray)
        matrix.asInstanceOf[DenseMatrix[Complex]] * vector
    }
    ret.toArray.toSeq
  }

  override val size = (n, m)

  val lSOP = log2Up(n) * lAdd + lMult

  override def latency = lSOP + timeFold - 1

  override def implH = Algebra(this)
}

case class Algebra[TSoft, THard <: Data]
(config: AlgebraConfig[TSoft, THard])
  extends TransformModule[THard, THard] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(dataType, n))
  override val dataOut = master Flow Fragment(Vec(dataType, m))

  // build coeffs and ROM
  val coeffHard = Util.getCoeff(coeffType, matrix.t.toArray)
  if (timeFold > 1) {
    val coeffGroups = coeffHard.grouped(m * n / timeFold).toSeq.map(Vec(_))
    val counter = autoInputCounter()
    val rom = Mem(coeffGroups)
    val currentCoeffs = rom.readAsync(counter.value)
    val afterMult = currentCoeffs.grouped(n).toSeq.map(coeff => Util.mult(dataIn.fragment, Vec(coeff), dataType))
    val afterSum = Vec(afterMult.map(Util.sum))
    val delayedControl = counter.value.d(lSOP % timeFold)
    val delayed = (0 until timeFold).flatMap(i => RegNextWhen(afterSum, delayedControl === i))
    dataOut.fragment := delayed
  } else {
    val afterMult = coeffHard.grouped(n).toSeq.map(coeff => Util.mult(dataIn.fragment, Vec(coeff), dataType))
    val afterSum = Vec(afterMult.map(Util.sum))
    dataOut.fragment := afterSum
  }

  autoValid()
  autoLast()
}
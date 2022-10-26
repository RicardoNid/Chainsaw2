package org.datenlord
package dsp

import org.datenlord.{ChainsawMetric, ChainsawTest, ComplexFixInfo, VivadoImpl}
import org.scalatest.flatspec.AnyFlatSpec
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

import scala.util.Random

class DftTest extends AnyFlatSpec {

  val data = Seq.fill(16)(Random.nextComplex())
  val invList = Seq(false, true)
  val sizeList = Seq(2,4,8)
  val dataType = ComplexFixInfo(4, 13)

  "dftAlgo" should "work on real-valued situation" in {
    val rv0 = Array.fill(256)(Random.nextDouble())
    val rv1 = Array.fill(256)(Random.nextDouble())

    DftAlgo.rvdftByDouble(DenseVector(rv0), DenseVector(rv1))
  }

  "dft" should "work for all configurations" in Seq.tabulate(2,3)((i, j) =>
    ChainsawTest.test(Dft(sizeList(j), invList(i), dataType, 16),
      data,
      metric = ChainsawMetric.complexAbs(1e-1)))

  it should "impl for all configurations" in Seq.tabulate(2,3)((i, j) =>
    VivadoImpl(Dft(sizeList(j), invList(i), dataType, 16).implH))

}

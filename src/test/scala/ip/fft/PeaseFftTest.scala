package org.datenlord
package ip.fft

import dataFlow.TransformTest

import breeze.linalg._
import breeze.math._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class PeaseFftTest extends AnyFlatSpec {

  val config = PeaseFftConfig(
    N = 16, radix = 2,
    dataWidth = 16, coeffWidth = 12,
    inverse = true, fold = 2)

  val data = Random.RandomComplexSequences(1, 32)

  def metric(yours: Seq[Complex], golden: Seq[Complex]) = {
    val yourV = new DenseVector(yours.toArray)
    val goldenV = new DenseVector(golden.toArray)
    val errorV = yourV - goldenV
    println(yourV)
    println(goldenV)
    println(errorV)
    errorV.forall(_.abs < 10e-3)
  }

  "Pease Fft" should "work" in {
    TransformTest.complexTest(PeaseFft(config), data.flatten, metric)
  }

}
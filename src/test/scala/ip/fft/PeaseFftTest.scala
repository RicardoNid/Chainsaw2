package org.datenlord
package ip.fft

import dataFlow.TransformTest

import breeze.linalg._
import breeze.math._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class PeaseFftTest extends AnyFlatSpec {

  val config = PeaseFftConfig(
    N = 8, radix = 2,
    dataWidth = 16, coeffWidth = 12,
    inverse = false, fold = 1)

  val data = Random.RandomComplexSequences(1, 16)

  def metric(yours: Seq[Complex], golden: Seq[Complex]) = {
    val yourV = new DenseVector(yours.toArray)
    val goldenV = new DenseVector(golden.toArray)
    println(yourV)
    println(goldenV)
    (yourV - goldenV).forall(_.abs < 10e-3)
  }

  "Pease Fft" should "work" in {
    TransformTest.complexTest(PeaseFft(config), data.flatten, metric)
  }

}

package org.datenlord
package ip.fft

import dataFlow.TransformTest

import breeze.linalg._
import breeze.math._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class PeaseFftTest extends AnyFlatSpec {

  val configs = (1 to 7).map { i =>
    PeaseFftConfig(
      N = 256, radix = 2,
      dataWidth = 16, coeffWidth = 12,
      inverse = true, fold = 1 << i)
  }

  val data = Random.RandomComplexSequences(1, 512).head.map(_ * Complex(0.9, 0))

  def metric(yours: Seq[Complex], golden: Seq[Complex]) = {
    val yourV = new DenseVector(yours.toArray)
    val goldenV = new DenseVector(golden.toArray)
    val errorV = yourV - goldenV
    println(yourV)
    println(goldenV)
    println(errorV)
    println(errorV.map(_.abs).toArray.max)
    errorV.forall(_.abs < 1e-2)
  }

  "Pease Fft" should "work" in {

    configs.foreach { config =>
      logger.info(s"generating pease fft on with fold = ${config.fold}")
      TransformTest.complexTest(PeaseFft(config), data, metric)
      VivadoSynth(PeaseFft(config))
    }
  }

}

package org.datenlord
package ip.fft

import dataFlow.TransformTest

import breeze.linalg._
import breeze.math._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class PeaseFftTest extends AnyFlatSpec {

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

  "Pease Fft" should "show the reuse space" in {
    Seq.tabulate(8, 4) { (space, time) =>
      PeaseFftConfig(
        N = 256, radix = 2,
        dataWidth = 16, coeffWidth = 12,
        inverse = true, spaceReuse = 1 << space, timeReuse = 1 << time)
    }
  }

  it should "work with space reuse" in {
    val spaceConfigs = (1 to 7).reverse.map { i =>
      PeaseFftConfig(
        N = 256, radix = 2,
        dataWidth = 16, coeffWidth = 12,
        inverse = true, spaceReuse = 1 << i, timeReuse = 1)
    }
    spaceConfigs.foreach { config => TransformTest.complexTest(PeaseFft(config), data, metric) }
  }

  it should "work with time reuse" in {

    val original = PeaseFftConfig(N = 64, radix = 2, dataWidth = 20, coeffWidth = 12, inverse = true, spaceReuse = 32, timeReuse = 1)
    TransformTest.complexTest(PeaseFft(original), data, metric)

    val timeConfigs = Seq(
      PeaseFftConfig(
        N = 64, radix = 2,
        dataWidth = 20, coeffWidth = 12,
        inverse = true, spaceReuse = 32, timeReuse = 6))
    timeConfigs.foreach { config => TransformTest.complexTest(PeaseFft(config), data, metric) }
  }
  //

  it should "synth for all configs" in {
    //    val configs = Seq.tabulate(8, 4) { (space, time) =>
    //      PeaseFftConfig(
    //        N = 256, radix = 2,
    //        dataWidth = 16, coeffWidth = 12,
    //        inverse = true, spaceReuse = 1 << space, timeReuse = 1 << time)
    //    }.flatten

    val configs = Seq(PeaseFftConfig(
      N = 256, radix = 2,
      dataWidth = 16, coeffWidth = 12,
      inverse = true, spaceReuse = 1 << 7, timeReuse = 1 << 3))
    configs.foreach(config => VivadoSynth(PeaseFft(config), s"Pease_FFT_256_space_${config.spaceReuse}_time_${config.timeReuse}"))
  }

}

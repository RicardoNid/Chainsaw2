package org.datenlord
package ip.fft

import dataFlow.TransformTest

import breeze.linalg._
import breeze.math._
import breeze.stats.mean
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class PeaseFftTest extends AnyFlatSpec {

  val data = Random.RandomComplexSequences(1, 512).head.map(_ * Complex(0.9, 0))

  def metric(yours: Seq[Complex], golden: Seq[Complex]) = {
    val yourV = new DenseVector(yours.toArray)
    val goldenV = new DenseVector(golden.toArray)
    val errorV = yourV - goldenV
    println(s"yours : $yourV")
    println(s"golden: $goldenV")
    println(s"error : $errorV")
    println(mean(errorV.map(_.abs)))
    println(max(errorV.map(_.abs)))
    errorV.forall(_.abs < 1e-2)
  }

  "Pease Fft" should "show the reuse space" in {
    Seq.tabulate(2, 1) { (space, time) =>
      val config = PeaseFftConfig(
        N = 64, radix = 2,
        dataWidth = 16, coeffWidth = 16,
        inverse = true, spaceReuse = 1 << (space + 2), timeReuse = 1 << time)
      TransformTest.complexTest(PeaseFft(config), data, metric, s"ofdm_fold_${1 << (space + 2)}")
    }
  }

  it should "work with space reuse" in {
    val spaceConfigs = (0 to 0).reverse.map { i =>
      PeaseFftConfig(
        N = 256, radix = 2,
        dataWidth = 20, coeffWidth = 12,
        inverse = true, spaceReuse = 128, timeReuse = 8)
    }
    spaceConfigs.foreach { config => TransformTest.complexTest(PeaseFft(config), data, metric) }
  }

  it should "work with time reuse" in {

    val timeConfigs = Seq(
      PeaseFftConfig(
        N = 256, radix = 2,
        dataWidth = 20, coeffWidth = 12,
        inverse = true, spaceReuse = 128, timeReuse = 8))
    timeConfigs.foreach { config => TransformTest.complexTest(PeaseFft(config), data, metric) }
  }
  //

  it should "synth for all configs" in {
    val configs = Seq.tabulate(8, 4) { (space, time) =>
      PeaseFftConfig(
        N = 256, radix = 2,
        dataWidth = 16, coeffWidth = 12,
        inverse = true, spaceReuse = 1 << space, timeReuse = 1 << time) // skip when spaceReuse = 1
    }.flatten

    val reports = configs.map { config => VivadoSynth(PeaseFft(config), s"Pease_FFT_256_space_${config.spaceReuse}_time_${config.timeReuse}") }
    reports.foreach(report => println(s"dsp ${report.DSP} lut ${report.LUT} ff ${report.FF} fmax ${report.Frequency}"))

  }

}

package org.datenlord
package ip.fft

import breeze.linalg._
import breeze.math._
import breeze.stats.mean
import org.datenlord.TransformTest
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class PeaseFftTest extends AnyFlatSpec {

  val data = Random.RandomComplexSequences(1, 512).head.map(_ * Complex(0.9, 0))

  "Pease Fft" should "show the reuse space" in {
    Seq.tabulate(2, 1) { (space, time) =>
      val config = PeaseFftConfig(
        N = 64, radix = 2,
        dataWidth = 16, coeffWidth = 16,
        inverse = true, spaceReuse = 1 << (space + 2), timeReuse = 1 << time)
      TransformTest.test(PeaseFft(config), data, Metric.ComplexAbs(1e-2))
    }
  }

  it should "work with space reuse" in {
    val spaceConfigs = (0 to 0).reverse.map { i =>
      PeaseFftConfig(
        N = 256, radix = 2,
        dataWidth = 20, coeffWidth = 12,
        inverse = true, spaceReuse = 128, timeReuse = 8)
    }
    spaceConfigs.foreach { config => TransformTest.test(PeaseFft(config), data, Metric.ComplexAbs(1e-2)) }
  }

  it should "work with time reuse" in {

    val timeConfigs = Seq(
      PeaseFftConfig(
        N = 256, radix = 2,
        dataWidth = 20, coeffWidth = 12,
        inverse = true, spaceReuse = 128, timeReuse = 8))
    timeConfigs.foreach { config => TransformTest.test(PeaseFft(config), data, Metric.ComplexAbs(1e-2)) }
  }
  //

  //  it should "synth for all configs for 256" in {
  //    val configs = Seq.tabulate(8, 4) { (space, time) =>
  //      PeaseFftConfig(
  //        N = 256, radix = 2,
  //        dataWidth = 16, coeffWidth = 12,
  //        inverse = true, spaceReuse = 1 << space, timeReuse = 1 << time) // skip when spaceReuse = 1
  //    }.flatten
  //
  //    val reports = configs.map { config => VivadoSynth(PeaseFft(config), s"Pease_FFT_256_space_${config.spaceReuse}_time_${config.timeReuse}") }
  //    reports.foreach(report => println(s"dsp ${report.DSP} lut ${report.LUT} ff ${report.FF} fmax ${report.Frequency}"))
  //
  //  }

  //  it should "synth for all configs for 64" in {
  //    val configs = (0 until 4).map { i =>
  //      PeaseFftConfig(
  //        N = 64, radix = 2,
  //        dataWidth = 16, coeffWidth = 12,
  //        inverse = true, spaceReuse = 1 << i, timeReuse = 1) // skip when spaceReuse = 1
  //    }
  //
  //    val reports = configs.map { config => VivadoSynth(PeaseFft(config), s"Pease_FFT_64_space_${config.spaceReuse}_time_${config.timeReuse}") }
  //    reports.foreach(report => println(s"dsp ${report.DSP} lut ${report.LUT} ff ${report.FF} fmax ${report.Frequency}"))
  //  }

}

package org.datenlord
package ip.das

import org.datenlord.intel.QuartusFlow
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

class SignalProTest extends AnyFlatSpec {

  val staticConfig = DasStaticConfig()
  val runtimeConfig = DasRuntimeConfig(10, 24.9, 5e6, 31)
  val constants = staticConfig.genConstants()
  val regValues = runtimeConfig.genRegValues(staticConfig)
  import constants._
  import regValues._

  "DAS signal processing" should "work" in {

    matlabEngine.eval("load('/home/ltr/sysudas/code/matlab/dataFromOscil/100ns_4k_1.mat');")
    //    matlabEngine.eval("dataIn = Channel_1.Data(1:2500000);") // 40 pulses
    matlabEngine.eval("dataIn = Channel_1.Data(1:1250000);") // 20 pulses
    matlabEngine.eval("dataIn = double(dataIn);")
    matlabEngine.eval("dataIn = dataIn ./ max(abs(dataIn));") // normalization
    val data = matlabEngine.getVariable[Array[Double]]("dataIn")

    val simName = "testDasSigPro"

    // TODO: get data by valid
    SimConfig.workspaceName("testDasSigPro").withFstWave.compile(SignalPro(staticConfig)).doSim { dut =>

      dut.clockDomain.forkStimulus(2)
      dut.flowIn.modeChange #= false
      dut.flowIn.valid #= false
      dut.clockDomain.waitSampling()

      val pulses = data.grouped(pulsePoints).toSeq
      val ret = Seq.fill(pulses.length)(ArrayBuffer[Double]())

      pulses.zipWithIndex.foreach { case (pulse, i) =>
        println(s"currently simulating on pulse ${i + 1}/${pulses.length}")
        dut.flowIn.pulseChange #= true
        pulse.grouped(subFilterCount).toSeq.foreach { vec =>
          dut.clockDomain.waitSampling()
          if (dut.flowOut.valid.toBoolean) ret(dut.flowOut.index.toInt) ++= dut.flowOut.payload.map(_.toDouble)
          dut.flowIn.valid #= true
          dut.flowIn.payload.zip(vec).foreach { case (port, data) => port #= data }
          dut.flowIn.index #= i
          dut.flowIn.pulseChange #= false
        }
      }

      (0 until 1000).foreach { _ =>
        dut.clockDomain.waitSampling()
        if (dut.flowOut.valid.toBoolean) ret(dut.flowOut.index.toInt) ++= dut.flowOut.payload.map(_.toDouble)
        dut.flowIn.valid #= false
      }

      val (goldenPhase, goldenIntensity) = DoDas(staticConfig, runtimeConfig, data)

      /** --------
       *
       * -------- */
      logger.info(s"size of input: ${pulses.length} * ${pulses.head.length}")
      logger.info(s"size of ret: ${ret.length} * ${ret.head.length}")
      logger.info(s"size of ret: ${goldenPhase.length} * ${goldenPhase.head.length}")

      // compare point by point
      val position = 10000 + 4
      matlabEngine.eval("figure;")
      (0 until 9).foreach { i =>
        matlabEngine.eval(s"subplot(3,3,${i + 1})")
        val yours = ret.transpose.apply(position + i - 4).toArray
        val golden = goldenPhase.transpose.apply(position + i - 4).toArray
        matlab.CompareData(yours, golden, name = s"fig_$i")
      }

      matlabEngine.eval(s"saveas(gcf, 'simWorkspace/$simName/$simName-point-by-point', 'png')")
      logger.info(s"view the figure generated: /home/ltr/IdeaProjects/Chainsaw2/simWorkspace/$simName/$simName-point-by-point.png")

      // compare pulse by pulse
      //      matlab.CompareData(ret.head.take(200), goldenPhase.head.take(200), name = s"compare")
      //      matlabEngine.eval(s"saveas(gcf, 'simWorkspace/$simName/$simName-pulse-by-pulse', 'png')")
      //      logger.info(s"view the figure generated: /home/ltr/IdeaProjects/Chainsaw2/simWorkspace/$simName/$simName-pulse-by-pulse.png")
    }
  }

  it should "synth" in new QuartusFlow(SignalPro(staticConfig)).impl()
}

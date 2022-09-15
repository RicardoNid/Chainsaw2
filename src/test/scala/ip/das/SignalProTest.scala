package org.datenlord
package ip.das

import intel.QuartusFlow

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._
import spinal.core._

import scala.collection.mutable.ArrayBuffer

class SignalProTest extends AnyFlatSpec {

  val staticConfig = DasStaticConfig()
  val runtimeConfig = DasRuntimeConfig(10.4, 24.9, 5e6, 31)
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

      /** --------
       * initialize parameters
       * -------- */
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      dut.flowIn.modeChange #= true
      dut.flowIn.pulseChange #= true
      dut.gaugePointsIn #= gaugePoints.divideAndCeil(subFilterCount)
      dut.gaugeReverseIn #= 1.0 / gaugePoints.nextMultiple(subFilterCount)
      dut.pulsePointsIn #= pulsePoints.divideAndCeil(subFilterCount)
      logger.info(s"ppi = ${pulsePoints.divideAndCeil(subFilterCount)}")
      dut.flowIn.valid #= false
      dut.clockDomain.waitSampling()

      val pulses = data.grouped(pulsePoints).toSeq
      val ret = Seq.fill(pulses.length)(ArrayBuffer[Double]())
      //      val prev = Seq.fill(pulses.length)(ArrayBuffer[Double]())
      //      val next = Seq.fill(pulses.length)(ArrayBuffer[Double]())

      var pokeStart = false

      /** --------
       * poke pulses & peek output
       * -------- */
      pulses.zipWithIndex.foreach { case (pulse, i) =>
        println(s"currently simulating on pulse ${i + 1}/${pulses.length}")
        val dataGrouped = pulse.grouped(subFilterCount).toSeq
        dataGrouped.zipWithIndex.foreach { case (vec, j) =>
          if (pokeStart && dut.flowOut.valid.toBoolean && dut.flowOut.index.toInt < ret.length) {
            ret(dut.flowOut.index.toInt) ++= dut.flowOut.payload.map(_.toDouble)
          }
          dut.flowIn.valid #= true
          dut.flowIn.payload.zip(vec).foreach { case (port, data) => port #= data }
          dut.flowIn.index #= i
          dut.flowIn.modeChange #= false // deassert after initialization
          dut.flowIn.pulseChange #= (j == dataGrouped.length - 1) // asserted before next pulse
          dut.clockDomain.waitSampling()
          if(!pokeStart) pokeStart = dut.flowOut.pulseChange.toBoolean
        }
      }

      /** --------
       * peek tailing part of the output
       * -------- */
      (0 until 1000).foreach { _ =>
        if (dut.flowOut.valid.toBoolean && dut.flowOut.index.toInt < ret.length) ret(dut.flowOut.index.toInt) ++= dut.flowOut.payload.map(_.toDouble)
        dut.flowIn.valid #= false
        dut.clockDomain.waitSampling()
      }

      val (goldenPhase, goldenIntensity) = DoDas(staticConfig, runtimeConfig, data)

      /** --------
       * show results
       * -------- */
      logger.info(s"size of input: ${pulses.length} * ${pulses.head.length}")
      logger.info(s"size of ret: ${ret.length} * ${ret.head.length}")
      logger.info(s"size of pulses in ret: ${ret.map(_.length).mkString(" ")}")
      logger.info(s"size of ret: ${goldenPhase.length} * ${goldenPhase.head.length}")

      val pointByPoint = true

      // compare point by point
      if (pointByPoint) {
        //        val position = 10000 + 4
        val position = 19785 / gaugePoints
        matlabEngine.eval("figure;")
        (0 until 9).foreach { i =>
          matlabEngine.eval(s"subplot(3,3,${i + 1})")
          val yours = ret.transpose.apply(position + i - 4).toArray
          val golden = goldenPhase.transpose.apply(position + i - 4).toArray
          matlab.CompareData(yours, golden, name = s"fig_$i")
        }

        matlabEngine.eval(s"saveas(gcf, 'simWorkspace/$simName/$simName-point-by-point', 'png')")
        logger.info(s"view the figure generated: /home/ltr/IdeaProjects/Chainsaw2/simWorkspace/$simName/$simName-point-by-point.png")
      } else { // compare pulse by pulse
        //        matlab.CompareData(ret.head.take(500), goldenPhase.head.take(500), name = s"compare")
        matlab.CompareData(ret.apply(1).take(200), goldenPhase.apply(1).take(200), name = s"compare")
        //      matlab.CompareData(prev.head.take(200), next.head.take(200), name = s"compare")
        matlabEngine.eval(s"saveas(gcf, 'simWorkspace/$simName/$simName-pulse-by-pulse', 'png')")
        logger.info(s"view the figure generated: /home/ltr/IdeaProjects/Chainsaw2/simWorkspace/$simName/$simName-pulse-by-pulse.png")
      }
    }
  }

  val phaseDiffType = HardType(SFix(3 exp, -16 exp)) // [-2, 2] for phase difference
  val phaseUnwrapType = HardType(SFix(4 exp, -16 exp))
  val phaseStoredType = HardType(SFix(4 exp, -16 exp))
  val phaseSumType = HardType(SFix(4 + log2Up(gaugePointsMax) exp, -16 exp))

  it should "synth for filterpath" in new QuartusFlow(FilterPath(staticConfig)).impl()
  //  it should "synth for diff" in new QuartusFlow(PhaseDiff(staticConfig, filterPath.cordicConfig.phaseType, phaseDiffType)).impl()
  it should "synth for unwrap0" in new QuartusFlow(PulseUnwrap(staticConfig, phaseUnwrapType, phaseStoredType)).impl()
  it should "synth for mean" in new QuartusFlow(PhaseMean(staticConfig, phaseUnwrapType, phaseSumType)).impl()
  it should "synth for unwrap1" in new QuartusFlow(FilterPath(staticConfig)).impl()
  it should "synth for full module" in new QuartusFlow(SignalPro(staticConfig)).impl()
}

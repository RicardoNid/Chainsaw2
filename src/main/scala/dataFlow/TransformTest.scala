package org.datenlord
package dataFlow

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer


object TransformTest {

  def testTransformModule[TIn <: BaseType, TOut <: BaseType](transformModule: => TransformModule[TIn, TOut], data: Seq[BigInt]): Unit = {
    SimConfig.withFstWave.compile(transformModule).doSim { dut =>

      import dut.{clockDomain, config, dataIn, dataOut}
      import config.{latency, inputFlow, outputFlow, transform}

      require(data.length % inputFlow.rawDataCount == 0, s"test data incomplete, should be a multiple of ${inputFlow.rawDataCount} while it is ${data.length}")

      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= true // refresh the inner state of dut
      clockDomain.waitSampling()

      val dataflows: Seq[(Seq[Seq[BigInt]], Seq[Boolean], Seq[Boolean])] = data.grouped(inputFlow.rawDataCount).toSeq
        .map(inputFlow.fromRawData)

      val dataFlow = dataflows.flatMap(_._1)
      val valid = dataflows.flatMap(_._2)
      val last = dataflows.flatMap(_._3)

      val dataRecord = ArrayBuffer[Seq[BigInt]]()
      val lastRecord = ArrayBuffer[Boolean]()

      dataFlow.indices.foreach { i =>
        dataIn.fragment.zip(dataFlow(i)).foreach { case (port, bigint) => port.assignBigInt(bigint) }
        dataIn.valid #= valid(i)
        dataIn.last #= last(i)
        dataRecord += dataOut.fragment.map(_.toBigInt)
        lastRecord += dataOut.last.toBoolean
        clockDomain.waitSampling()
      }

      (0 until latency + 1).foreach { _ =>
        dataRecord += dataOut.fragment.map(_.toBigInt)
        lastRecord += dataOut.last.toBoolean
        clockDomain.waitSampling()
      }

      val firstTime = lastRecord.indexOf(true) // first time when last appeared
      val lastTime = lastRecord.lastIndexOf(true) // last time when last appeared

      val yours = dataRecord.slice(firstTime + 1, lastTime + 1)
        .grouped(outputFlow.period).toSeq
        .flatMap(outputFlow.toRawData)

      val golden = transform(data)

      if (firstTime != latency) logger.warn(s"latency is ${firstTime - 1}, while supposed to be $latency")
      assert(yours == golden, s"\nyours : ${yours.mkString(" ")}\ngolden: ${golden.mkString(" ")}")
      logger.info("test for transform module passed")
    }
  }
}

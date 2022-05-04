package org.datenlord
package dataFlow

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer


object TransformTest {

  def testTransformModule[TIn <: BaseType, TOut <: BaseType](data: Seq[BigInt], golden: Seq[BigInt],
                                                             transformModule: => TransformModule[TIn, TOut]): Unit = {
    SimConfig.withFstWave.compile(transformModule).doSim { dut =>

      import dut.{clockDomain, config, dataIn, dataOut}
      require(data.length % config.inputFlow.rawDataCount == 0, s"test data incomplete, should be a multiple of ${config.inputFlow.rawDataCount} while it is ${data.length}")
      require(golden.length % config.outputFlow.rawDataCount == 0, s"golden data incomplete")

      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= true // refresh the inner state of dut
      clockDomain.waitSampling()

      val dataflows: Seq[(Seq[Seq[BigInt]], Seq[Boolean], Seq[Boolean])] = data.grouped(config.inputFlow.rawDataCount).toSeq
        .map(config.inputFlow.fromRawData)

      val dataFlow = dataflows.flatMap(_._1)
      val valid = dataflows.flatMap(_._2)
      val last = dataflows.flatMap(_._3)

      println(last.mkString(" "))

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

      (0 until config.latency + 1).foreach { _ =>
        dataRecord += dataOut.fragment.map(_.toBigInt)
        lastRecord += dataOut.last.toBoolean
        clockDomain.waitSampling()
      }

      println(dataRecord.map(_.mkString(" ")).mkString("\n"))
      val firstTime = lastRecord.indexOf(true) // first time when last appeared
      val lastTime = lastRecord.lastIndexOf(true) // last time when last appeared
      println(firstTime, lastTime)

      val yours= dataRecord.slice(firstTime + 1, lastTime + 1)
        .grouped(config.outputFlow.period).toSeq
        .flatMap(config.outputFlow.toRawData)

      assert(yours == golden, s"\nyours : ${yours.mkString(" ")}\ngolden: ${golden.mkString(" ")}")
    }
  }
}

package org.datenlord
package dataFlow

import breeze.math._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer


object TransformTest {

  private def data2Flow[T](data: Seq[T], flow: DataFlow, zero: T) = {
    val dataflows = data.grouped(flow.rawDataCount).toSeq.map(flow.fromRawData(_, zero))
    val rawData = dataflows.flatMap(_._1)
    val valid = dataflows.flatMap(_._2)
    val last = dataflows.flatMap(_._3)
    (rawData, valid, last)
  }

  def bitAccurateTest[TIn <: BaseType, TOut <: BaseType](transformModule: => TransformModule[TIn, TOut], data: Seq[BigInt]): Unit = {
    SimConfig.withFstWave.compile(transformModule).doSim { dut =>

      import dut.{clockDomain, config, dataIn, dataOut}
      import config.{inputFlow, latency, outputFlow, transform}

      require(data.length % inputFlow.rawDataCount == 0, s"test data incomplete, should be a multiple of ${inputFlow.rawDataCount} while it is ${data.length}")

      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= true // refresh the inner state of dut
      clockDomain.waitSampling()

      val dataflows: Seq[(Seq[Seq[BigInt]], Seq[Boolean], Seq[Boolean])] = data.grouped(inputFlow.rawDataCount).toSeq
        .map(inputFlow.fromRawData(_, BigInt(0)))

      val dataFlow = dataflows.flatMap(_._1)
      val valid = dataflows.flatMap(_._2)
      val last = dataflows.flatMap(_._3)

      val dataRecord = ArrayBuffer[Seq[BigInt]]()
      val lastRecord = ArrayBuffer[Boolean]()

      // peek and poke
      dataFlow.indices.foreach { i =>
        dataIn.fragment.zip(dataFlow(i)).foreach { case (port, bigint) => port.assignBigInt(bigint) }
        dataIn.valid #= valid(i)
        dataIn.last #= last(i)
        dataRecord += dataOut.fragment.map(_.toBigInt)
        lastRecord += dataOut.last.toBoolean
        clockDomain.waitSampling()
      }

      // peek only
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

      val golden = data.grouped(outputFlow.rawDataCount).toSeq.flatMap(transform)

      if (firstTime != latency) logger.warn(s"latency is ${firstTime - 1}, while supposed to be $latency")
      assert(yours == golden,
        s"\nyours:\n${yours.grouped(inputFlow.rawDataCount).toSeq.map(_.mkString(" ")).mkString("\n")}\ngolden:\n" +
          s"${golden.grouped(inputFlow.rawDataCount).toSeq.map(_.mkString(" ")).mkString("\n")}")
      logger.info("test for transform module passed")
    }
  }

  def complexTest(transformModule: => TransformModule[ComplexFix, ComplexFix], data: Seq[Complex], metric:(Seq[Complex], Seq[Complex]) => Boolean): Unit = {
    SimConfig.withFstWave.compile(transformModule).doSim { dut =>

      import dut.{clockDomain, config, dataIn, dataOut}
      import config.{inputFlow, latency, outputFlow, complexTransform}

      require(data.length % inputFlow.rawDataCount == 0, s"test data incomplete, should be a multiple of ${inputFlow.rawDataCount} while it is ${data.length}")

      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= true // refresh the inner state of dut
      clockDomain.waitSampling()

      val (rawData, valid, last) = data2Flow(data, inputFlow, Complex(0, 0))
      val dataRecord = ArrayBuffer[Seq[Complex]]()
      val lastRecord = ArrayBuffer[Boolean]()

      // peek and poke
      rawData.indices.foreach { i =>
        dataIn.fragment.zip(rawData(i)).foreach { case (port, complex) => port #= complex }
        dataIn.valid #= valid(i)
        dataIn.last #= last(i)
        dataRecord += dataOut.fragment.map(_.toComplex)
        lastRecord += dataOut.last.toBoolean
        clockDomain.waitSampling()
      }

      // peek only
      (0 until latency + 1).foreach { _ =>
        dataRecord += dataOut.fragment.map(_.toComplex)
        lastRecord += dataOut.last.toBoolean
        clockDomain.waitSampling()
      }

      val firstTime = lastRecord.indexOf(true) // first time when last appeared
      val lastTime = lastRecord.lastIndexOf(true) // last time when last appeared

      val yours = dataRecord.slice(firstTime + 1, lastTime + 1)
        .grouped(outputFlow.period).toSeq
        .map(outputFlow.toRawData)

      val golden = data.grouped(outputFlow.rawDataCount).toSeq.map(complexTransform)
      if (firstTime != latency) logger.warn(s"latency is ${firstTime - 1}, while supposed to be $latency")
      yours.zip(golden).foreach{ case (a, b) => assert(metric(a, b))}
      logger.info("test for transform module passed")
    }
  }
}

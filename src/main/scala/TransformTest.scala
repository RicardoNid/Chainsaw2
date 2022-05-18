package org.datenlord

import breeze.math.Complex
import spinal.core.sim.SimConfig
import spinal.core.{BaseType, assert}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer

object TransformTest {

  def getZero[T](data: T) = {
    val temp = data match {
      case _: BigInt => BigInt(0)
      case _: Double => 0.0
      case _: Complex => Complex(0, 0)
    }
    temp.asInstanceOf[T]
  }

  private def data2Flow[T](data: Seq[T], flow: DataFlow) = {
    val zero = getZero(data.head)
    val dataflows = data.grouped(flow.rawDataCount).toSeq.map(flow.fromRawData(_, zero))
    val rawData = dataflows.flatMap(_._1)
    val valid = dataflows.flatMap(_._2)
    val last = dataflows.flatMap(_._3)
    (rawData, valid, last)
  }

  // TODO: better method?
  def pokeWhatever[T <: Data](port: Vec[T], data: Seq[_]): Unit = {
    port.head match {
      case _: Bool => port.asInstanceOf[Vec[Bool]].zip(data).foreach { case (bool, value) => bool #= value.asInstanceOf[Boolean] }
      case _: BitVector => port.asInstanceOf[Vec[BitVector]].zip(data).foreach { case (bool, value) => bool #= value.asInstanceOf[BigInt] }
      case _: SFix => port.asInstanceOf[Vec[SFix]].zip(data).foreach { case (bool, value) => bool #= value.asInstanceOf[Double] }
      case _: ComplexFix => port.asInstanceOf[Vec[ComplexFix]].zip(data).foreach { case (bool, value) => bool #= value.asInstanceOf[Complex] }
    }
  }

  def peekWhatever[T <: Data](port: Vec[T]) = {
    port.head match {
      case _: Bool => port.asInstanceOf[Vec[Bool]].map(_.toBoolean)
      case _: BitVector => port.asInstanceOf[Vec[BitVector]].map(_.toBigInt)
      case _: SFix => port.asInstanceOf[Vec[SFix]].map(_.toDouble)
      case _: ComplexFix => port.asInstanceOf[Vec[ComplexFix]].map(_.toComplex)
    }
  }

  def showError[T](yours: Seq[T], golden: Seq[T]) = s"yours:\n${yours.mkString(" ")}\ngolden:\n${golden.mkString(" ")}"

  def test[TIn <: Data, TOut <: Data, T](transformModule: => TransformModule[TIn, TOut], data: Seq[T],
                                         metric: (Seq[T], Seq[T]) => Boolean = null, name: String = "temp") = {
    SimConfig.workspaceName(name).withFstWave.compile(transformModule).doSim { dut =>

      import dut.{clockDomain, config, dataIn, dataOut}
      import config.{impl, inputFlow, latency, outputFlow}

      require(data.length % inputFlow.rawDataCount == 0, s"test data incomplete, should be a multiple of ${inputFlow.rawDataCount} while it is ${data.length}")

      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= true // refresh the inner state of dut
      clockDomain.waitSampling()

      val zero = getZero(data.head)
      val dataflows = data.grouped(inputFlow.rawDataCount).toSeq
        .map(inputFlow.fromRawData(_, zero))

      val dataFlow = dataflows.flatMap(_._1)
      val valid = dataflows.flatMap(_._2)
      val last = dataflows.flatMap(_._3)

      val dataRecord = ArrayBuffer[Seq[T]]()
      val lastRecord = ArrayBuffer[Boolean]()

      // peek and poke
      dataFlow.indices.foreach { i =>
        pokeWhatever(dataIn.fragment, dataFlow(i))
        dataIn.valid #= valid(i)
        dataIn.last #= last(i)
        dataRecord += peekWhatever(dataOut.fragment).asInstanceOf[Seq[T]]
        lastRecord += dataOut.last.toBoolean
        clockDomain.waitSampling()
      }

      // peek only
      (0 until latency + 1).foreach { i =>
        dataRecord += peekWhatever(dataOut.fragment).asInstanceOf[Seq[T]]
        lastRecord += dataOut.last.toBoolean
        dataIn.valid #= valid(i % inputFlow.period)
        dataIn.last #= last(i % inputFlow.period)
        clockDomain.waitSampling()
      }

      val firstTime = lastRecord.indexOf(true) // first time when last appeared
      val lastTime = lastRecord.lastIndexOf(true) // last time when last appeared

      val yours = dataRecord.slice(firstTime + 1, lastTime + 1)
        .grouped(outputFlow.period).toSeq
        .map(outputFlow.toRawData)

      val golden = data.grouped(outputFlow.rawDataCount).toSeq.map(impl).map(_.asInstanceOf[Seq[T]])
      logger.warn(data.mkString(" "))
      logger.warn(golden.mkString(" "))

      if (firstTime != latency) logger.warn(s"latency is ${firstTime - 1}, while supposed to be $latency")

      logger.info(s"first pair:\n${showError(yours.head, golden.head)}")

      yours.zip(golden).foreach { case (y, g) =>
        if (metric == null) assert(y == g, showError(y, g))
        else assert(metric(y, g), showError(y, g))
      }

      logger.info("test for transform module passed")
    }
  }

  def testAllFolds[TIn <: Data, TOut <: Data, T](config: TransformConfig, data: Seq[T], metric: (Seq[T], Seq[T]) => Boolean = null, name: String = "temp") = {
    if (config.spaceFolds.length == 1 && config.timeFolds.length == 1) test[TIn, TOut, T](config.implH.asInstanceOf[TransformModule[TIn, TOut]], data, metric, name)
    else if (config.spaceFolds.length != 1) {
      config.spaceFolds.foreach { sf =>
        val configUnderTest = config.getConfigWithFoldsChanged(sf, config.timeFold)
        println(configUnderTest.spaceFold)
        test[TIn, TOut, T](configUnderTest.implH.asInstanceOf[TransformModule[TIn, TOut]], data, metric, s"${name}_sf_$sf")
      }
    } else {
      config.timeFolds.foreach(tf =>
        test[TIn, TOut, T](config.getConfigWithFoldsChanged(config.spaceFold, tf).implH.asInstanceOf[TransformModule[TIn, TOut]], data, metric, s"${name}_tf_$tf"))
    }
  }
}

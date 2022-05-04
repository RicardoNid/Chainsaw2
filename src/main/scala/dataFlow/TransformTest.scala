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

      val record = ArrayBuffer[Seq[BigInt]]()

      dataFlow.indices.foreach { i =>
        dataIn.fragment.zip(dataFlow(i)).foreach { case (port, bigint) => port.assignBigInt(bigint) }
        dataIn.valid #= valid(i)
        dataIn.last #= last(i)
        record += dataOut.fragment.map(_.toBigInt)
        clockDomain.waitSampling()
      }

      (0 until config.latency + 1).foreach { _ =>
        record += dataOut.fragment.map(_.toBigInt)
        clockDomain.waitSampling()
      }

      println(record.map(_.mkString(" ")).mkString("\n"))

    }
  }
}

package org.datenlord
package dsp

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object FilterStructure extends Enumeration {
  val DIRECT, TRANSPOSE, SYSTOLIC = Value
  type FilterStructure = Value
}

abstract class Filter extends Component {

  val coeffs: Seq[Double]
  val sizeIn: Int
  val sizeOut: Int
  val typeIn: HardType[SFix]
  val typeCoeff: HardType[SFix]
  val typeOut: HardType[SFix]

  val dataIn: Flow[Vec[SFix]]
  val dataOut: Flow[Vec[SFix]]

  def getDataIn = slave Flow Vec(typeIn(), sizeIn)

  def getDataOut = master Flow Vec(typeOut(), sizeOut)

  def taps = coeffs.length

  def coeffsHard = coeffs.map(SF(_, typeCoeff().maxExp exp, typeCoeff().minExp exp))

  def latency: Int

  def impl: Seq[Double] => Seq[Double]
}

object FilterTest {

  def test(dut: => Filter, data: Seq[Double]): Unit = {

    SimConfig.withFstWave.compile(dut).doSim { dut =>
      import dut._
      require(data.length % sizeIn == 0)

      val golden = dut.impl(data).toArray

      val simTime = dut.latency + (data.length / sizeIn)
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()

      val retBuffer = ArrayBuffer[Double]()
      val dataSegments: Seq[Seq[Double]] = data.grouped(sizeIn).toSeq

      (0 until taps).foreach { _ =>
        dut.dataIn.payload.foreach(_ #= 0.0)
        dut.clockDomain.waitSampling()
      }

      (0 until simTime).foreach { i =>
        if (i < dataSegments.length) // poke
          dut.dataIn.payload.zip(dataSegments(i))
            .foreach { case (port, value) => port #= value }
        else dut.dataIn.payload.foreach(_ #= 0.0)

        dut.clockDomain.waitSampling()

        if (i >= dut.latency) // peek
          retBuffer ++= dut.dataOut.payload.map(_.toDouble)
      }

      assert(retBuffer.length == data.length / sizeIn * sizeOut)

      val yours = retBuffer.toArray

      matlabEngine.putVariable("y", yours)
      matlabEngine.putVariable("g", golden)
      matlabEngine.eval(s"plot(y, 'b')")
      matlabEngine.eval("hold on;")
      matlabEngine.eval(s"plot(g, 'g')")
      matlabEngine.eval("legend('yours', 'golden')")
      logger.info("PRESS ANY KEY TO QUIT")
      StdIn.readLine()
    }
  }
}
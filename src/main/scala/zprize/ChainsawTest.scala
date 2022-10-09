package org.datenlord
package zprize

import breeze.math.Complex

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object ChainsawTest {

  /** --------
   * methods for numeric types conversion
   * -------- */
  def getZero[T](data: T) = {
    val temp = data match {
      case _: Bool => false
      case _: BigInt => BigInt(0)
      case _: Double => 0.0
      case _: Complex => Complex(0, 0)
    }
    temp.asInstanceOf[T]
  }

  def pokeWhatever[T <: Data](port: Vec[T], data: Seq[_]): Unit = {
    port.head match {
      case _: Bool => port.asInstanceOf[Vec[Bool]].zip(data).foreach { case (bool, value) => bool #= value.asInstanceOf[Boolean] }
      // TODO: is it compatible with signed integer?
      case _: BitVector => port.asInstanceOf[Vec[BitVector]].zip(data).foreach { case (bool, value) => bool #= value.asInstanceOf[BigInt] }
      case _: SFix => port.asInstanceOf[Vec[SFix]].zip(data).foreach { case (bool, value) => bool #= value.asInstanceOf[Double] }
      case _: ComplexFix => port.asInstanceOf[Vec[ComplexFix]].zip(data).foreach { case (bool, value) => bool #= value.asInstanceOf[Complex] }
    }
  }

  def peekWhatever[T <: Data](port: Vec[T]) = {
    port.head match {
      case _: Bool => port.asInstanceOf[Vec[Bool]].map(_.toBoolean)
      // BitVector.toBigInt works for SInt correctly
      case _: BitVector => port.asInstanceOf[Vec[BitVector]].map(_.toBigInt)
      case _: SFix => port.asInstanceOf[Vec[SFix]].map(_.toDouble)
      case _: ComplexFix => port.asInstanceOf[Vec[ComplexFix]].map(_.toComplex)
    }
  }

  /** --------
   * methods for debug/visualization
   * -------- */
  def showData[T](input: Seq[T], yours: Seq[T], golden: Seq[T], index: Int) =
    s"\n$index-th pair:\ninput:\n${input.mkString(" ")}\nyours:\n${yours.mkString(" ")}\ngolden:\n${golden.mkString(" ")}"

  def drawAndSaveData[T: ClassTag](yours: Seq[T], golden: Seq[T], name: String): Unit = {
    logger.info(s"generating the figure yours vs golden...")
    matlab.CompareData(yours, golden, name)
    matlabEngine.eval(s"saveas(gcf, 'simWorkspace/$name/$name', 'png')")
    logger.info(s"view the figure generated: /home/ltr/IdeaProjects/Chainsaw2/simWorkspace/$name/$name.png")
  }

  /** auto test for a TransformModule
   *
   * @param gen    dut generator
   * @param data   test cases
   * @param metric metric for correctness, it is a function that take two vectors, print a report and make a conclusion
   * @param testName   name of the test, will be used as the dir name in simWorkspace
   */
  def test[TSoft: ClassTag](gen: ChainsawGenerator, data: Seq[TSoft], metric: (Seq[TSoft], Seq[TSoft]) => Boolean = null, testName: String = "testTemp"): Unit = {

    import gen._

    SimConfig.workspaceName(testName).withFstWave.compile(gen.implDut).doSim { dut =>

      logger.info(
        s"\n----Chainsaw test status----" +
          s"\n\tmodules set as naive: ${naiveSet.mkString(" ")}"
      )

      import dut.{clockDomain, dataIn, dataOut}

      require(data.length % inputFormat.rawDataCount == 0, s"test data incomplete, " +
        s"should be a multiple of the input frame size ${inputFormat.rawDataCount} while it is actually ${data.length}")

      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= true // refresh the inner state of dut
      clockDomain.waitSampling()

      val zero = getZero(data.head)
      val dataflows = data.grouped(inputFormat.rawDataCount).toSeq
        .map(inputFormat.fromRawData(_, zero))

      val dataFlow = dataflows.flatMap(_._1)
      val valid = dataflows.flatMap(_._2)
      val last = dataflows.flatMap(_._3)

      val dataRecord = ArrayBuffer[Seq[TSoft]]()
      val lastRecord = ArrayBuffer[Boolean]()

      // peek and poke
      dataFlow.indices.foreach { i =>
        pokeWhatever(dataIn.fragment, dataFlow(i))
        dataIn.valid #= valid(i)
        dataIn.last #= last(i)
        dataRecord += peekWhatever(dataOut.fragment).asInstanceOf[Seq[TSoft]]
        lastRecord += dataOut.last.toBoolean
        clockDomain.waitSampling()
      }

      // peek only
      (0 until latency + 1).foreach { i =>
        dataRecord += peekWhatever(dataOut.fragment).asInstanceOf[Seq[TSoft]]
        lastRecord += dataOut.last.toBoolean
        dataIn.valid #= valid(i % inputFormat.period)
        dataIn.last #= last(i % inputFormat.period)
        clockDomain.waitSampling()
      }

      val firstTime = lastRecord.indexOf(true) // first time when last appeared
      val lastTime = lastRecord.lastIndexOf(true) // last time when last appeared

      val yours = dataRecord.slice(firstTime + 1, lastTime + 1)
        .grouped(outputFormat.period).toSeq
        .map(outputFormat.toRawData)

      if (firstTime != latency) logger.warn(s"latency is ${firstTime - 1}, while supposed to be $latency")

      // TODO: is this the same as dataFlow?
      val dataSlices: Seq[Seq[TSoft]] = data.grouped(inputFormat.rawDataCount).toSeq

      implMode match {
        case Comb => // get and compare golden & yours slice by slice
          val golden = dataSlices.map(impl).map(_.asInstanceOf[Seq[TSoft]])
          //          drawAndSaveData(yours.flatten, golden.flatten, name)
          yours.zip(golden).zipWithIndex.foreach { case ((y, g), i) =>
            val pass = if (metric == null) y == g else metric(y, g)
            assert(pass, showData(dataFlow(i), y, g, i))
          }
        case StateMachine => ???
        case Infinite => // view all slices as a input vector
          val golden = impl(data).asInstanceOf[Seq[TSoft]]
          val pass = if (metric == null) yours.flatten == golden else metric(yours.flatten, golden)
          // TODO: add assertion
          drawAndSaveData(yours.flatten, golden, testName)
        //          assert(pass, showData(data, yours.flatten, golden, 0))
      }

      logger.info(s"test for transform module passed\n${showData(dataFlow.head, yours.head, impl(dataSlices.head), 0)}")
    }
  }
}
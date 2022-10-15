package org.datenlord
package zprize

import breeze.math.Complex

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim.SimThread
import matlab._

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

  def pokeWhatever(port: Vec[Bits], data: Seq[Any], typeInfos: Seq[NumericTypeInfo]): Unit =
    port.zip(typeInfos).zip(data).foreach { case ((bits, info), v) => bits #= info.toBits(v) }

  def peekWhatever(port: Vec[Bits], typeInfos: Seq[NumericTypeInfo]) =
    port.zip(typeInfos).map { case (bits, info) => info.fromBits(bits.toBigInt) }

  /** --------
   * methods for debug/visualization
   * -------- */
  def showData[T](data: Seq[T], portSize: Int) = {
    val elementsPerCycle = 4 // TODO: this should be adjustable
    val cycles = 4 // TODO: this should be adjustable

    def showRow(data: Seq[T]) = data.take(elementsPerCycle).mkString(" ") + (if (data.length > elementsPerCycle) s" ${data.length - elementsPerCycle} more elements... " else "")

    val matrix = data.grouped(portSize).toSeq
    matrix.take(cycles).map(showRow).mkString("\n") + (if (matrix.length > 2 * cycles) s"\n${matrix.length - 2 * cycles} more cycles... \n" else "\n") + matrix.takeRight(cycles).map(showRow).mkString("\n")
  }

  def defaultMetric(yours: Seq[Any], golden: Seq[Any]): Boolean = yours.equals(golden)

  /** auto test for a TransformModule
   *
   * @param gen      dut generator
   * @param data     test vector
   * @param metric   metric for correctness, it is a function that take two vectors, print a report and make a conclusion
   * @param testName name of the test, will be used as the dir name in simWorkspace
   */
  def test[TIn: ClassTag, TOut: ClassTag](gen: ChainsawGenerator,
                                          data: Seq[TIn],
                                          golden: Seq[TOut] = null,
                                          metric: (Seq[Any], Seq[Any]) => Boolean = defaultMetric,
                                          draw: (Seq[TOut], Seq[TOut], String) => Unit = null,
                                          testName: String = "testTemp"): Unit = {

    import gen._

    logger.info(
      s"\n----starting ChainsawTest on ${gen.name}----"
    )

    require(data.length % inputFormat.rawDataCount == 0, s"testing vector length ${data.length} is not a multiple of input frame raw data count ${gen.inputFormat.rawDataCount}")
    logger.info(s"testing vector length: ${data.length}, containing ${data.length / gen.inputFormat.rawDataCount} frames")
    val zero = getZero(data.head)

    /** --------
     * preparation for simulation
     * -------- */
    // constructing frames from raw data
    val raws = data.grouped(gen.inputFormat.rawDataCount).toSeq // frames of raw data
    val all = raws.map(inputFormat.fromRawData(_, zero)) // payload, valid & last
    val dataFlow = all.flatMap(_._1)
    val valid = all.flatMap(_._2)
    val last = all.flatMap(_._3)

    val simTimeMax = (26 // forkStimulus & flushing
      + dataFlow.length // peek
      + gen.latency // wait for last valid
      + gen.outputFormat.period
      + (if (outputTimes != null) outputTimes.max else 0) // wait for latest port
      ) * 2

    // data container
    val dataRecord = ArrayBuffer[Seq[TOut]]()
    val lastRecord = ArrayBuffer[Boolean]()
    val validRecord = ArrayBuffer[Boolean]()
    val timeRecord = ArrayBuffer[Long]()

    /** --------
     * do simulation
     * -------- */
    SimConfig.workspaceName(testName).withFstWave.compile(gen.implDut).doSim { dut =>

      logger.info(
        s"\n----Chainsaw test status----" +
          s"\n\tmodules set as naive: ${naiveSet.mkString(" ")}" +
          s"\n\tdata length = ${dataFlow.length} cycles, ${dataFlow.length / gen.inputFormat.period} frames in total"
      )

      import dut.{clockDomain, dataIn, dataOut}

      // init
      def init(): Unit = {
        clockDomain.forkStimulus(2)
        dataIn.fragment.foreach(_.randomize())
        dataIn.valid #= false
        dataIn.last #= false
        clockDomain.waitSampling(9) // flushing
        dataIn.last #= true // refresh the inner state of dut
        clockDomain.waitSampling(1)
      }

      def poke(): SimThread = fork {
        var i = 0
        while (true) {
          if (i < dataFlow.length) {
            pokeWhatever(dataIn.fragment, dataFlow(i), gen.inputTypes)
            dataIn.valid #= valid(i)
            dataIn.last #= last(i)
          } else {
            pokeWhatever(dataIn.fragment, dataIn.fragment.map(_ => zero), gen.inputTypes)
            dataIn.valid #= false
            dataIn.last #= false
          }
          i += 1
          clockDomain.waitSampling()
        }
      }

      def peek(): SimThread = fork {
        while (true) {
          dataRecord += peekWhatever(dataOut.fragment, gen.outputTypes).asInstanceOf[Seq[TOut]]
          lastRecord += dataOut.last.toBoolean
          validRecord += dataOut.valid.toBoolean
          timeRecord += simTime()
          clockDomain.waitSampling()
        }
      }

      def waitSimDone() = {
        do {
          clockDomain.waitSampling(10)
        } while (simTime() <= simTimeMax)
      }

      init()
      // TODO: will peek run before poke?
      peek()
      poke()
      waitSimDone() // working on the main thread, pushing the simulation forward
    }

    /** --------
     * analysis after simulation
     * -------- */
    val firstTime = validRecord.indexOf(true) - gen.outputFormat.firstValid

    logger.info(s"frames starts at $firstTime, that is," +
      s"simTime ${timeRecord(firstTime)}, ")

    val yours: Seq[Seq[TOut]] = dataRecord.slice(firstTime, firstTime + dataFlow.length) // range of interest
      .grouped(outputFormat.period).toSeq // frames
      .map(outputFormat.toRawData) // raw data

    def showAllData[T](input: Seq[T], yours: Seq[T], golden: Seq[T], index: Int) =
      s"\n$index-th frame:" +
        s"\ninput :\n${showData(input, gen.sizeIn)} " +
        s"\nyours :\n${showData(yours, gen.sizeOut)} " +
        s"\ngolden:\n${showData(golden, gen.sizeOut)}"

    val goldenInUsed: Seq[Seq[TOut]] =
      if (golden == null) raws.map(impl).map(_.asInstanceOf[Seq[TOut]])
      else golden.grouped(outputFormat.rawDataCount).toSeq

    implMode match {
      case Comb => // get and compare golden & yours slice by slice
        // compare yours with the golden frame by frame
        if (draw != null) draw(yours.flatten, goldenInUsed.flatten, testName)
        yours.zip(goldenInUsed).zipWithIndex.foreach { case ((y, g), i) =>
          assert(metric(y, g), showAllData(raws(i), y, g, i))
        }
      case StateMachine => ???
      case Infinite => ???
    }

    logger.info(s"test for generator ${gen.name} passed\n${showAllData(raws.head, yours.head, goldenInUsed.head, 0)}")
  }
}
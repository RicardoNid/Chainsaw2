package org.datenlord
import breeze.math.Complex
import org.datenlord.{Comb, Infinite, Metric, StateMachine, logger, naiveSet}
import spinal.core._
import spinal.core.sim._
import spinal.sim.SimThread

import scala.collection.mutable.ArrayBuffer

case class TestReport(passed: Boolean, golden: Seq[Any])

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

  // TODO: type parameter Any and Any are redundant for test, remove them

  /** auto test for a TransformModule
   *
   * @param gen      dut generator
   * @param data     test vector
   * @param metric   metric for correctness, it is a function that take two vectors, print a report and make a conclusion
   * @param testName name of the test, will be used as the dir name in simWorkspace
   */
  def test(gen: ChainsawGenerator,
           data: Seq[Any],
           golden: Seq[Any] = null,
           metric: (Seq[Any], Seq[Any]) => Boolean = defaultMetric,
           draw: (Seq[Any], Seq[Any], String) => Unit = null,
           silentTest: Boolean = false,
           testName: String = "testTemp"): TestReport = {

    import gen._

    logger.info(
      s"\n----starting ChainsawTest on ${gen.name}----"
    )

    require(data.length % inputFormat.rawDataCount == 0, s"testing vector length ${data.length} is not a multiple of input frame raw data count ${gen.inputFormat.rawDataCount}")
    if (golden != null) require(golden.length % outputFormat.rawDataCount == 0, s"golden vector length ${golden.length} is not a multiple of output frame raw data count ${gen.outputFormat.rawDataCount}")
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
      + actualOutTimes.max // wait for latest port
      ) * 2

    // data container
    val dataRecord = ArrayBuffer[Seq[Any]]()
    val lastRecord = ArrayBuffer[Boolean]()
    val validRecord = ArrayBuffer[Boolean]()
    val timeRecord = ArrayBuffer[Long]()

    /** --------
     * do simulation
     * -------- */
    SimConfig.workspaceName(testName).withFstWave.compile(gen.implDut).doSim { dut =>

      logger.info(
        s"\n----Chainsaw test status----" +
          s"\n\tmodules set as naive: \n${naiveSet.mkString("\n\t")}" +
          s"\n\tdata length = ${dataFlow.length} cycles, ${dataFlow.length / gen.inputFormat.period} frames in total"
      )

      import dut.{clockDomain, dataIn, dataOut}

      // init
      def init(): Unit = {
        dataIn.valid #= false
        dataIn.last #= false
        clockDomain.forkStimulus(2)
        dataIn.fragment.foreach(_.randomize())
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
          dataRecord += peekWhatever(dataOut.fragment, gen.outputTypes).asInstanceOf[Seq[Any]]
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
    // TODO： accurate first time
    // FIXME: double sample problem when latency = 1, this may or may not happen, confusing!
    val firstTime = if (gen.latency == 1) lastRecord.indexOf(true) + 2 else lastRecord.indexOf(true) + 1
    //    val firstTime = lastRecord.indexOf(true) + 1

    logger.info(s"frames starts at $firstTime, that is," +
      s"simTime ${timeRecord(firstTime)}, ")

    val yours: Seq[Seq[Any]] = dataRecord.slice(firstTime, firstTime + dataFlow.length) // range of interest
      .grouped(outputFormat.period).toSeq // frames
      .map(outputFormat.toRawData) // raw data

    def showAllData[T](input: Seq[T], yours: Seq[T], golden: Seq[T], index: Int) =
      s"\n$index-th frame:" +
        s"\ninput :\n${showData(input, gen.sizeIn)} " +
        s"\nyours :\n${showData(yours, gen.sizeOut)} " +
        s"\ngolden:\n${showData(golden, gen.sizeOut)}"

    val goldenInUsed: Seq[Seq[Any]] =
      if (golden == null) raws.map(impl)
      else golden.grouped(outputFormat.rawDataCount).toSeq

    val success = implMode match {
      case Comb => // get and compare golden & yours slice by slice
        // compare yours with the golden frame by frame
        if (draw != null) draw(yours.flatten, goldenInUsed.flatten, testName)
        val remained = yours.zip(goldenInUsed).zipWithIndex.dropWhile { case ((y, g), _) => metric(y, g) }
        if (remained.nonEmpty && !silentTest) {
          val ((y, g), i) = remained.head
          logger.info(
            s"\n----error frame report----" +
              s"\n${showAllData(raws(i), y, g, i)}"
          )
        }
        remained.isEmpty
      case StateMachine => ???
      case Infinite => ???
    }


    if (!silentTest) {
      if (success) logger.info(s"test for generator ${gen.name} passed\n${showAllData(raws.last, yours.last, goldenInUsed.last, raws.length)}")
      else logger.error(s"test for generator ${gen.name} failed")
      assert(success)
    }

    TestReport(success, goldenInUsed.flatten)
  }

  def testChain(gens: Seq[ChainsawGenerator],
                data: Seq[Any],
                metrics: Seq[Metric],
                testName: String = "testTemp"
               ): Unit = {

    def testOnce(chain: Seq[ChainsawGenerator], metric: Metric) = {
      val name = s"${testName}_minus_${gens.length - chain.length}"
      val report = test(chain.reduce(_ + _), data, metric = metric, silentTest = true, testName = name)
      if (report.passed) logger.info(s"test $name passed")
      else logger.error(s"test $name failed")
      report
    }

    val dutChains = gens.inits.toSeq.init
    var finalGolden = Seq[Any]()
    val failedChains = dutChains.takeWhile { chain =>
      val report = testOnce(chain, metrics(chain.length - 1))
      finalGolden = report.golden
      !report.passed // keep going when failed
    }
    if (failedChains.nonEmpty) {
      val id = gens.length - failedChains.length - 1
      val problem = failedChains.last.last
      logger.warn(s"found problematic generator: the $id-th generator ${problem.name}")
      logger.info(s"test on problematic generator...")
      val report = test(problem, finalGolden, metric = metrics(id), silentTest = true, testName = s"test_${problem.name}")
      if (report.passed) logger.info("test on problematic generator passed, Chainsaw connection problem")
      else logger.error(s"test on problematic generator failed")
    }

    assert(failedChains.isEmpty)
  }
}
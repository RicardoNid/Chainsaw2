package org.datenlord

import org.datenlord.util.{Waveform, WaveformGraph}

import scala.collection.mutable.ArrayBuffer

/**
 * @param flow a 2-dimensional matrix in which each row is a cycle, a each column is a port
 */
case class FrameFormat(flow: Seq[Seq[Int]]) {

  /** --------
   * properties
   * -------- */
  def period: Int = flow.length

  def portSize: Int = flow.head.length

  def rawData = flow.flatten.filter(_ >= 0)

  def rawDataCount = rawData.distinct.length

  def isUnique = rawData.length == rawData.distinct.length

  def isCompact = !flow.flatten.contains(-1)

  /** --------
   * query
   * -------- */
  def getTime(elem: Int) = flow.flatten.indexWhere(_ == elem) / portSize

  def getPort(elem: Int) = flow.flatten.indexWhere(_ == elem) % portSize

  def validCycles = flow.zipWithIndex.filterNot(_._1.forall(_ == -1)).map(_._2)

  def bubbleCycles = flow.zipWithIndex.filter(_._1.forall(_ == -1)).map(_._2)

  def firstValid = validCycles.head

  /** --------
   * methods for readability & visualization
   * -------- */
  override def toString = {
    val charChange = (index: Int) => if (index < 0) " " else "■"
    s"data flow: portSize=$portSize, period=$period, size=${portSize * period}, raw=$rawDataCount\n${
      flow.zipWithIndex.map { case (seq, i) =>
        s"c$i".padTo(5, ' ') + seq.map(charChange(_)).mkString("")
      }.mkString("\n")
    }"
  }

  /** generate the waveform figure as json file, which can be rendered by VS Code plugin "Waveform Render"
   *
   * @param name   name of the json file
   * @param symbol symbol used for elements in waveform file, x -> x_0, x_1...
   */
  def generateWaveform(name: String, symbol: String = "x"): Unit = {

    def toWave(index: Int) = if (index < 0) "x" else "="

    def toData(index: Int) = symbol + index.toString

    def toDataPrime(index: Int) = symbol + "\'" + index.toString

    def addPrePost(wave: String) = "d" + wave + "d"

    val waves: Seq[String] = flow.transpose.map(seq => seq.map(toWave).mkString(""))
    val data: Seq[Seq[String]] = flow.transpose.map(seq => seq.filter(_ > -1).map(toData) ++ seq.filter(_ > -1).map(toDataPrime))

    val waveforms = waves.zip(data).zipWithIndex.map { case ((wave, data), i) => Waveform(s"port$i", addPrePost(wave.repeat(2)), data) }
    val valid = Waveform("valid", addPrePost(flow.map(seq => if (seq.forall(_ == -1)) "0" else "1").mkString("").repeat(2)), Seq())
    val last = Waveform("last", addPrePost(("0" * (period - 1) + "1").repeat(2)), Seq())

    WaveformGraph(name, waveforms :+ last :+ valid).generateJsonFile()
  }

  /** --------
   * simulation utils
   * -------- */
  def fromRawData[T](seq: Seq[T], zero: T) = {
    val data = flow.map(_.map(index => if (index >= 0) seq(index) else zero)) //
    val valid = flow.map(_.exists(_ >= 0))
    val last = Seq.fill(period - 1)(false) :+ true
    (data, valid, last)
  }

  def toRawData[T](dataFlow: Seq[Seq[T]]) = {
    require(dataFlow.length == period && dataFlow.head.length == portSize,
      s"period should be $period while it is ${dataFlow.length}, portWidth should be $portSize while it is ${dataFlow.head.length}")
    val rawDataTemp = Seq.fill(rawDataCount)(ArrayBuffer[T]())
    flow.zip(dataFlow).foreach { case (flowRow, dataRow) =>
      flowRow.zip(dataRow).foreach { case (index, data) =>
        if (index >= 0) rawDataTemp(index) += data
      }
    }
    require(rawDataTemp.forall(valuesForSameIndex => valuesForSameIndex.distinct.length == valuesForSameIndex.length))
    rawDataTemp.map(_.head)
  }

  /** --------
   * format algebra
   * -------- */
  def getBubble: Seq[Int] = flow.head.map(_ => -1)

  def interpolate(multiple: Int): FrameFormat = {
    val interpolated = flow.flatMap(row => row +: Seq.fill(multiple - 1)(getBubble))
    FrameFormat(interpolated)
  }

  def pad(cycle: Int) = {
    val padded = flow ++ Seq.fill(cycle)(getBubble)
    FrameFormat(padded)
  }

  def repeat(multiple: Int): FrameFormat = {
    def next(current: Seq[Int]): Seq[Int] = current.map(i => if (i < 0) -1 else i + rawDataCount)

    val repeated = Seq.iterate(flow.flatten, multiple)(next).flatten
    FrameFormat(repeated.grouped(portSize).toSeq)
  }

}

object FrameFormat {
  def apply(flow: Seq[Int], portSize: Int): FrameFormat = FrameFormat(flow.grouped(portSize).toSeq)

}

object MatrixFormat {
  def apply(streamWidth: Int, period: Int) = {
    val flow = (0 until streamWidth * period).grouped(streamWidth).toSeq
    FrameFormat(flow)
  }
}

object MatrixFormatAddBubble {
  def apply(streamWidth: Int, valid: Int, bubble: Int) = {
    val flow = (0 until streamWidth * valid).grouped(streamWidth).toSeq ++ Seq.fill(bubble)(Seq.fill(streamWidth)(-1))
    FrameFormat(flow)
  }
}

object ShowFrameFormat extends App {

  val seq0 = (-1 +: (0 until 224)) ++ Seq.fill(256 - 225)(-1)
  val format0 = FrameFormat(seq0, 64)
  println(format0)
  println(format0.repeat(4))

}
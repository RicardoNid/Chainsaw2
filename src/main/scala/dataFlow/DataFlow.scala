package org.datenlord
package dataFlow

import util._

import scala.collection.mutable.ArrayBuffer

abstract class DataFlow() {
  val flow: Seq[Seq[Int]]

  def period = flow.length

  def portWidth = flow.head.length

  def rawData = flow.flatten.filter(_ >= 0)

  def rawDataCount = rawData.distinct.length

  def isUnique = rawData.length == rawData.distinct.length

  def getTime(elem: Int) = flow.flatten.indexWhere(_ == elem) / portWidth

  def getPort(elem: Int) = flow.flatten.indexWhere(_ == elem) % portWidth

  def validCycles = flow.zipWithIndex.filter(!_._1.forall(_ == -1)).map(_._2)

  override def toString = {
    val charChange = (index: Int) => if (index < 0) "-" else index.toString
    s"data flow: \n${flow.transpose.map(_.map(charChange(_).padTo(2, ' ')).mkString(" ")).mkString("\n")}"
  }

  /** generate the waveform figure as json file, which can be rendered by VS Code plugin "Waveform Render"
   *
   * @param name   name of the json file
   * @param symbol symbol used for elements in waveform file, x -> x_0, x_1...
   */
  def generateWaveform(name: String, symbol: String): Unit = {

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

  def fromRawData(seq: Seq[BigInt]) = {
    val data = flow.map(_.map(index => if (index >= 0) seq(index) else BigInt(0)))
    val valid = flow.map(_.exists(_ >= 0))
    val last = Seq.fill(period - 1)(false) :+ true
    (data, valid, last)
  }

  def toRawData(dataFlow: Seq[Seq[BigInt]]) = {
    require(dataFlow.length == period && dataFlow.head.length == portWidth)
    val rawDataTemp = Seq.fill(rawDataCount)(ArrayBuffer[BigInt]())
    flow.zip(dataFlow).foreach { case (flowRow, dataRow) =>
      flowRow.zip(dataRow).foreach { case (index, data) =>
        if (index >= 0) rawDataTemp(index) += data
      }
    }
    require(rawDataTemp.forall(valuesForSameIndex => valuesForSameIndex.distinct.length == valuesForSameIndex.length))
    rawDataTemp.map(_.head)
  }
}

object DataFlow { // examples
  def main(args: Array[String]): Unit = {
    val flow = Seq(Seq(0, 1, -1), Seq(2, 3, -1))
    val dataflow = BasicDataFlow(flow)
    dataflow.generateWaveform("example", "x")
    val rawData = Seq(1, 5, 9, 3).map(BigInt(_))
    val theFlow = dataflow.fromRawData(rawData)._1
    println(theFlow.map(_.mkString(" ")).mkString("\n"))
    val theRaw = dataflow.toRawData(theFlow)
    println(theRaw.mkString(" "))
  }
}

case class BasicDataFlow(override val flow: Seq[Seq[Int]]) extends DataFlow

case class FullyPipelinedFlow(thePortWidth: Int) extends DataFlow {
  override val flow = Seq(0 until thePortWidth)
}

case class CyclicFlow(thePortWidth: Int, thePeriod: Int) extends DataFlow {
  override val flow = (0 until thePortWidth * thePeriod).grouped(thePortWidth).toSeq
}

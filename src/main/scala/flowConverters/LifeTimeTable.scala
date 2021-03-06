package org.datenlord
package flowConverters

import scala.math.ceil

case class LifeTimeTable(tIns: Seq[Int], tZlOuts: Seq[Int]) {

  val rawDataCount = tIns.length

  val period = tIns.max max tZlOuts.max

  def tDiff = tZlOuts.zip(tIns).map { case (a, b) => a - b }

  def latency = tDiff.min.abs

  def tOuts = tZlOuts.map(_ + latency)

  def lifeLengths = tOuts.zip(tIns).map { case (a, b) => a - b }

  def positive(value: Double) = if (value > 0) value else 0

  def dataAlive(time: Int) =
    tIns.map(tIn => ceil(positive(time - tIn.toDouble) / period)).sum.toInt -
      tOuts.map(tOut => ceil(positive(time - tOut.toDouble) / period)).sum.toInt

  def minimizedRegisterCount = (latency until latency + period).map(dataAlive).max

  def toKaTex = {
    val head = "\\begin{array}{crrrrrc}"
    val last = "\\hline\\end{array}"
    val header = "\\hline \\text { Variable }(v) & T_{\\text {input }} & T_{\\text {zlout }} & T_{\\text {diff }} & T_{\\text {out }} & L(v) & \\text { Life Period } \\\\ \\hline"
    val contents = (0 until rawDataCount).map(index => s"\\text { $index } & ${tIns(index)} & ${tZlOuts(index)} & ${tDiff(index)} & ${tOuts(index)} & ${tOuts(index) - tIns(index)} & ${tIns(index)} \\rightarrow ${tOuts(index)} \\\\").mkString("\n")
    Seq(head, header, contents, last).mkString(" ")
  }

  override def toString = toKaTex
}

package org.datenlord
package zprize

case class FrameFormat(flowIn: Seq[Seq[Int]], flowOut: Seq[Seq[Int]]) {
  val inputFormat = BasicDataFlow(flowIn)
  val outputFormat = BasicDataFlow(flowOut)

  def needNoControl = inputFormat.period == 1 && outputFormat.period == 1
}

object FrameNoControl {
  def apply(sizeIn: Int, sizeOut: Int) = FrameFormat(Seq(0 until sizeIn), Seq(0 until sizeOut))
}

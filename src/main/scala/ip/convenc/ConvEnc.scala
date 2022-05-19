package org.datenlord
package ip.convenc

import spinal.core._
import spinal.lib._


case class ConvEncConfig(codeGen: Seq[Seq[String]], codeGenRadix: Int = 8)
  extends TransformBase {

  val binaryCodeGen = codeGen.map(_.map(BigInt(_, codeGenRadix).toString(2)))
  val constraintLength = binaryCodeGen.flatten.map(_.length).max
  val constraintLengths = binaryCodeGen.map(_.map(_.length).max)

  val inputStep = binaryCodeGen.length
  val codeRate = inputStep.toDouble / outputWidth

  override val size = (constraintLength, outputWidth)

  override def latency = constraintLength - 1

  override def implH = ConvEnc(this)

  // TODO: reference model

  override def impl(dataIn: Seq[Any]) = null
}

case class ConvEnc(config: ConvEncConfig) extends TransformModule[Bool, Bool] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(Bool(), inputWidth))
  override val dataOut = master Flow Fragment(Vec(Bool(), outputWidth))

  val delayLines = (0 until inputStep).map(i =>
    dataIn.fragment.zipWithIndex.filter(_._2 % inputStep == i).map(_._1))

  val ret = binaryCodeGen.transpose.flatMap {
    stringsForSameOutput =>
      delayLines.zip(stringsForSameOutput).map {
        case (line, string) =>
          line.zip(string).filter(_._2 == '1').map(_._1).reduce(_ ^ _)
      }
  }

  dataOut.fragment.zip(ret).foreach { case (port, data) => port := data }
  autoValid()
  autoLast()
}
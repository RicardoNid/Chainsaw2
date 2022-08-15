package org.datenlord
package flopoco

import xilinx.XilinxDeviceFamily._

import spinal.core._
import spinal.lib._

import scala.io.Source
import scala.language.postfixOps

abstract class Flopoco extends TransformBase {

  // params required for all operators, fmax and device family can be globally changed for your project by change xilinx.defaultDevice
  val operatorName: String
  val frequency = xilinx.defaultDevice.fmax
  val family = xilinx.defaultDevice.family

  // parameters for specific operator, this field should be implemented by concrete operators
  val params: Seq[(String, Any)]

  val flopocoPath = "/home/ltr/flopoco/build/flopoco" // TODO: MAKE THIS ADJUSTABLE
  val outputDir = s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/flopocoGenerated" // TODO: MAKE THIS RELATIVE

  /** output RTL file name defined by all params, making cache file possible
   */
  def rtlPath = {
    val paramsInName = (params :+ ("target", family)).map { case (param, value) => s"${param}_$value" }.mkString("_")
    val filename = s"${operatorName}_$paramsInName.vhd"
    s"$outputDir/$filename"
  }

  /** invoke flopoco to generate RTL and get terminal output
   */
  def flopocoRun() = {

    val familyLine = family match {
      case UltraScale => "VirtexUltrascalePlus"
      case Series7 => "Kintex7"
    }
    val paramsLine = params.map { case (param, value) => s"$param=$value" }.mkString(" ")

    // TODO: figure out whether cplex will be useful or not
    val command = s"$flopocoPath frequency=${frequency.toInt / 1e6} target=$familyLine verbose=1 outputFile=$rtlPath ilpSolver=cplex $operatorName $paramsLine"
    doCmdAndGetLines(command, outputDir)
  }

  /** extract the module name as well as the pipeline level from generated RTL
   */
  def getInfoFromRtl = {
    val src = Source.fromFile(rtlPath)
    val lines = src.getLines().toSeq
    val lineIndex = lines.indexWhere(_.contains(s"${operatorName}_"))
    val linesForSearch = lines.drop(lineIndex)
    val latency = linesForSearch
      .filter(_.startsWith("-- Pipeline depth: ")).head
      .filter(_.isDigit).mkString("").toInt
    val defName = linesForSearch
      .filter(_.startsWith(s"entity ${operatorName}_")).head
      .split(" ")(1)
    src.close()
    (latency, defName)
  }

  override def latency = {
    if (!new java.io.File(rtlPath).exists()) flopocoRun()
    val latency = getInfoFromRtl._1
    logger.info(s"flopoco latency = $latency")
    latency
  }

  def moduleName = {
    if (!new java.io.File(rtlPath).exists()) flopocoRun()
    val moduleName = getInfoFromRtl._2
    logger.info(s"flopoco module name = $moduleName")
    moduleName
  }

  val widthsIn: Seq[Int]

  /** black box used in synthesis
   */
  def blackbox: FlopocoBlackBox

  /** rtl model used in simulation
   */
  def model(dataIn: Seq[Bits]): Seq[Bits]

  override def implH = {
    val theConfig = this
    new TransformModule[Bits, Bits] {
      override val config = theConfig

      override val dataIn = slave Flow Fragment(Vec(widthsIn.map(width => Bits(width bits))))
      override val dataOut = master Flow Fragment(Vec(Bits(), size._2))
      val ret =
        if (!useFlopoco)
          model(dataIn.fragment.map(_.d(latency)))
        else {
          val box = blackbox
          box.addRTLPath(rtlPath)
          box.definitionName = moduleName
          box.asNode(dataIn.fragment)
        }
      dataOut.fragment := Vec(ret)
      autoValid()
      autoLast()
    }
  }
}

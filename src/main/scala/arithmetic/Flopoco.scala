package org.datenlord
package arithmetic

import xilinx.XilinxDeviceFamily._

import spinal.core._
import spinal.lib._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Random

abstract class Flopoco extends TransformBase {

  val operatorName: String
  val params: Seq[(String, Any)]
  val frequency: HertzNumber
  val family: XilinxDeviceFamily

  val flopocoPath = "/home/ltr/flopoco/build/flopoco" // TODO: MAKE THIS ADJUSTABLE
  val outputDir = s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/flopocoGenerated" // TODO: MAKE THIS RELATIVE

  def rtlPath = {
    val paramsInName = (params :+ ("target", family)).map { case (param, value) => s"${param}_$value" }.mkString("_")
    val filename = s"${operatorName}_$paramsInName.vhd"
    s"$outputDir/$filename"
  }

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

  def blackbox: FlopocoBlackBox

  def model(dataIn: Seq[UInt]): Seq[UInt]

  override def implH = {
    val theConfig = this
    new TransformModule[UInt, UInt] {
      override val config = theConfig

      override val dataIn = slave Flow Fragment(Vec(widthsIn.map(width => UInt(width bits))))
      override val dataOut = master Flow Fragment(Vec(UInt(), size._2))
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

abstract class FlopocoBlackBox() extends BlackBox {

  val clk = in Bool()
  mapCurrentClockDomain(clk)

  val config: Flopoco

  def asNode: Seq[UInt] => Seq[UInt]
}

case class IntMultiAdderConfig(widthIn: Int, n: Int) extends Flopoco {
  override val operatorName = "IntMultiAdder"
  override val params = Seq(("signedIn", 0), ("n", n), ("wIn", widthIn))
  override val frequency = 800 MHz
  override val family = UltraScale
  override val widthsIn = Seq.fill(n)(widthIn)

  override def blackbox = IntMultiAdder(this)

  override def model(dataIn: Seq[UInt]) = Seq(dataIn.reduceBalancedTree(_ +^ _))

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)

  override val size = (n, 1)
}

case class IntMultiAdder(config: IntMultiAdderConfig) extends FlopocoBlackBox {

  import config._

  val X = in Vec(UInt(widthIn bits), n)
  X.zipWithIndex.foreach { case (int, i) => int.setName(s"X$i") }
  val widthOut = widthsIn.head + log2Up(n)
  val R = out UInt (widthOut bits)

  override def asNode = (dataIn: Seq[UInt]) => {
    val core = this
    core.X := Vec(dataIn)
    Seq(core.R)
  }
}

object IntMultiAdder {
  def main(args: Array[String]): Unit = {
    //    val config = IntMultiAdderConfig(100, 20)
    val config = IntMultiplierConfig(377, 377, 162)
    val data = (0 until 20).map(_ => Random.nextBigInt(96))
    TransformTest.test(config.implH, data)
    //    VivadoSynth(config.implH, "testCompressor")
    //    VivadoSynth(IntMultiAdderConfig(100, 20).implH, "testCompressor")
  }
}

case class IntMultiplierConfig(widthX: Int, widthY: Int, maxDSP: Int) extends Flopoco {
  override val operatorName = "IntMultiplier"
  override val params = Seq(("wX", widthX), ("wY", widthY), ("maxDSP", maxDSP), ("useKaratsuba", 1))
  override val frequency = 800 MHz
  override val family = UltraScale
  override val widthsIn = Seq(widthX, widthY)

  override def blackbox = IntMultiplier(this)

  override def model(dataIn: Seq[UInt]) = Seq(dataIn.reduce(_ * _))

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].product)

  override val size = (2, 1)
}

case class IntMultiplier(config: IntMultiplierConfig) extends FlopocoBlackBox {

  import config._

  val X = in UInt (widthX bits)
  val Y = in UInt (widthY bits)
  val R = out UInt (widthX + widthY bits)

  override def asNode = (dataIn: Seq[UInt]) => {
    this.X := dataIn(0)
    this.Y := dataIn(1)
    Seq(this.R)
  }
}

package org.datenlord
package xilinx

import xilinx.XilinxDeviceFamily._

import spinal.core._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try

case class VivadoUtil(lut: Int, ff: Int, dsp: Int, bram36: Int, carry8: Int = 0) {

  def getValues = Seq(lut, ff, dsp, bram36, carry8)

  def +(that: VivadoUtil) = VivadoUtil(this.getValues.zip(that.getValues).map { case (a, b) => a + b })

  def *(k: Int) = VivadoUtil(this.getValues.map(_ * k))

  // to get percentage
  def /(that: VivadoUtil) = this.getValues.zip(that.getValues).map { case (a, b) => a.toDouble / b }

  def <=(that: VivadoUtil) = this.getValues.zip(that.getValues).forall { case (a, b) => a.toDouble <= b }

  def >=(that: VivadoUtil) = that <= this

  def showInt(value:Int) = if(value == Int.MaxValue) "unlimited" else value.toString

  override def toString = {
    Seq("lut", "ff", "dsp", "bram36", "carry8").map(_.toUpperCase)
      .zip(getValues.map(showInt))
      .map{ case (name, value) => s"$name = $value"}.mkString(" ")
}

object VivadoUtil {
  def apply(values: Seq[Int]): VivadoUtil = new VivadoUtil(values(0), values(1), values(2), values(3), values(4))
  }
}

object VivadoUtilRequirement {
  val limit = Int.MaxValue

  def apply(lut: Int = limit, ff: Int = limit, dsp: Int = limit, bram36: Int = limit, carry8: Int = limit) =
    VivadoUtil(lut, ff, dsp, bram36, carry8)
}

/** this class is designed to extract information from Vivado log file(synth or impl)
 *
 */
class VivadoReport(
                    logPath: String,
                    deviceFamily: XilinxDeviceFamily
                  ) {

  val log = Source.fromFile(logPath)
  val lines = log.getLines.toSeq
  private val report = lines.mkString("\n")

  val patternAdder = """\s*(\d+)\s*Input\s*(\d+)\s*Bit\s*Adders :=\s*(\d+)\s*""".r
  val patternReg = """\s*(\d+)\s*Bit\s*Registers\s*:=\s*(\d+)\s*""".r

  // retailed components analysis
  var binaryAdderCost = 0
  var ternaryAdderCost = 0
  var registerCost = 0

  lines.foreach {
    case patternAdder(input, width, number) =>
      if(input.toInt == 2) binaryAdderCost += width.toInt * number.toInt
      if(input.toInt == 3) ternaryAdderCost += width.toInt * number.toInt
    case patternReg(width, number) =>
      registerCost += width.toInt * number.toInt
    case _ =>
  }

  logger.info(s"binary adder cost = $binaryAdderCost")
  logger.info(s"ternary adder cost = $ternaryAdderCost")
  logger.info(s"reg cost = $registerCost")

  // patterns
  private val intFind = "[0-9]\\d*"
  private val doubleFind = "-?(?:[1-9]\\d*\\.\\d*|0\\.\\d*[1-9]\\d*|0\\.0+|0)"

  def getPatternAfterHeader(header: String, pattern: String) = {
    val regex = s"\\|\\s*$header\\s*\\|\\s*($pattern)\\s*\\|"
    Try(regex.r.findFirstMatchIn(report).get.group(1))
  }

  def getIntAfterHeader: String => Int = getPatternAfterHeader(_, intFind).getOrElse("-1").toInt

  /*
  TODO:
    extract more attribute from doit.log
   */

  // TODO: extract information from detailed components
  val LUT = if (deviceFamily == UltraScale) getIntAfterHeader("CLB LUTs\\*") // FIXME: impl log doesn't have *
  else getIntAfterHeader("Slice LUTs")

  val FF = if (deviceFamily == UltraScale) getIntAfterHeader("CLB Registers")
  else getIntAfterHeader("Slice Registers")

  val DSP = getIntAfterHeader("DSPs")
  val BRAM = getIntAfterHeader("Block RAM Tile")
  val CARRY8 = getIntAfterHeader("CARRY8")

  val delayEx = s"Data Path Delay:\\s*(${doubleFind})ns"
  val DatapathDelay = delayEx.r.findFirstMatchIn(report).get.group(1).toDouble

  val Frequency = 1.0 / DatapathDelay * 1e9

  val util = VivadoUtil(LUT, FF, DSP, BRAM, CARRY8)

  def printArea(): Unit = logger.info(s"\nLUT: ${LUT}\nFF: ${FF}\nDSP: ${DSP}\nBRAM: ${BRAM}\nCARRY8: ${CARRY8}\n")

  def printFMax(): Unit = logger.info(s"\nfmax = ${Frequency / 1E6} MHz\n")

  def getReport = Array(LUT.toString, FF.toString, DSP.toString, BRAM.toString, Frequency.toString)

  def getUtil = util

  override def toString: String = s"LUT $LUT, FF $FF, DSP $DSP, BRAM $BRAM, CARRY8 $CARRY8, Freq $Frequency"

  def require(utilRequirement: VivadoUtil, fmaxRequirement: HertzNumber) = {
    assert(this.util <= utilRequirement, s"util failed: yours: $util, target: $utilRequirement")
    assert(this.Frequency >= fmaxRequirement.toDouble, s"critical path failed: yours: ${Frequency / 1e6} MHz, target: $fmaxRequirement")
  }

  log.close()
}

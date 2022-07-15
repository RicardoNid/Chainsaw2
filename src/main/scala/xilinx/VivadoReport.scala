package org.datenlord
package xilinx

import xilinx.XilinxDeviceFamily._

import spinal.core._

import java.nio.file.Paths
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
}

object VivadoUtil {
  def apply(values: Seq[Int]): VivadoUtil = new VivadoUtil(values(0), values(1), values(2), values(3), values(4))
}

object VivadoUtilRequirement{
  val limit = Int.MaxValue
  def apply(lut: Int = limit, ff: Int = limit, dsp: Int = limit, bram36: Int = limit, carry8: Int = limit) =
    VivadoUtil(lut, ff, dsp, bram36, carry8)
}

class VivadoReport(
                    workspacePath: String,
                    deviceFamily: XilinxDeviceFamily,
                    fmax: HertzNumber = null
                  ) {

  val log = Source.fromFile(Paths.get(workspacePath, "doit.log").toFile)
  private val report = log.getLines.mkString("\n")
  log.close()

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

  val LUT = if (deviceFamily == UltraScale) getIntAfterHeader("CLB LUTs\\*")
  else getIntAfterHeader("Slice LUTs")

  val FF = if (deviceFamily == UltraScale) getIntAfterHeader("CLB Registers")
  else getIntAfterHeader("Slice Registers")

  val DSP = getIntAfterHeader("DSPs")
  val BRAM = getIntAfterHeader("Block RAM Tile")
  val CARRY8 = getIntAfterHeader("CARRY8")

  val delayEx = s"Data Path Delay:\\s*(${doubleFind})ns"
  val DatapathDelay = delayEx.r.findFirstMatchIn(report).get.group(1).toDouble

  // TODO: extract slack info
  private val targetPeriod = fmax.toTime.toDouble
  val Frequency = 1.0 / DatapathDelay * 1e9

  val util = VivadoUtil(LUT, FF, DSP, BRAM, CARRY8)

  def printArea(): Unit = logger.info(s"\nLUT: ${LUT}\nFF: ${FF}\nDSP: ${DSP}\nBRAM: ${BRAM}\nCARRY8: ${CARRY8}\n")

  def printFMax(): Unit = logger.info(s"\nfmax = ${Frequency / 1E6} MHz\n")

  def getReport = Array(LUT.toString, FF.toString, DSP.toString, BRAM.toString, Frequency.toString)

  def getUtil = util

  override def toString: String = s"LUT $LUT, FF $FF, DSP $DSP, BRAM $BRAM, CARRY8 $CARRY8, Freq $Frequency"

  def require(utilRequirement: VivadoUtil, fmaxRequirement:HertzNumber) = {
    assert(this.util <= utilRequirement, s"util failed: yours: $util, target: $utilRequirement")
    assert(this.Frequency >= fmaxRequirement.toDouble, s"critical path failed: yours: ${Frequency / 1e6} MHz, target: $fmaxRequirement")
  }
}

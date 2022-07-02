package org.datenlord
package xilinx

import xilinx.XilinxDeviceFamily._

import spinal.core._

import java.nio.file.Paths
import scala.io.Source
import scala.language.postfixOps

case class VivadoUtil(lut: Int, ff: Int, dsp: Int, bram36: Int, carry8: Int = 0) {
  def +(that: VivadoUtil) = VivadoUtil(lut + that.lut, ff + that.ff, dsp + that.dsp, bram36 + that.bram36, this.carry8 + that.carry8)

  def *(k: Int) = VivadoUtil(lut * k, ff * k, dsp * k, bram36 * k, carry8 * k)

  // to get percentage
  def /(that: VivadoUtil) =
    (lut.toDouble / that.lut, ff.toDouble / that.ff,
      dsp.toDouble / that.dsp, bram36.toDouble / that.bram36, carry8.toDouble / that.carry8)
}

class VivadoReport(
                    workspacePath: String,
                    deviceFamily: XilinxDeviceFamily,
                    fmax: HertzNumber = null
                  ) {

  private val report = Source.fromFile(Paths.get(workspacePath, "doit.log").toFile).getLines.mkString

  private val intFind = "(\\d+,?)+".r // Regex to find int
  private val doubleFind = "-?(\\d+\\.?)+".r

  private def getIntAfter(pattern: String) = {
    try {
      val line = s"${pattern}[ ]*\\|[ ]*(\\d+,?)+".r
        .findFirstIn(report).get
        .replace(pattern, "")
      intFind
        .findFirstIn(line).get
        .toInt
    }
    catch {
      case e: Exception => -1
    }
  }

  private def getDoubleBefore(pattern: String) = {
    try {
      doubleFind.findFirstIn(s"-?(\\d+.?)+ns  \\(${pattern}\\)".r.findFirstIn(report).get).get.toDouble
    }
    catch {
      case e: Exception => -10000.0
    }
  }

  /*
  TODO:
    extract more attribute from doit.log
    1.build your "XXXFind" regex according to the attribute type
    2.build a "getXXX" method to extract your pattern safely
    3.create a field of VivadoReport, implement the corresponding "printXXX" method
   */

  val LUT = if (deviceFamily == UltraScale) getIntAfter("CLB LUTs\\*")
  else getIntAfter("Slice LUTs")

  val FF = if (deviceFamily == UltraScale) getIntAfter("CLB Registers")
  else getIntAfter("Slice Registers")

  val DSP = getIntAfter("DSPs")
  val BRAM = getIntAfter("Block RAM Tile")
  val CARRY8 = getIntAfter("CARRY8")

  private val targetPeriod = fmax.toTime.toDouble
  private val slack = getDoubleBefore("required time - arrival time") * 1e-9 //
  val Frequency = 1.0 / (targetPeriod - slack) // 1 / (T - WNS)

  val util = VivadoUtil(LUT, FF, DSP, BRAM, CARRY8)

  def printArea(): Unit = logger.info(s"\nLUT: ${LUT}\nFF: ${FF}\nDSP: ${DSP}\nBRAM: ${BRAM}\nCARRY8: ${CARRY8}\n")

  def printFMax(): Unit = logger.info(s"\nfmax = 1.0 / ($targetPeriod s - $slack s) = ${Frequency / 1E6} MHz\n")

  def getReport = Array(LUT.toString, FF.toString, DSP.toString, BRAM.toString, Frequency.toString)

  override def toString: String = s"LUT $LUT, FF $FF, DSP $DSP, BRAM $BRAM, Freq $Frequency"
}

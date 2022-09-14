package org.datenlord

import org.apache.commons.io.FileUtils
import spinal.core._

import java.io.File
import scala.language.postfixOps

package object xilinx {

  // enumeration of Vivado task types
  object VivadoTaskType extends Enumeration {
    type VivadoTaskType = Value
    val SYNTH, IMPL = Value
  }

  // enumeration of device families
  object XilinxDeviceFamily extends Enumeration {
    type XilinxDeviceFamily = Value
    val UltraScale, Series7 = Value
  }

  def xilinxCDConfig = ClockDomainConfig( // recommended by Xilinx UG901
    clockEdge = RISING,
    resetKind = ASYNC,
    resetActiveLevel = HIGH,
    softResetActiveLevel = HIGH,
    clockEnableActiveLevel = HIGH
  )

  // our devices

  import XilinxDeviceFamily._

  val vu9p = XilinxDevice(UltraScale, "xcvu9p-flga2104-2-i", 800 MHz)
  //      val zybo = XilinxDevice(Series7, "xc7z010", 125 MHz, constraint = FileUtils.readFileToString(new File("./src/main/resources/zybo.xdc")))
  val zcu104 = XilinxDevice(UltraScale, "xczu7ev-ffvc1156-2-e", 200 MHz)
  // TODO: find out part name for U250
  val u250 =
    XilinxDevice(UltraScale, "XCU250-FIGD2104-2L-E".toLowerCase, 800 MHz)
  val u200 =
    XilinxDevice(UltraScale, "XCU200-FSGD2104-2-E".toLowerCase, 800 MHz)
  val kcu1500 = XilinxDevice(UltraScale, "xcku115-flvb2104-2-e", 800 MHz)

  val defaultDevice = u200
  val defaultDeviceFamily = defaultDevice.family
  val defaultVivadoPath = "/tools/Xilinx/Vivado/2021.1/bin"
}

package org.datenlord
package xilinx

import xilinx.XilinxDeviceFamily._

import spinal.core._

import scala.collection.mutable.ArrayBuffer

case class VivadoConfig(
                         vivadoPath: String = defaultVivadoPath,
                         processorCount: Int = 10
                       )

case class XilinxDevice(
                         family: XilinxDeviceFamily,
                         part: String,
                         fmax: HertzNumber,
                         constraint: String = null
                       )

// TODO: design methods to generate Vivado constraint
case class VivadoConstraint(commands: ArrayBuffer[String] = ArrayBuffer[String]()) {

  def setLoc(loc: String, port: String) = {
    commands :+ s"set_property LOC $loc [get_cells $port]"
    this
  }
}
package org.datenlord
package ip.das

import spinal.core._

import scala.language.postfixOps

// TODO: FifoWrite & MemRead
case class FifoRead(device: XillybusDevice) extends Bundle {
  val rden, open = out Bool()
  val empty, eof = in Bool()
  val data = in Bits (device.width bits)
  this.setName(s"${device.fullName}")
}

case class FifoWrite(device: XillybusDevice) extends Bundle {
  val wren, open = out Bool()
  val full = in Bool()
  val data = in Bits (device.width bits)
  this.setName(s"${device.fullName}")
}

case class MemBi(device: XillybusDevice) extends Bundle {
  // Host -> FPGA
  val wren, openW = out Bool()
  val dataW = out Bits (device.width bits)
  val full = in Bool()
  val wName = s"user_w_${device.name}"
  wren.setName(s"${wName}_wren")
  full.setName(s"${wName}_full")
  dataW.setName(s"${wName}_data")
  openW.setName(s"${wName}_open")
  // FPGA -> Host
  val rden, openR = out Bool()
  val empty, eof = in Bool()
  val dataR = in Bits(device.width bits)
  val rName = s"user_r_${device.name}"
  rden.setName(s"${rName}_rden")
  empty.setName(s"${rName}_empty")
  dataR.setName(s"${rName}_data")
  eof.setName(s"${rName}_eof")
  openR.setName(s"${rName}_open")
  // addr
  val addr = out UInt (device.addrWidth bits)
  val addr_update = out Bool()
  addr.setName(s"user_${device.name}_addr")
  addr_update.setName(s"user_${device.name}_addr_update")
}

case class MemWrite(device: XillybusDevice) extends Bundle {
  val wren, open, addr_update = out Bool()
  val addr = out UInt (device.addrWidth bits)
  val data = out Bits (device.width bits)
  val full = in Bool()
  this.setName(s"${device.fullName}")
  addr.setName(s"user_${device.name}_addr")
  addr_update.setName(s"user_${device.name}_addr_update")
}

/** pcie-related signals on a board, corresponding pin locations are needed
 *
 */
case class PcieBundle() extends Bundle {
  val perstn, refclk = in Bool()
  val rx = in Bits (4 bits)
  val tx = out Bits (4 bits)
  this.setName("pcie")
}

/** a xillybus device file
 * @param name the name you specify through xillybus IP factory [[http://xillybus.com/custom-ip-factory]]
 * @param deviceType stream(FIFO) or address based(RegFile or Mem) port
 * @param direction write for Host -> FPGA, "read" for FPGA -> Host
 * @param width bit width of the fifo/register, can be 8, 16, 32
 * @param addrWidth for address based port, width of address
 */
case class XillybusDevice(name: String, deviceType: String, direction: String, width: Int, addrWidth: Int = -1) {

  val directionName = direction match {
    case "write" => "w"
    case "read" => "r"
    case "bi" => ""
  }

  def fullName = s"user_${directionName}_$name"

  def winAddress = s"\\\\.\\xillybus_$name"
}

/** black box generator for xillybus IP
 * @param devices the device files you define through xillybus IP factory [[http://xillybus.com/custom-ip-factory]]
 */
case class Xillybus(devices: Seq[XillybusDevice]) extends BlackBox {
  val quiesce, bus_clk = out Bool()
  val user_led = out Bits (4 bits)
  val pcie = PcieBundle()
  val FifoReads = devices.filter(device => device.direction == "read" && device.deviceType == "fifo").map(FifoRead)
  val FifoWrites = devices.filter(device => device.direction == "write" && device.deviceType == "fifo").map(FifoWrite)
  val MemWrites = devices.filter(device => device.direction == "write" && device.deviceType == "mem").map(MemWrite)
  setDefinitionName("xillybus")
}
package org.datenlord

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

// TODO: FifoWrite & MemRead
case class FifoRead(device: XillybusDevice) extends Bundle {
  val rden, open = out Bool()
  val empty, eof = in Bool()
  val data = in Bits (device.width bits)
  this.setName(s"${device.fullName}")
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

case class PcieBundle() extends Bundle {
  val perstn, refclk = in Bool()
  val rx = in Bits (4 bits)
  val tx = out Bits (4 bits)
  this.setName("pcie")
}

case class XillybusDevice(name: String, deviceType: String, direction: String, width: Int, addrWidth: Int = -1) {

  val directionName = direction match {
    case "write" => "w"
    case "read" => "r"
  }

  def fullName = s"user_${directionName}_${name}"

  def winAddress = s"\\\\.\\xillybus_$name"
}

case class Xillybus(devices: Seq[XillybusDevice]) extends BlackBox {
  val quiesce, bus_clk = out Bool()
  val user_led = out Bits (4 bits)
  val pcie = PcieBundle()
  val FifoReads = devices.filter(device => device.direction == "read" && device.deviceType == "fifo").map(FifoRead)
  val MemWrites = devices.filter(device => device.direction == "write" && device.deviceType == "mem").map(MemWrite)
  setDefinitionName("xillybus")
}

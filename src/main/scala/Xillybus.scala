package org.datenlord

import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

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
}

case class PcieBundle() extends Bundle {
  val perstn, refclk = in Bool()
  val rx = in Bits (4 bits)
  val tx = out Bits (4 bits)
  this.setName("pcie")
}

case class XillybusDevice(name: String, deviceType: String, direction: String, width: Int, addrWidth: Int = -1){

  val directionName = direction match {
    case "write" => "w"
    case "read" => "r"
  }

  def fullName = s"${directionName}_${name}"
}

case class Xillybus(devices: Seq[XillybusDevice]) extends BlackBox {
  val quiesce, bus_clk = out Bool()
  val user_led = out Bits (4 bits)
  val pcie = PcieBundle()
  val FifoReads = devices.filter(device => device.direction == "read" && device.deviceType == "fifo").map(FifoRead)
  val MemWrites = devices.filter(device => device.direction == "write" && device.deviceType == "mem").map(MemWrite)
  this.setName("user")
}

object Xillybus {

  val device0 = XillybusDevice("read_32", "fifo", "read", 32)
  val device1 = XillybusDevice("ctrl", "mem", "write", 8, 5)

  val top = () => new Module {

    val pcie = PcieBundle()
    val xillybus = Xillybus(Seq(device0, device1))
    // global signals
    pcie <> xillybus.pcie

    val counter = CounterFreeRun(1 << 16)
    val fifo = StreamFifo(Bits(32 bits), 512)
    val stateRam = Mem(Bits(8 bits), 1 << 5)

    val fifoInterface = xillybus.FifoReads(0)
    val memInterface = xillybus.MemWrites(0)

    // counter -> FIFO
    fifo.io.push.valid := True
    fifo.io.push.payload := counter.value.resize(32).asBits

    // FIFO -> xillybus
    fifoInterface.data := fifo.io.pop.payload
    // FIXME: expose the empty signal
    //    fifoInterface.empty := fifo.logic.empty
    fifoInterface.empty := ~fifo.io.pop.valid
    fifoInterface.eof := True
    fifo.io.pop.ready := fifoInterface.rden

    // xillybus -> RAM
    stateRam.write(memInterface.addr, memInterface.data, memInterface.wren)
    memInterface.full := False
  }

  def main(args: Array[String]): Unit = {
    SpinalVerilog(top())
  }
}

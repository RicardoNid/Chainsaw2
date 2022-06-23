package org.datenlord
package ip.das

import intel.AlteraFIFO

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class HostInterface(width: Int) extends Bundle with IMasterSlave {

  val data = Bits(width bits)
  val ctrlAddr = UInt(5 bits)
  val ctrlValue = UInt(8 bits)
  val ctrlEnable = Bool()

  override def asMaster(): Unit = {
    in(data)
    out(ctrlAddr)
    out(ctrlValue)
    out(ctrlEnable)
  }
}

case class Host() extends Component {

  val clkPro, rstn = in Bool()
  val hostInterface = master(HostInterface(14))
  val pcieInterface = PcieBundle()

  val device0 = XillybusDevice("read_32", "fifo", "read", 32)
  val device1 = XillybusDevice("ctrl", "mem", "write", 8, 5)
  val xillybus = Xillybus(Seq(device0, device1))
  xillybus.pcie <> pcieInterface

  val fifo = AlteraFIFO(32)
  fifo.setDefinitionName("PCIeFIFO")

  val proDomain = ClockDomain(
    clock = clkPro, reset = rstn, config = dasClockConfig,
    frequency = FixedFrequency(125 * processFactor MHz)
  )

  val pcieClockDomain = ClockDomain(
    clock = xillybus.bus_clk, reset = rstn, config = dasClockConfig,
    frequency = FixedFrequency(125 MHz)
  )

  new ClockingArea(proDomain) {
    val validCounter = CounterFreeRun(4)
    val parityCounter = CounterFreeRun(BigInt(1) << (32 - hostInterface.width * 2)) // 8 bit parity for data integrity
    val parity = parityCounter.value
    parity.setName("parity")

    val dataWithParity = RegNext(hostInterface.data) ## hostInterface.data ## parity.asBits

    fifo.wrclk := clkPro
    fifo.wrreq := validCounter.willOverflow
    fifo.data := RegNext(dataWithParity)
  }

  new ClockingArea(pcieClockDomain) {
    val fifoInterface = xillybus.FifoReads(0)
    val memInterface = xillybus.MemWrites(0)

    // FIFO -> xillybus
    fifo.rdclk := xillybus.bus_clk
    fifo.rdreq := fifoInterface.rden
    fifoInterface.data := fifo.q
    fifoInterface.empty := fifo.rdempty
    fifoInterface.eof := False

    // xillybus -> registers, control
    memInterface.full := False

    hostInterface.ctrlAddr := memInterface.addr
    hostInterface.ctrlValue := memInterface.data.asUInt
    hostInterface.ctrlEnable := memInterface.wren
  }
}

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

/** connection with host (via xillybus IP)
 *
 */
case class Host() extends Component {

  // TODO: write a cross-clockdomain template in IDEA
  val clkRef, rstn = in Bool()
  val hostInterface = master(HostInterface(14))
  val pcieInterface = PcieBundle()

  val device0 = XillybusDevice("read_32", "fifo", "read", 32)
  val device1 = XillybusDevice("ctrl", "mem", "write", 8, 5)
  val xillybus = Xillybus(Seq(device0, device1))

//  val info0 = XillybusDevice("info_0_8", "fifo", "read", 8) // 1MB/s
//  val info1 = XillybusDevice("info_1_8", "fifo", "read", 8) // 1MB/s
//  val adc0 = XillybusDevice("adc_0_16", "fifo", "read", 16) // 100MB/s
//  val adc1 = XillybusDevice("adc_1_16", "fifo", "read", 16) // 100MB/s
//  val ctrl = XillybusDevice("ctrl_8", "mem", "write", 8, 5) // 0.1MB/s
//  val write = XillybusDevice("write_16", "fifo", "write", 16) // 10MB/s
//  val devices  = Seq(adc0, adc1, info0, info1, ctrl,  write)

  xillybus.pcie <> pcieInterface

  // fifos
  val fifo = AlteraFIFO(32)
  fifo.setDefinitionName("PCIeFIFO")

  // data processing domain
  val proDomain = ClockDomain(
    clock = clkRef, reset = rstn, config = dasClockConfig,
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

    fifo.wrclk := clkRef
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

package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps


/** interface of a MemWrite device when you use it a as control channel which modify a register file for control
 * @param ctrlDevice the MemWrite device configuration
 */
case class CtrlInterface(ctrlDevice: XillybusDevice) extends Bundle {
  val ctrlUpdate = Bool()
  val ctrlAddr = UInt(ctrlDevice.addrWidth bits)
  val ctrlValue = UInt(ctrlDevice.width bits)
}

/** wrapper for xillybus that generate FIFOs and RAMs automatically, only expose read/write ports for each channel
 *
 * @param devices the device files you define through xillybus IP factory [[http://xillybus.com/custom-ip-factory]]
 */
case class XillybusWrapper(devices: Seq[XillybusDevice]) extends Component {

  val userClk = in Bool() // clock from user application domain(rather than xillybus domain)
  val pcie = PcieBundle()

  val xillybus = Xillybus(devices) // xillybus IP

  // I/O
  val fifoWriteInterfaces = xillybus.streamsRead.map(device => slave(FifoWriteInterface(device.width)))
  val fifoReadInterfaces = xillybus.streamsWrite.map(device => slave(FifoReadInterface(device.width)))
  require(xillybus.memsWrite.length == 1, "this wrapper is valid only when xillybus has one and only one MemWrite device")
  val ctrlDevice = xillybus.memsWrite.head
  val ctrlOut = out(CtrlInterface(ctrlDevice))

  // global connections
  pcie <> xillybus.pcie

  // stream channels connections
  xillybus.streamsRead.zip(xillybus.streamReadInterfaces.zip(fifoWriteInterfaces))
    .foreach { case (device, (busSide, userSide)) =>
      val fifo = DcFifo(device.width)
      fifo.setDefinitionName(s"fifo${device.width}")
      // xillybus <-> FIFO
      fifo.rdclk := xillybus.bus_clk
      fifo.rdreq := busSide.rden
      busSide.data := fifo.data
      busSide.empty := fifo.rdempty
      busSide.eof := False
      // user logic <-> FIFO
      fifo.wrclk := userClk
      fifo.wrreq := userSide.wrreq
      fifo.data := userSide.data
      userSide.wrfull := fifo.wrfull
    }

  xillybus.streamsWrite.zip(xillybus.streamWriteInterfaces.zip(fifoReadInterfaces))
    .foreach { case (device, (busSide, userSide)) =>
      val fifo = DcFifo(device.width)
      fifo.setDefinitionName(s"fifo${device.width}")
      // xillybus <-> FIFO
      fifo.wrclk := xillybus.bus_clk
      fifo.wrreq := busSide.wren
      fifo.data := busSide.data
      busSide.full := fifo.wrfull
      // user logic <-> FIFO
      fifo.rdclk := userClk
      fifo.rdreq := userSide.rdreq
      userSide.q := fifo.q
      userSide.rdempty := fifo.rdempty
    }

  // ctrl channel connections
  val ctrlChannel = xillybus.memWriteInterfaces.head
  ctrlChannel.full := False // as we use a register file
  ctrlOut.ctrlUpdate := ctrlChannel.wren
  ctrlOut.ctrlAddr := ctrlChannel.addr
  ctrlOut.ctrlValue := ctrlChannel.data

  def getReadInterfaceByName(name: String) =
    xillybus.streamsWrite.zip(fifoReadInterfaces)
      .find(_._1.name == name).get._2

  def getWriteInterfaceByName(name: String) =
    xillybus.streamsRead.zip(fifoWriteInterfaces)
      .find(_._1.name == name).get._2
}
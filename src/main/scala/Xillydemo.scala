package org.datenlord

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class Xillydemo() extends Component {

  val device0 = XillybusDevice("read_32", "fifo", "read", 32)
  val device1 = XillybusDevice("ctrl", "mem", "write", 8, 5)

  val reset = in Bool() // TODO: considering reset
  val pcie = PcieBundle()
  val xillybus = Xillybus(Seq(device0, device1))
  // global signals
  pcie <> xillybus.pcie

  val currentClockDomain = ClockDomain(clock = xillybus.bus_clk, reset = reset)

  new ClockingArea(currentClockDomain) {
    val counter = CounterFreeRun(1 << 16)
    val fifo = StreamFifo(Bits(32 bits), 512)
    val stateRam = Mem(Bits(8 bits), 1 << 5)

    val fifoInterface = xillybus.FifoReads(0)
    val memInterface = xillybus.MemWrites(0)

    // counter -> FIFO
    fifo.io.push.valid := True
    fifo.io.push.payload := counter.value.resize(32).asBits

    // FIFO -> xillybus
    fifoInterface.data := B(BigInt("42434445", 16), 32 bits)
    fifoInterface.empty := False
    fifoInterface.eof := False
    fifo.io.pop.ready := fifoInterface.rden

    // xillybus -> RAM
    stateRam.write(memInterface.addr, memInterface.data, memInterface.wren)
    memInterface.full := False
  }
  setDefinitionName("xillydemo")
}

object Xillydemo {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(Xillydemo())
  }
}

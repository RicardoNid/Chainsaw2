package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

case class PcieIntegrity() extends Component {

  val device0 = XillybusDevice("read_32", "fifo", "read", 32)
  val device1 = XillybusDevice("ctrl", "mem", "write", 8, 5)

  // I/O
  val rstn = in Bool()
  val pcie = PcieBundle() // pcie-related signals
  val occupancy = out UInt (10 bits)
  val produce, consume = out Bool()
  val gain = out UInt (6 bits)

  // global signals
  val xillybus = Xillybus(Seq(device0, device1))
  pcie <> xillybus.pcie

  val clockConfig = ClockDomainConfig(resetActiveLevel = LOW)
  val pcieClockDomain = ClockDomain(clock = xillybus.bus_clk, reset = rstn, config = clockConfig)

  // control registers

  new ClockingArea(pcieClockDomain) {

    val n = 4 // data rate is 1/n of the max data rate
    val validCounter = CounterFreeRun(n)
    val dataCounter = Counter(BigInt(1) << 32, inc = validCounter.willOverflowIfInc)

    dataCounter.value.setName("counter_value")

    val fifo = StreamFifo(Bits(32 bits), 512)

    val fifoInterface = xillybus.FifoReads(0)
    val memInterface = xillybus.MemWrites(0)

    // counter -> FIFO
    fifo.io.push.valid := validCounter.willOverflow
    fifo.io.push.payload := dataCounter.value.asBits

    // FIFO -> xillybus
    fifoInterface.data := fifo.io.pop.payload
    fifo.io.pop.ready := RegNext(fifoInterface.rden)
    //    fifoInterface.empty := !fifo.io.pop.valid
    val emptyReg = RegInit(False)
    when(fifo.io.occupancy <= U(2) && fifoInterface.rden)(emptyReg.set())
    when(fifo.io.occupancy > U(2))(emptyReg.clear())
    fifoInterface.empty := emptyReg
    fifoInterface.eof := False

    // xillybus -> RAM
    val gainValue = RegInit(U(0, 6 bits))
    when(memInterface.wren) {
      when(memInterface.addr === U(3))(gainValue := memInterface.data.takeLow(6).asUInt)
    }
    memInterface.full := False

    occupancy := fifo.io.occupancy
    gain := gainValue
    produce := fifo.io.push.valid
    consume := fifoInterface.rden
  }
  setDefinitionName("xillydemo")

}

object PcieIntegrity {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "/home/ltr/sysudas/project/xillybus-custom/verilog/src").generateVerilog(PcieIntegrity())
    //    SpinalVerilog(PcieIntegrity())
  }
}

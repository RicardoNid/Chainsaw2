package org.datenlord
package ip.das

import org.datenlord.intel.{AlteraLvdsRx, AlteraPll}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

// this is the top module of DAS board, which generate all I/Os that should be assigned to pins on FPGA
case class LvdsWithPcie() extends Component {

  // pcie device file
  val device0 = XillybusDevice("read_32", "fifo", "read", 32)
  val device1 = XillybusDevice("ctrl", "mem", "write", 8, 5)

  // parameters
  val desFactor = 4

  // I/O assigned
  // ADC related
  val adc_clk = in Bool() // 250MHz
  val rstn = in Bool()
  val adc_in_a, adc_in_b = in UInt (7 bits) // data from two ADCs
  val vga_b = out UInt (6 bits) // control the amplitude of the amplifier
  // pcie related
  val pcieInterface = PcieBundle()
  // I/O for signal tap
  // states
  val gauge = out UInt (8 bits) // gauge factor
  val downFactor = out UInt (8 bits) //
  val occupancy = out UInt (10 bits)
  val produce, consume = out Bool()
  // data
  val dout_a, dout_b = out UInt (14 bits) // LVDS signal out for user logic

  // clock generation & clock domain definition
  val pll = AlteraPll(4)
  pll.setDefinitionName("LVDSPLL")
  pll.refclk := adc_clk
  pll.rst := ~rstn
  val Seq(clkFast0, // 500MHz
  clkSlow, // 62.5MHz
  clkParallel, // 62.5MHz
  clkFast1 // 500MHz
  ) = pll.outclk

  val xillybus = Xillybus(Seq(device0, device1))

  val clockConfig = ClockDomainConfig(resetActiveLevel = LOW)
  val lvdsClockDomain = ClockDomain(
    clock = clkParallel, reset = rstn, config = clockConfig,
    frequency = FixedFrequency(250.0 / desFactor MHz)
  )
  val pcieClockDomain = ClockDomain(
    clock = xillybus.bus_clk, reset = rstn, config = clockConfig,
    frequency = FixedFrequency(125 MHz)
  )

  // modules and connections
  // pcie
  pcieInterface <> xillybus.pcie
  // lvds
  // as the ADC output is in DDR format, factor here is actual desFactor multiplied by 2
  val lvdsA, lvdsB = AlteraLvdsRx(7, desFactor * 2)
  lvdsA.setDefinitionName("LVDS14")
  lvdsB.setDefinitionName("LVDS14")

  // clock domain cross fifo
  val fifo = StreamFifoCC(Bits(32 bits), 512, lvdsClockDomain, pcieClockDomain)
  logger.info(s"fifo from ${lvdsClockDomain.frequency.getValue} to ${pcieClockDomain.frequency.getValue}")

  // push domain
  new ClockingArea(lvdsClockDomain) {
    // lvds connections
    Seq(lvdsA, lvdsB).zip(Seq(adc_in_a, adc_in_b).zip(Seq(dout_a, dout_b))).foreach {
      case (lvds, (dataIn, dataOut)) =>
        lvds.rx_inclock := clkFast0
        lvds.rx_enable := clkSlow
        lvds.rx_in := dataIn.asBits
        // bit extraction
        val out1of4prev = lvds.rx_out.asBools.zipWithIndex.filter(_._2 % (desFactor * 2) == 0).map(_._1) // e.g. 48,40,32...
        val out1of4next = lvds.rx_out.asBools.zipWithIndex.filter(_._2 % (desFactor * 2) == 1).map(_._1) // e.g. 49,41,33...
        val out1of4 = out1of4next.zip(out1of4prev).flatMap { case (next, prev) => Seq(next, prev) }.asBits() // e.g. 48,49,40,41...
        dataOut := out1of4.d(1).asUInt
    }
    // lvds -> FIFO
    val sampleCounter = DynamicCounter(downFactor)
    sampleCounter.increment()
    val parityCounter = Counter(BigInt(1) << 8, inc = sampleCounter.willOverflow) // 8 bit parity for data integrity

    fifo.io.push.valid := sampleCounter.willOverflow // down sampling by downFactor
    fifo.io.push.payload := (dout_a @@ parityCounter.value).resize(32).asBits

    produce := fifo.io.push.valid
  }

  // pop domain
  new ClockingArea(pcieClockDomain) {
    val fifoInterface = xillybus.FifoReads(0)
    val memInterface = xillybus.MemWrites(0)

    // FIFO -> xillybus
    fifoInterface.data := fifo.io.pop.payload
    fifo.io.pop.ready := RegNext(fifoInterface.rden)
    val emptyReg = RegInit(False)
    when(fifo.io.popOccupancy <= U(3) && fifoInterface.rden)(emptyReg.set())
    when(fifo.io.popOccupancy > U(3))(emptyReg.clear())
    fifoInterface.empty := emptyReg
    fifoInterface.eof := False

    // xillybus -> RAM, control
    // register declaration
    val gainValue = RegInit(U(0, 6 bits))
    val gaugeValue = RegInit(U(0, 8 bits))
    val downFactorValue = RegInit(U(8, 8 bits))
    downFactorValue.addTag(crossClockDomain)
    // control logic
    memInterface.full := False
    when(memInterface.wren) {
      // gain control, addr = 0
      when(memInterface.addr === U(0))(gainValue := memInterface.data.takeLow(6).asUInt)
      // gauge control, addr = 1
      when(memInterface.addr === U(1))(gaugeValue := memInterface.data.takeLow(8).asUInt)
      // down factor control, addr = 2
      when(memInterface.addr === U(2))(downFactorValue := memInterface.data.takeLow(8).asUInt)
    }

    occupancy := fifo.io.popOccupancy
    vga_b := gainValue
    gauge := gaugeValue
    downFactor := downFactorValue
    consume := fifoInterface.rden
  }
  setDefinitionName("xillydemo")

}

object LvdsWithPcie {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "/home/ltr/sysudas/project/xillybus-combined/verilog/src").generateVerilog(LvdsWithPcie())
  }
}

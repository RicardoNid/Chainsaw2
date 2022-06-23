package org.datenlord
package ip.das

import org.datenlord.intel.{AlteraFIFO, AlteraPll}
import spinal.core._
import spinal.lib._

case class CdcTest() extends Component {

  val adc_clk, rstn = in Bool()
  val numberOut = out UInt (10 bits)

  val pll = AlteraPll(1)
  pll.setDefinitionName("pll")
  pll.refclk := adc_clk
  pll.rst := ~rstn
  val clkSlow = pll.outclk.head

  val domainFast = ClockDomain(clock = adc_clk, reset = rstn, config = dasClockConfig)
  val domainSlow = ClockDomain(clock = clkSlow, reset = rstn, config = dasClockConfig)

  val numbers = Seq.fill(4)(UInt(10 bits))
  numbers.foreach(_.addTag(crossClockDomain))

  val fifos = Seq.fill(4)(AlteraFIFO(10))
  fifos.foreach(_.setDefinitionName("FifoForMerge"))

  new ClockingArea(domainSlow) {
    val counters = Seq.fill(4)(CounterFreeRun(1 << 8))
    //    val slowRegs = Seq.fill(4)(Reg(UInt(10 bits)))
    //    slowRegs.zipWithIndex.foreach{ case (reg, i) => reg.setName(s"SlowReg_$i")}
    (0 until 4).foreach(i => fifos(i).data := (counters(i).value @@ U(i, 2 bits)).asBits)
    fifos.foreach { fifo =>
      fifo.wrclk := clkSlow
      fifo.wrreq := True
    }
    //    numbers.zip(slowRegs).foreach{ case (number, slowReg) => number := slowReg}
  }

  new ClockingArea(domainFast) {
    val counter = CounterFreeRun(4)
    //    val fastRegs = numbers.map(RegNextWhen(_, counter.value === 2).d(2))
    //    fastRegs.zipWithIndex.foreach { case (reg, i) => reg.setName(s"FastReg_$i") }
    //    val values = (0 until 4).map(i => fastRegs(i).d(i))
    fifos.foreach { fifo =>
      fifo.rdclk := adc_clk
      fifo.rdreq := counter.willOverflow
    }

    switch(counter.value) {
      (0 until 4).foreach(i => is(i)(numberOut := fifos(i).q.asUInt))
    }
  }

  setDefinitionName("CdcTest")
}

object CdcTest {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "/home/ltr/sysudas/project/CdcTest").generateVerilog(CdcTest())
  }
}

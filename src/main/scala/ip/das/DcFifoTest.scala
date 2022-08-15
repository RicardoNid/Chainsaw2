package org.datenlord
package ip.das

import org.datenlord.intel.{AlteraFIFO, AlteraPll}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DcFifoTest() extends Component {

  val adc_clk, rstn = in Bool()
  val numberOut = out UInt (10 bits)
  val discontinuity = out Bool()

  val pll = AlteraPll(2)
  pll.setDefinitionName("Pll4to1")
  pll.refclk := adc_clk
  pll.rst := ~rstn
  val clkSlow = pll.outclks.head
  val clkFast = pll.outclks.last

  val domainFast = ClockDomain(clock = clkFast, reset = rstn, config = dasClockConfig)
  val domainSlow = ClockDomain(clock = clkSlow, reset = rstn, config = dasClockConfig)

  val fifos = Seq.fill(4)(AlteraFIFO(8))
  fifos.foreach { fifo =>
    fifo.setDefinitionName("AsyncFifo")
    fifo.rdclk := clkFast
    fifo.wrclk := clkSlow
  }

  new ClockingArea(domainSlow) {
    val counters = Seq.fill(4)(CounterFreeRun(1 << 8))
    fifos.zip(counters).foreach { case (fifo, counter) =>
      fifo.wrreq := True
      fifo.data := counter.value.asBits
    }
  }

  new ClockingArea(domainFast) {
    val reqCounter = CounterFreeRun(4)
    val qs = Vec(UInt(10 bits), 4)
    fifos.zipWithIndex.foreach { case (fifo, i) =>
      fifo.rdreq := reqCounter.willOverflow
      qs(i) := fifo.q.asUInt @@ U(i, 2 bits)
    }
    switch(reqCounter.value) {
      qs.zipWithIndex.foreach { case (q, i) =>
        is(i)(numberOut := q.d(i))
      }
    }
    discontinuity := (numberOut =/= RegNext(numberOut) + U(1))
  }
}

object DcFifoTest {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "/home/ltr/sysudas/project/DcFifoTest").generateVerilog(DcFifoTest())
  }
}

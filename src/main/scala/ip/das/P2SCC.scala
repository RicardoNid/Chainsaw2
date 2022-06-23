package org.datenlord
package ip.das

import org.datenlord.intel.{AlteraFIFO, AlteraPll}
import spinal.core._
import spinal.lib._

// cross clock-domain parallel-to-serial converter
case class P2SCC(width: Int, N: Int) extends Component {

  val clkRef, rstn = in Bool()
  val clkOut = out Bool()
  val dataIns = in Vec(Bits(width bits), N)
  val dataOut = out Bits(width bits)

  val pll = AlteraPll(2)
  pll.setDefinitionName("PllP2S")
  pll.refclk := clkRef
  pll.rst := ~rstn
  val clkSlow = pll.outclk.head
  val clkFast = pll.outclk.last
  clkOut := clkFast

  val domainSlow = ClockDomain(clock = clkSlow, reset = rstn, config = dasClockConfig)
  val domainFast = ClockDomain(clock = clkFast, reset = rstn, config = dasClockConfig)

  val fifos = Seq.fill(4)(AlteraFIFO(width))
  fifos.foreach { fifo =>
    fifo.setDefinitionName("AsyncFifo")
    fifo.rdclk := clkFast
    fifo.wrclk := clkSlow
  }

  new ClockingArea(domainSlow) {
    fifos.zip(dataIns).foreach { case (fifo, data) =>
      fifo.wrreq := True
      fifo.data := data
    }
  }

  new ClockingArea(domainFast) {
    val reqCounter = CounterFreeRun(4)
    val qs = Vec(Bits(width bits), 4)
    fifos.zipWithIndex.foreach { case (fifo, i) =>
      fifo.rdreq := reqCounter.willOverflow
      qs(i) := fifo.q
    }
    switch(reqCounter.value) {
      qs.reverse.zipWithIndex.foreach { case (q, i) =>
        is(i)(dataOut := q.d(i))
      }
    }
  }
}

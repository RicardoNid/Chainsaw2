package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

// cross clock-domain parallel-to-serial converter
case class P2SCC(width: Int, N: Int) {

  val refClk, rstn = in Bool()
  val dataIns = Vec(Bits(width bits), N)
  val dataOut = Bits(width bits)

  val pll = AlteraPll(2)
  pll.setDefinitionName("PllP2S")
  pll.refclk := refClk
  pll.rst := ~rstn
  val clkSlow = pll.outclk.head
  val clkFast = pll.outclk.last

  val domainFast = ClockDomain(clock = clkFast, reset = rstn, config = dasClockConfig)
  val domainSlow = ClockDomain(clock = clkSlow, reset = rstn, config = dasClockConfig)

  val fifos = Seq.fill(4)(AlteraFIFO(8))
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
    val qs = Vec(UInt(10 bits), 4)
    fifos.zipWithIndex.foreach { case (fifo, i) =>
      fifo.rdreq := reqCounter.willOverflow
      qs(i) := fifo.q.asUInt @@ U(i, 2 bits)
    }
    switch(reqCounter.value) {
      qs.zipWithIndex.foreach { case (q, i) =>
        is(i)(dataOut := q.d(i))
      }
    }
  }
}

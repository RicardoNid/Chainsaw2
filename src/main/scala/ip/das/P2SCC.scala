package org.datenlord
package ip.das

import intel.{AlteraFIFO, AlteraPll}

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class P2SCCInterface(width:Int, factor: Int) {
  val clkIn, rstn = in Bool() // clk for parallel in
  val clkOut = out Bool() // clk for serial out
  val dataIns = in Vec(Bits(width bits), factor)
  val dataOut = out Bits (width bits)
}

// cross clock-domain parallel-to-serial converter
case class P2SCC(width: Int, factor: Int) extends Bundle  {

  val clkRef, rstn = in Bool()
  val clkOut = out Bool()
  val dataIns = in Vec(Bits(width bits), factor)
  val dataOut = out Bits (width bits)

  // clock generation
  val pll = AlteraPll(2)
  pll.setDefinitionName("PllP2S")
  pll.refclk := clkRef
  pll.rst := ~rstn
  val clkSlow = pll.outclks.head
  val clkFast = pll.outclks.last
  clkOut := clkFast

  // clock domain definitions
  val domainSlow = ClockDomain(clock = clkSlow, reset = rstn, config = dasClockConfig)
  val domainFast = ClockDomain(clock = clkFast, reset = rstn, config = dasClockConfig)

  // instantiating async FIFO
  val fifos = Seq.fill(4)(AlteraFIFO(width))
  fifos.foreach { fifo =>
    fifo.setDefinitionName("AsyncFifo")
    fifo.rdclk := clkFast
    fifo.wrclk := clkSlow
  }

  new ClockingArea(domainSlow) {
    fifos.zip(dataIns).foreach { case (fifo, data) =>
      fifo.wrreq := True // real-time
      fifo.data := data
    }
  }

  // P2S logic
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

package org.datenlord
package ip.das

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

// cross clock-domain parallel-to-serial converter
case class P2SCC(width: Int, factor: Int, baseFrequency: HertzNumber = 100 MHz)
  extends Component {

  // clk for parallel in and serial out
  val clkIn, clkOut, rstn = in Bool()
  val dataIns = in Vec(Bits(width bits), factor) // parallel in, dataIns(0) should be the first element in serial out
  val dataOut = out Bits (width bits) // serial out

  /** --------
   * clock domain definitions
   * -------- */
  val frequencySlow = FixedFrequency(baseFrequency)
  val frequencyFast = FixedFrequency(frequencySlow.getValue * factor)
  val domainSlow = ClockDomain(clock = clkIn, reset = rstn, config = dasClockConfig, frequency = frequencySlow)
  val domainFast = ClockDomain(clock = clkOut, reset = rstn, config = dasClockConfig, frequency = frequencyFast)
  clkIn.setName(s"clk_${frequencySlow.getValue.toInt/1000000}M")
  clkOut.setName(s"clk_${frequencyFast.getValue.toInt/1000000}M")

  /** --------
   * CDC data transfer by async FIFO
   -------- */
  val fifo = AlteraDcFifo(width * factor)
  fifo.setDefinitionName("P2SFIFO")
  fifo.wrclk := clkIn
  fifo.rdclk := clkOut

  new ClockingArea(domainSlow) {
    fifo.wrreq := True // real-time
    fifo.data := dataIns.reduce(_ ## _)
  }

  /** --------
   * P2S logic in serial domain
   -------- */
  new ClockingArea(domainFast) {
    val reqCounter = CounterFreeRun(factor) // controlled by a counter
    // get the data from domainSlow
    fifo.rdreq := reqCounter.willOverflow
    val parallel = RegNextWhen(fifo.q, reqCounter.willOverflow)
    // distribute data to different cycles
    val qs = parallel.subdivideIn(width bits).reverse
    switch(reqCounter.value) {
      qs.zipWithIndex.foreach { case (q, i) => is(i)(dataOut := q.d(i)) }
    }
  }
}



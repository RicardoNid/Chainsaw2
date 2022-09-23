package org.datenlord
package ip.ftn

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class ThroughputGen() {

  val target = 50e9 // throughput target
  val signalProFreq = 300e6

  val fftThroughput = (target / signalProFreq)




}

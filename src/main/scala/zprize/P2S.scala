
package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class P2S(p: Int, s: Int, bitWidth: Int)
  extends ChainsawGenerator {
  require(p % s == 0)

  override def name = s"P2S_s${s}_p$p"

  override val impl = (dataIn: Seq[Any]) => dataIn

  override var inputTypes = Seq.fill(p)(UIntInfo(bitWidth))
  override var outputTypes = Seq.fill(s)(UIntInfo(bitWidth))

  override var inputFormat = MatrixFormatAddBubble(p, 1, p / s - 1)
  override var outputFormat = MatrixFormat(s, p / s)
  override var latency = 1

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val counter = CounterFreeRun(p / s)
    when(lastIn)(counter.clear())

    // write
    val segments = dataIn.grouped(s).toSeq.map(_.asBits()) // merge elements before mux
    val buffers = segments.tail.map(segment =>
      RegNextWhen(segment, counter.value === U(0, log2Up(p / s) bits)))

    // read
    val ret = Bits(outputWidths.sum bits)
    switch(counter.value) {
      is(U(0))(ret := segments.head)
      (1 until p / s).foreach(i => is(U(i))(ret := buffers(i - 1)))
      if (!isPow2(p / s)) default(ret.assignDontCare())
    }

    dataOut := ret.d(1).subdivideIn(bitWidth bits) // split elements
  }
}


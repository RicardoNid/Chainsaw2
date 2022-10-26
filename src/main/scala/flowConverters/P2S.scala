package org.datenlord
package flowConverters

import org.datenlord.{ChainsawGenerator, ChainsawModule, UIntInfo}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class P2S(p: Int, s: Int, bitWidth: Int, blockLength: Int = 1)
  extends ChainsawGenerator {
  require(p % s == 0)

  override def name = s"P2S_s${s}_p${p}_bl${blockLength}_bw${bitWidth}"

  override def impl(dataIn: Seq[Any]) = dataIn

  override var inputTypes = Seq.fill(p)(UIntInfo(bitWidth))
  override var outputTypes = Seq.fill(s)(UIntInfo(bitWidth))

  override var inputFormat = MatrixFormatAddBubble(p, blockLength, (p / s - 1) * blockLength)
  override var outputFormat = MatrixFormat(s, p / s * blockLength)
  override var latency = if(blockLength > 1) p / s + 1 else 1

  override def implH: ChainsawModule = new ChainsawModule(this) {

    if(blockLength > 1){ // RAM-based
      val writeCounter = CounterFreeRun(blockLength * p / s)
      when(lastIn)(writeCounter.clear())
      val selectCounter = CounterFreeRun(p / s)
      when(lastIn)(selectCounter.clear())
      val readCounter = Counter(blockLength, inc = selectCounter.willOverflow)
      when(lastIn.validAfter(p / s))(readCounter.clear())

      val segments = dataIn.grouped(s).toSeq.map(Vec(_)) // merge elements before mux
      val rams = segments.map(segment => Mem(HardType(segment), blockLength))

      // write
      val we = writeCounter < U(blockLength, writeCounter.getBitsWidth bits)
      rams.zip(segments).foreach { case (ram, segment) => ram.write(writeCounter.resized, segment, we) }

      // read
      val reads = rams.map(ram => ram.readSync(readCounter))

      switch(selectCounter.value.d(1)) {
        (0 until p / s).foreach(i => is(U(i))(dataOut := reads(i)))
        if (!isPow2(p / s)) default(dataOut.assignDontCare())
      }
    }
    else {

      (0 until s).foreach { i =>
        // write
        val segments = (0 until p / s).map(j => dataIn(j * s + i))
        val buffers = segments.tail.map(segment =>
          RegNextWhen(segment, localCounter.value === U(0, log2Up(p / s) bits)))
        // read
        val ret = Bits(bitWidth bits)
        switch(localCounter.value) {
          is(U(0))(ret := segments.head)
          (1 until p / s).foreach(i => is(U(i))(ret := buffers(i - 1)))
          if (!isPow2(p / s)) default(ret.assignDontCare())
        }
        dataOut(i) := ret.d(1)
      }
    }

  }
}

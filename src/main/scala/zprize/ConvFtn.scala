package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._
import scala.language.postfixOps // as JGraphT is based on Java

object ConvFtn extends ChainsawGenerator {
  override def name = "convFtn"

  override val impl = (dataIn: Seq[Any]) => ??? // not necessary as we use data from simulink

  override var inputFormat = rawFrameFormat
  override var outputFormat = codedFrameFormat
  override var inputTypes = Seq.fill(N1/2)(UIntInfo(1))
  override var outputTypes = Seq.fill(N2)(UIntInfo(1))

  override var latency = 1

  val string171 = "1111001"
  val string133 = "1011011"

  def convenc(raw: Bits): Seq[Bool] = { // convolutional encoder
    // as delay line can be flushed during preamble gap, not explicit clear required
    val delayLine = History(raw, 7, init = B(0, 1 bits)).map(_.asBool)
    val ret0 = delayLine.zip(string171).filter(_._2 == '1').map(_._1).xorR
    val ret1 = delayLine.zip(string133).filter(_._2 == '1').map(_._1).xorR
    Seq(ret0, ret1)
  }

  override def implH = new ChainsawModule(this) {
    (0 until N1 / 2).foreach { i =>
      val source = Mux(validIn, dataIn(i), B(0, 1 bits))
      val ret = convenc(source)
      dataOut(i) := ret(0).asBits.d(1)
      dataOut(i + N1 / 2) := ret(1).asBits.d(1)
    }
  }

}

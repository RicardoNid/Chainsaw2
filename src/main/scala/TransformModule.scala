package org.datenlord

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

abstract class TransformModule[TIn <: Data, TOut <: Data] extends Component {

  val config: TransformConfig
  val dataIn: Flow[Fragment[Vec[TIn]]]
  val dataOut: Flow[Fragment[Vec[TOut]]]

  def autoValid(): Unit = dataOut.valid := dataIn.valid.validAfter(config.latency)

  def autoLast(): Unit = dataOut.last := dataIn.last.validAfter(config.latency)

  def skipControl() = {
    dataIn.valid.assignDontCare()
    dataIn.last.assignDontCare()
    dataIn.valid.allowPruning()
    dataIn.last.allowPruning()
  }

  def autoInputCounter() = {
    val counter = CounterFreeRun(config.inputFlow.period)
    // TODO: implement "valid free" mode as it is useless for control
    when(dataIn.last)(counter.clear())
    counter
  }

  def getWrapper() = {
    val inWidth = dataIn.fragment.length
    val inBitWidth = dataIn.fragment.head.getBitsWidth
    val outWidth = dataOut.fragment.length
    val outBitWidth = dataOut.fragment.head.getBitsWidth
    val dataInBits = Flow Fragment Vec(Bits(inBitWidth bits), inWidth)
    val dataOutBits = Flow Fragment Vec(Bits(outBitWidth bits), outWidth)
    dataIn.fragment.assignFromBits(dataInBits.fragment.asBits)
    dataIn.valid := dataInBits.valid
    dataIn.last := dataInBits.last
    dataOutBits.fragment.assignFromBits(dataOut.fragment.asBits)
    dataOutBits.valid := dataOut.valid
    dataOutBits.last := dataOut.last
    (dataInBits, dataOutBits)
  }

}

// TODO: make generated code shorter
object TransformBitsWrapper {
  def apply[TIn <: Data, TOut <: Data](original: => TransformModule[TIn, TOut]) = {
    logger.info("wrapper invoked")
    new TransformModule[Bits, Bits] {
      val core = original
      override val config = core.config
      val inWidth = core.dataIn.fragment.head.getBitsWidth
      val outWidth = core.dataOut.fragment.head.getBitsWidth

      import config._

      override val dataIn = slave Flow Fragment(Vec(Bits(inWidth bits), inputWidth))
      override val dataOut = master Flow Fragment(Vec(Bits(outWidth bits), outputWidth))

      core.dataIn.fragment.assignFromBits(dataIn.fragment.asBits)
      core.dataIn.valid := dataIn.valid
      core.dataIn.last := dataIn.last
      dataOut.fragment.assignFromBits(core.dataOut.fragment.asBits)
      dataOut.valid := core.dataOut.valid
      dataOut.last := core.dataOut.last
    }
  }
}

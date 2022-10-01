package org.datenlord
package arithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

// prefix sum network
case class PrefixSumNetworkConfig(bitWidth: Int, portWidth: Int,
                                  operator: (BigInt, BigInt) => BigInt,
                                  operatorHard: (Bits, Bits) => Bits
                                  )
  extends TransformDfg {
  override val name = "prefixSumNetwork"
  override val opType = Custom
  override val widthsIn = Seq.fill(portWidth)(bitWidth)
  override val widthsOut = Seq.fill(portWidth)(bitWidth)

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    data.tail.scan(data.head)(operator)
  }

  override val size = (portWidth, portWidth)

  override def latency = ???

  override def implH = PrefixSumNetwork(this)
}

// TODO: implement this by Brent-Kung and ...
case class PrefixSumNetwork(config: PrefixSumNetworkConfig)
  extends TransformModule[Bits, Bits] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(Bits(bitWidth bits), portWidth))
  override val dataOut = master Flow Fragment(Vec(Bits(bitWidth bits), portWidth))


}

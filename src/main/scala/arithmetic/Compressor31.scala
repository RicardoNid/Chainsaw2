package org.datenlord
package arithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class Compressor31() extends Component {

  val dataIn = in Vec(UInt(7 bits), 3)
  val dataOut = out UInt (9 bits)

  val fa = (a: UInt, b: UInt, c: UInt) => {
    val sum = a ^ b ^ c
    val carry = (a & b) | (b & c) | (c & a)
    (carry, sum)
  }

  val Seq(a, b, c) = dataIn.d(1)

  val twoLines = a.asBools.zip(b.asBools).zip(c.asBools).map { case ((a, b), c) =>
    fa(a.asUInt, b.asUInt, c.asUInt)
  }

  val d = twoLines.map(_._1)
  val e = twoLines.map(_._1)

  val ret = (d.asBits.takeLow(7).asUInt << 1) +^ e.asBits.asUInt
  dataOut := ret.d(1)
}

object Compressor31 {
  def main(args: Array[String]): Unit = {
    VivadoSynth(Compressor31())
  }
}

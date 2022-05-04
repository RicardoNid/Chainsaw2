package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

case class KaratsubaForXilinx() extends Component {

  val dataIn = slave Flow Vec(UInt(34 bits), 2)
  val dataOut = master Flow UInt(68 bits)

  val Seq(x, y) = dataIn.payload

  // stage1
  val Seq(xHigh, xLow) = x.subdivideIn(2 slices).reverse.map(_.d(1))
  val Seq(yHigh, yLow) = y.subdivideIn(2 slices).reverse.map(_.d(1))

  // state2
  val aPlusB = (xHigh +^ xLow).d(1)
  val cPlusD = (yHigh +^ yLow).d(1)
  val ac = (xHigh * yHigh).d(1) // 34 bits
  val bd = (xLow * yLow).d(1) // 34 bits

  // state3
  val all = (aPlusB * cPlusD).d(1)

  // stage4
  val temp = (all -^ ac.d(1)).d(1)

  // stage5
  val adbc = (temp -^ bd.d(2)).d(1)

  // stage6
  val (high, mid, low) = (ac.d(3), adbc, bd.d(3))
  //  val ret = ((high << 34) + (mid << 17) + low).d(1)

  val (lowHigh, lowLow) = low.splitAt(17)
  val ret0 = (mid +^ lowHigh.asUInt) @@ lowLow.asUInt

  val (ret0High, ret0Low) = ret0.splitAt(34)
  val ret1 = (high + ret0High.asUInt) @@ ret0Low.asUInt

  dataOut.payload := ret1.d(1)
  dataOut.valid := dataIn.valid.validAfter(KaratsubaForXilinx.latency)
}

object KaratsubaForXilinx {

  def latency = 6

  def main(args: Array[String]): Unit = {
    VivadoImpl(KaratsubaForXilinx(), "karatsubaForXilinx")
  }
}
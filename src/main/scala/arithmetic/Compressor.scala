package org.datenlord
package arithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class Compressor(outWidth: Int) extends Component {

  val inWidth = (1 << outWidth) - 1
  require(inWidth < 64)

  val dataIn = in Bits(inWidth bits)
  val dataOut = out Bits(outWidth bits)

  val asUInt = dataIn.d(1).asUInt


}

object Compressor {
  def main(args: Array[String]): Unit = {
    (2 to 4).map(i => VivadoSynth(Compressor(i), s"compressor${(1 << i) - 1}to$i"))
  }
}

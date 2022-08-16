package org.datenlord
package arithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import device._

import scala.language.postfixOps

/** general parallel counter (4; 2) for Xilinx FPGA
 *
 * @see ''Kumm, Martin & Zipf, P.. (2014). Efficient High Speed Compression Trees on Xilinx FPGAs. ''
 * @see ''Parhami, Behrooz. “Computer arithmetic - algorithms and hardware designs.” (2010).'' 8.4 PARALLEL COUNTERS AND COMPRESSORS
 */
case class Compressor42(width: Int) extends Component {
  require(width % 8 == 0)
  logger.info(s"compressor efficiency = ${(width * 4 - width * 2 - 1).toDouble / width}")
  val cIn = in Bool()
  val w, x, y, z = in UInt (width bits)
  val sumsOut, carrysOut = out UInt (width bits)
  val cOut = out Bool()

  def lut(w: Bool, x: Bool, y: Bool, z: Bool) = {
    val core = LUT6_2(BigInt("69966996e8e8e8e8", 16))
    core.I0 := x
    core.I1 := y
    core.I2 := z
    core.I3 := w
    core.I4 := False
    core.I5 := True
    (core.O5, core.O6) // O5 is carry output, O6 is XOR output
  }
  val lutOuts = (0 until width).map(i => lut(w(i), x(i), y(i), z(i)))

  val carryChains = Seq.fill(width / 8)(CARRY8())
  val selects = lutOuts.map(_._2)
  val data = w.asBits

  carryChains.zipWithIndex.foreach { case (carryChain, i) =>
    (0 until 8).foreach { j =>
      val index = i * 8 + j
      carryChain.DI(j) := data(index)
      carryChain.S(j) := selects(index)
    }
    if (i == 0) carryChain.CI := cIn else carryChain.CI := carryChains(i - 1).CO(7)
    carryChain.CI_TOP := False
  }

  carrysOut := lutOuts.map(_._1).asBits().asUInt
  sumsOut := carryChains.reverse.map(_.O).reduce(_ @@ _)
  cOut := carryChains.last.CO(7)
}

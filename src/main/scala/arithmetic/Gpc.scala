package org.datenlord
package arithmetic

import device._

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object GPC {
  // list of a
  def apply() = Seq(Compressor1to1, Compressor4to2, Compressor3to1, Compressor6to3)
}

/** general parallel counter (4; 2) for Xilinx FPGA
 *
 * @see ''Kumm, Martin & Zipf, P.. (2014). Efficient High Speed Compression Trees on Xilinx FPGAs. ''
 * @see ''Parhami, Behrooz. “Computer arithmetic - algorithms and hardware designs.” (2010).'' 8.4 PARALLEL COUNTERS AND COMPRESSORS
 */
case class Compressor4to2Hard(width: Int) extends Component {
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

  val carryCount = (width + 7) / 8
  val carryChains = Seq.fill(carryCount)(CARRY8())
  val selects = lutOuts.map(_._2)
  val data = w.asBits

  carryChains.zipWithIndex.foreach { case (carryChain, i) =>
    (0 until 8).foreach { j =>
      val index = i * 8 + j
      if (index < width) {
        carryChain.DI(j) := data(index)
        carryChain.S(j) := selects(index)
      }
      else {
        carryChain.DI(j) := False
        carryChain.S(j) := False
      }
    }
    if (i == 0) carryChain.CI := cIn else carryChain.CI := carryChains(i - 1).CO(7)
    carryChain.CI_TOP := False
  }

  carrysOut := lutOuts.map(_._1).asBits().asUInt
  sumsOut := carryChains.reverse.map(_.O).reduce(_ @@ _).takeLow(width).asUInt
  cOut := carryChains.last.CO((width + 7) % 8)
}

object Compressor4to2 extends Compressor[Bool] {
  override val isFixed = false

  override val widthLimit = 32

  override def inputFormat(width: Int) = Seq.fill(width)(4)

  override def outputFormat(width: Int) = 1 +: Seq.fill(width)(2)

  override def cost(width: Int): Int = width

  override def impl(bitsIn: BitHeap[Bool], width: Int) = {
    val Seq(w, x, y, z) = bitsIn.bitHeap.map(_.padTo(4, False)).transpose.map(_.asBits().asUInt)
    val core = Compressor4to2Hard(width)
    core.cIn := False
    core.w := w
    core.x := x
    core.y := y
    core.z := z
    val bitHeap = ArrayBuffer.fill(width + 1)(ArrayBuffer[Bool]())
    bitHeap.last += core.cOut
    core.sumsOut.asBools.zip(bitHeap).foreach { case (bit, column) => column += bit }
    core.carrysOut.asBools.zip(bitHeap.tail).foreach { case (bit, column) => column += bit }
    BitHeap(bitHeap, bitsIn.weightLow)
  }
}

/** compression by ternary adder
 *
 */
object Compressor3to1 extends Compressor[Bool] {

  override val isFixed = false

  override val widthLimit = 16

  override def inputFormat(width: Int) = Seq.fill(width)(3)

  override def outputFormat(width: Int) = Seq.fill(width + 2)(1)

  override def cost(width: Int): Int = width

  override def impl(bitsIn: BitHeap[Bool], width: Int) = {
    val dataIns = bitsIn.bitHeap.map(_.padTo(4, False)).transpose.map(_.asBits().asUInt)
    val op = TernaryAdderConfig(width, pipelined = 0).implH.asNode
    val ret = op(dataIns)
    val bitHeap = ArrayBuffer.fill(width + 2)(ArrayBuffer[Bool]())
    ret.asBits().asBools.zip(bitHeap).foreach { case (bit, column) => column += bit }
    BitHeap(bitHeap, bitsIn.weightLow)
  }
}

object Compressor1to1 extends Compressor[Bool] {

  override val isFixed = false

  override val widthLimit = Int.MaxValue

  override def inputFormat(width: Int) = Seq.fill(width)(1)

  override def outputFormat(width: Int) = Seq.fill(width)(1)

  override def cost(width: Int): Int = 0

  override def impl(bitsIn: BitHeap[Bool], width: Int): BitHeap[Bool] = bitsIn
}

case class Compressor6to3Hard() extends Component {

  val dataIn = in Bits (6 bits)
  val dataOut = out Bits (3 bits)

  switch(dataIn) {
    (0 until 64).foreach { i =>
      is(B(i, 6 bits))(dataOut := B(i.toBinaryString.map(_.asDigit).sum, 3 bits))
    }
  }
}

object Compressor6to3 extends Compressor[Bool] {

  override val isFixed = true

  override val widthLimit = 1

  override def inputFormat(width: Int) = Seq(6)

  override def outputFormat(width: Int) = Seq.fill(3)(1)

  override def cost(width: Int): Int = 3

  override def impl(bitsIn: BitHeap[Bool], width: Int): BitHeap[Bool] = {
    val dataIns = bitsIn.bitHeap.head.padTo(6, False).asBits()
    val core = Compressor6to3Hard()
    core.dataIn := dataIns
    val ret = core.dataOut
    val bitHeap = ArrayBuffer.fill(3)(ArrayBuffer[Bool]())
    ret.asBools.zip(bitHeap).foreach { case (bit, column) => column += bit }
    BitHeap(bitHeap, bitsIn.weightLow)
  }
}